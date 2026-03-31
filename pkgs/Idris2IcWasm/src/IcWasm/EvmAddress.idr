||| Type-Safe EVM Address Derivation
|||
||| Prevents the "compressed key → wrong address" bug at the type level.
|||
||| ICP's ecdsa_public_key returns a COMPRESSED SEC1 key (33 bytes).
||| EVM address derivation requires UNCOMPRESSED key (65 bytes):
|||   address = keccak256(uncompressed[1:64])[-20:]
|||
||| If you apply keccak256 to compressed[1:32] you get a WRONG address
||| that doesn't correspond to any private key. ETH sent there is LOST.
|||
||| This module makes that mistake a TYPE ERROR:
|||   deriveAddress : UncompressedKey -> EvmAddress   -- OK
|||   deriveAddress : CompressedKey -> ...             -- TYPE ERROR
|||
||| Discovered 2026-03-31: 0.01 ETH lost to keccak256(compressed_x_only).
module IcWasm.EvmAddress

import Data.Vect
import Data.Bits

%default total

-- =============================================================================
-- Key Types (phantom-tagged)
-- =============================================================================

||| SEC1 key format tag
public export
data KeyFormat = Compressed | Uncompressed

||| SEC1 public key, tagged with its format.
||| Compressed: 33 bytes (02/03 prefix + 32 bytes x)
||| Uncompressed: 65 bytes (04 prefix + 32 bytes x + 32 bytes y)
public export
data Sec1Key : KeyFormat -> Type where
  MkCompressed   : Bits8 -> Vect 32 Bits8 -> Sec1Key Compressed
  MkUncompressed : Vect 32 Bits8 -> Vect 32 Bits8 -> Sec1Key Uncompressed

-- =============================================================================
-- EVM Address (20 bytes, derived from uncompressed key only)
-- =============================================================================

||| EVM address (20 bytes), derived from keccak256(uncompressed_pubkey[1:64])
public export
data EvmAddress = MkEvmAddress (Vect 20 Bits8)

||| Display as 0x-prefixed hex
export
Show EvmAddress where
  show (MkEvmAddress bytes) = "0x" ++ concatMap showByte (toList bytes)
    where
      hexChar : Bits8 -> Char
      hexChar n = if n < 10 then chr (cast n + cast '0')
                  else chr (cast n - 10 + cast 'a')
      showByte : Bits8 -> String
      showByte b = pack [hexChar (b `shiftR` 4), hexChar (b .&. 0x0F)]

-- =============================================================================
-- Address Derivation (Uncompressed ONLY)
-- =============================================================================

||| Derive EVM address from an UNCOMPRESSED public key.
|||
||| This is the ONLY way to create an EvmAddress.
||| Compressed keys CANNOT be passed here — it's a type error.
|||
||| @key Uncompressed SEC1 public key (must be decompressed first)
||| @keccak256 Hash function (injected — C FFI or pure implementation)
export
deriveAddress : Sec1Key Uncompressed
             -> (keccak256 : Vect 64 Bits8 -> Vect 32 Bits8)
             -> EvmAddress
deriveAddress (MkUncompressed x y) keccak256Fn =
  let pubBytes = x ++ y  -- 64 bytes (no 04 prefix)
      hash = keccak256Fn pubBytes  -- 32 bytes
      -- Take last 20 bytes
      addr = drop 12 hash
  in MkEvmAddress addr

-- =============================================================================
-- Point Decompression (Compressed → Uncompressed)
-- =============================================================================

||| Decompress a compressed SEC1 key to uncompressed.
||| Requires secp256k1 field arithmetic (modular sqrt).
|||
||| This is an IO operation because:
||| 1. On-canister: uses C FFI for big integer math
||| 2. Off-canister (CLI): uses external library
|||
||| @key Compressed SEC1 public key
||| Returns Either error or uncompressed key
export
decompress : Sec1Key Compressed -> IO (Either String (Sec1Key Uncompressed))
decompress (MkCompressed pfx x) = do
  -- TODO: secp256k1 point decompression
  -- y^2 = x^3 + 7 (mod p)
  -- y = sqrt(x^3 + 7) mod p
  -- if prefix == 0x02: y is even; if 0x03: y is odd
  pure (Left "decompress: not yet implemented (use CLI-side web3.py)")

-- =============================================================================
-- Parsing from raw bytes
-- =============================================================================

takeList : Nat -> List a -> List a
takeList Z _ = []
takeList _ [] = []
takeList (S k) (x :: xs) = x :: takeList k xs

dropList : Nat -> List a -> List a
dropList Z xs = xs
dropList _ [] = []
dropList (S k) (_ :: xs) = dropList k xs

listToVect : (n : Nat) -> List Bits8 -> Maybe (Vect n Bits8)
listToVect Z _ = Just []
listToVect (S k) [] = Nothing
listToVect (S k) (b :: bs) = map (b ::) (listToVect k bs)

||| Result of parsing SEC1 key — either compressed or uncompressed
public export
data ParsedKey = ParsedCompressed (Sec1Key Compressed) | ParsedUncompressed (Sec1Key Uncompressed)

||| Parse raw bytes from ICP ecdsa_public_key response
export
parseSec1 : List Bits8 -> Either String ParsedKey
parseSec1 [] = Left "empty key"
parseSec1 (p :: rest) =
  if p == 4
    then let xBytes = takeList 32 rest
             yBytes = takeList 32 (dropList 32 rest)
         in case listToVect 32 xBytes of
              Just x => case listToVect 32 yBytes of
                Just y => Right (ParsedUncompressed (MkUncompressed x y))
                Nothing => Left "uncompressed key: need y bytes"
              Nothing => Left "uncompressed key: need x bytes"
    else if p == 2 || p == 3
    then case listToVect 32 (takeList 32 rest) of
           Just x => Right (ParsedCompressed (MkCompressed p x))
           Nothing => Left "compressed key: need 32 bytes after prefix"
    else Left "unknown SEC1 prefix"

-- =============================================================================
-- Type-Level Documentation
-- =============================================================================
--
-- The bug this prevents:
--   keccak256(compressed_key[1:32]) → WRONG address (not a real private key)
--   ETH sent to this address is IRRECOVERABLE.
--
-- With this module:
--   parseSec1 icpResponse → Right (Compressed ** key)
--   deriveAddress key keccak  → TYPE ERROR: expected Uncompressed, got Compressed
--   decompress key >>= \uncompressed => deriveAddress uncompressed keccak  → OK
--
-- The forced detour through `decompress` ensures correct address derivation.
