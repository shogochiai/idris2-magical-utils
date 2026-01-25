||| ICP Management Canister - Threshold ECDSA
|||
||| This module provides typed access to the ICP Management Canister's
||| threshold ECDSA (t-ECDSA) signing functionality.
|||
||| t-ECDSA enables canisters to sign messages using ECDSA keys that are
||| distributed across the IC subnet nodes, without any single node
||| having access to the full private key.
|||
||| Reference: https://internetcomputer.org/docs/current/references/ic-interface-spec#ic-ecdsa_public_key
module ICP.Management.TECDSA

import ICP.IC0
import ICP.Candid.Types
import Data.List

%default total

--------------------------------------------------------------------------------
-- Key Derivation
--------------------------------------------------------------------------------

||| ECDSA curve identifier
public export
data EcdsaCurve = Secp256k1

public export
Show EcdsaCurve where
  show Secp256k1 = "secp256k1"

||| Key derivation path component
public export
DerivationPath : Type
DerivationPath = List (List Bits8)

||| ECDSA key identifier
public export
record EcdsaKeyId where
  constructor MkEcdsaKeyId
  curve : EcdsaCurve
  name  : String  -- Key name (e.g., "dfx_test_key", "key_1")

||| Default test key (for local development)
public export
testKey : EcdsaKeyId
testKey = MkEcdsaKeyId Secp256k1 "dfx_test_key"

||| Production key
public export
productionKey : EcdsaKeyId
productionKey = MkEcdsaKeyId Secp256k1 "key_1"

--------------------------------------------------------------------------------
-- Public Key Request/Response
--------------------------------------------------------------------------------

||| Request for ECDSA public key
public export
record EcdsaPublicKeyRequest where
  constructor MkEcdsaPublicKeyRequest
  canisterId     : Maybe Principal  -- None = caller's canister
  derivationPath : DerivationPath
  keyId          : EcdsaKeyId

||| Response containing ECDSA public key
public export
record EcdsaPublicKeyResponse where
  constructor MkEcdsaPublicKeyResponse
  publicKey : List Bits8  -- SEC1 encoded public key (33 bytes compressed, 65 uncompressed)
  chainCode : List Bits8  -- 32 bytes

--------------------------------------------------------------------------------
-- Sign Request/Response
--------------------------------------------------------------------------------

||| Request to sign a message hash
public export
record SignWithEcdsaRequest where
  constructor MkSignWithEcdsaRequest
  messageHash    : List Bits8  -- 32 bytes (SHA-256 hash of message)
  derivationPath : DerivationPath
  keyId          : EcdsaKeyId

||| Response containing ECDSA signature
public export
record SignWithEcdsaResponse where
  constructor MkSignWithEcdsaResponse
  signature : List Bits8  -- DER encoded signature (typically 70-72 bytes)

--------------------------------------------------------------------------------
-- Cycles Cost
--------------------------------------------------------------------------------

||| Cost for public key request (in cycles)
public export
ecdsaPublicKeyCost : Nat
ecdsaPublicKeyCost = 10_000_000_000  -- 10B cycles

||| Cost for signing request (in cycles)
public export
ecdsaSignCost : Nat
ecdsaSignCost = 10_000_000_000  -- 10B cycles

--------------------------------------------------------------------------------
-- Candid Encoding
--------------------------------------------------------------------------------

public export
Candidable EcdsaCurve where
  candidType = CtVariant
    [ MkFieldType (hashFieldName "secp256k1") (Just "secp256k1") CtNull
    ]
  toCandid Secp256k1 = CVariant (hashFieldName "secp256k1") CNull
  fromCandid (CVariant h CNull) =
    if h == hashFieldName "secp256k1" then Just Secp256k1 else Nothing
  fromCandid _ = Nothing

public export
Candidable EcdsaKeyId where
  candidType = CtRecord
    [ MkFieldType (hashFieldName "curve") (Just "curve") (candidType {a=EcdsaCurve})
    , MkFieldType (hashFieldName "name") (Just "name") CtText
    ]
  toCandid (MkEcdsaKeyId curve name) = CRecord
    [ (hashFieldName "curve", toCandid curve)
    , (hashFieldName "name", CText name)
    ]
  fromCandid (CRecord fields) = do
    curve <- lookup (hashFieldName "curve") fields >>= fromCandid
    name <- lookup (hashFieldName "name") fields >>= fromCandid
    Just (MkEcdsaKeyId curve name)
  fromCandid _ = Nothing

--------------------------------------------------------------------------------
-- t-ECDSA Operations
--------------------------------------------------------------------------------

||| Result of t-ECDSA operations
public export
data TecdsaResult a
  = TecdsaSuccess a
  | TecdsaError String

||| Get ECDSA public key for this canister
||| Derivation path allows deriving child keys
export
ecdsaPublicKey : EcdsaKeyId -> DerivationPath -> IO (TecdsaResult EcdsaPublicKeyResponse)
ecdsaPublicKey keyId path = do
  -- In real implementation:
  -- 1. Call management canister method "ecdsa_public_key"
  -- 2. Attach ecdsaPublicKeyCost cycles
  -- 3. Decode response
  pure (TecdsaError "Not implemented - requires ICP runtime")

||| Sign a message hash with t-ECDSA
||| IMPORTANT: messageHash must be exactly 32 bytes (SHA-256)
export
signWithEcdsa : EcdsaKeyId -> DerivationPath -> List Bits8 -> IO (TecdsaResult SignWithEcdsaResponse)
signWithEcdsa keyId path messageHash = do
  if length messageHash /= 32
    then pure (TecdsaError "Message hash must be exactly 32 bytes")
    else do
      -- In real implementation:
      -- 1. Call management canister method "sign_with_ecdsa"
      -- 2. Attach ecdsaSignCost cycles
      -- 3. Decode response
      pure (TecdsaError "Not implemented - requires ICP runtime")

--------------------------------------------------------------------------------
-- Ethereum-specific Helpers
--------------------------------------------------------------------------------

||| Ethereum derivation path for this canister
||| Creates a unique key based on canister ID
public export
ethereumDerivationPath : Principal -> DerivationPath
ethereumDerivationPath (MkPrincipal bytes) = [bytes]

||| Convert signature to Ethereum format (r, s, v)
||| Returns (r: 32 bytes, s: 32 bytes, v: 1 byte)
public export
toEthereumSignature : List Bits8 -> Nat -> Maybe (List Bits8, List Bits8, Bits8)
toEthereumSignature derSig chainId =
  -- DER signature parsing would go here
  -- For now, placeholder
  Nothing

||| Compute Ethereum address from public key
||| Takes uncompressed SEC1 public key (65 bytes), returns 20-byte address
public export
publicKeyToEthAddress : List Bits8 -> Maybe (List Bits8)
publicKeyToEthAddress pubKey =
  case pubKey of
    [] => Nothing
    (b :: _) =>
      if length pubKey /= 65 || b /= 0x04
        then Nothing
        else
          -- In real implementation:
          -- 1. Take last 64 bytes (x, y coordinates)
          -- 2. Keccak256 hash
          -- 3. Take last 20 bytes
          Just []  -- Placeholder

--------------------------------------------------------------------------------
-- Derivation Path Helpers
--------------------------------------------------------------------------------

||| Empty derivation path (use canister's root key)
public export
emptyPath : DerivationPath
emptyPath = []

||| Create derivation path from string (hashed)
public export
pathFromString : String -> DerivationPath
pathFromString s = [map (cast . ord) (unpack s)]

||| Append to derivation path
public export
appendPath : DerivationPath -> List Bits8 -> DerivationPath
appendPath path segment = path ++ [segment]
