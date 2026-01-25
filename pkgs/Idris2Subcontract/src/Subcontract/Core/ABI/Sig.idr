||| Subcontract ABI: Type-Safe Function Signatures and Selectors
|||
||| Provides compile-time binding between function signatures and selectors.
||| Selector values are still provided as constants (due to KECCAK256 runtime
||| constraints), but the type system ensures signature-selector pairs remain
||| consistent.
|||
||| Key insight: We cannot compute keccak256 at compile time in the current
||| idris2-evm interpreter, so we use phantom types to bind signatures to
||| known selector constants, and verify consistency via tests.
|||
module Subcontract.Core.ABI.Sig

import public Data.List

-- =============================================================================
-- ABI Parameter Types (Static only for now)
-- =============================================================================

||| Static ABI types that occupy exactly 32 bytes
public export
data ABIStaticType
  = TUint256
  | TBytes32
  | TAddress
  | TBool

||| Convert to canonical Solidity type string
export
abiTypeStr : ABIStaticType -> String
abiTypeStr TUint256 = "uint256"
abiTypeStr TBytes32 = "bytes32"
abiTypeStr TAddress = "address"
abiTypeStr TBool = "bool"

-- =============================================================================
-- Function Signature
-- =============================================================================

||| A function signature with name and typed parameters
||| This is a value-level representation that can be used at runtime
public export
record Sig where
  constructor MkSig
  name : String
  args : List ABIStaticType
  rets : List ABIStaticType

||| Generate canonical signature string: "name(type1,type2,...)"
export
sigString : Sig -> String
sigString s = s.name ++ "(" ++ joinWith "," (map abiTypeStr s.args) ++ ")"
  where
    joinWith : String -> List String -> String
    joinWith sep [] = ""
    joinWith sep [x] = x
    joinWith sep (x :: xs) = x ++ sep ++ joinWith sep xs

-- =============================================================================
-- Selector (Phantom-Typed)
-- =============================================================================

||| A selector bound to a specific signature via phantom type
||| The `sig` parameter ensures this selector is only used with matching signature
public export
data Sel : (sig : Sig) -> Type where
  MkSel : (value : Integer) -> Sel sig

||| Extract the 4-byte selector value
export
selValue : Sel sig -> Integer
selValue (MkSel v) = v

||| Show selector with its signature for debugging
export
showSel : {sig : Sig} -> Sel sig -> String
showSel {sig} (MkSel v) = sigString sig ++ " => 0x" ++ showHex8 v
  where
    hexDigit : Integer -> Char
    hexDigit x = if x < 10 then chr (ord '0' + cast x) else chr (ord 'a' + cast (x - 10))
    showHex8 : Integer -> String
    showHex8 n = pack [hexDigit ((n `div` 0x10000000) `mod` 16),
                       hexDigit ((n `div` 0x1000000) `mod` 16),
                       hexDigit ((n `div` 0x100000) `mod` 16),
                       hexDigit ((n `div` 0x10000) `mod` 16),
                       hexDigit ((n `div` 0x1000) `mod` 16),
                       hexDigit ((n `div` 0x100) `mod` 16),
                       hexDigit ((n `div` 0x10) `mod` 16),
                       hexDigit (n `mod` 16)]
