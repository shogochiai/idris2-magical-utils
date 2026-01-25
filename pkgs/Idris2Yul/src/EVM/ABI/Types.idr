||| EVM ABI Type Definitions
|||
||| Provides type-safe ABI definitions for EVM contract functions.
||| Use these types to define function signatures that can be
||| automatically converted to Ethereum ABI JSON format.
|||
||| Example:
|||   proposeABI : Function
|||   proposeABI = MkFunction "propose" 0x12345678
|||     [Bytes32, Address] [Uint256] Nonpayable
|||
module EVM.ABI.Types

import Data.List

-- =============================================================================
-- ABI Type Definitions
-- =============================================================================

||| Solidity/EVM ABI types
||| These correspond to the types supported by the Ethereum ABI specification
public export
data ABIType
  = ABI_Uint256           -- uint256
  | ABI_Uint128           -- uint128
  | ABI_Uint64            -- uint64
  | ABI_Uint32            -- uint32
  | ABI_Uint16            -- uint16
  | ABI_Uint8             -- uint8
  | ABI_Int256            -- int256
  | ABI_Int128            -- int128
  | ABI_Int64             -- int64
  | ABI_Int32             -- int32
  | ABI_Int16             -- int16
  | ABI_Int8              -- int8
  | ABI_Address           -- address (20 bytes)
  | ABI_Bool              -- bool
  | ABI_Bytes32           -- bytes32
  | ABI_Bytes20           -- bytes20 (address-sized)
  | ABI_Bytes4            -- bytes4 (function selector)
  | ABI_Bytes             -- bytes (dynamic)
  | ABI_String            -- string (dynamic)
  | ABI_Array ABIType     -- T[] (dynamic array)
  | ABI_FixedArray Nat ABIType  -- T[N] (fixed-size array)
  | ABI_Tuple (List ABIType)    -- tuple (for struct encoding)

||| Function mutability (state mutability in Solidity)
public export
data Mutability
  = Pure        -- Does not read or write state
  | View        -- Reads state but does not write
  | Nonpayable  -- May write state, does not accept ETH
  | Payable     -- May write state and accepts ETH

||| Named parameter for ABI (name is optional but useful for documentation)
public export
record Param where
  constructor MkParam
  paramName : String
  paramType : ABIType
  indexed : Bool  -- For event parameters

||| Simple param constructor (non-indexed, named)
export
param : String -> ABIType -> Param
param name ty = MkParam name ty False

||| Anonymous param (for when name doesn't matter)
export
anon : ABIType -> Param
anon ty = MkParam "" ty False

||| Indexed event param
export
indexed : String -> ABIType -> Param
indexed name ty = MkParam name ty True

-- =============================================================================
-- Function Definition
-- =============================================================================

||| Contract function ABI definition
public export
record Function where
  constructor MkFunction
  funcName : String
  selector : Integer       -- 4-byte selector (0x12345678)
  inputs : List Param
  outputs : List Param
  mutability : Mutability

||| Simplified function constructor with just types (no param names)
export
mkFunc : String -> Integer -> List ABIType -> List ABIType -> Mutability -> Function
mkFunc name sel ins outs mut = MkFunction name sel
  (map anon ins)
  (map anon outs)
  mut

-- =============================================================================
-- Event Definition
-- =============================================================================

||| Contract event ABI definition
public export
record Event where
  constructor MkEvent
  eventName : String
  topic0 : Integer         -- keccak256(signature)
  params : List Param
  anonymous : Bool

||| Simple event constructor
export
mkEvent : String -> Integer -> List Param -> Event
mkEvent name topic ps = MkEvent name topic ps False

-- =============================================================================
-- Error Definition
-- =============================================================================

||| Custom error ABI definition (Solidity 0.8.4+)
public export
record Error where
  constructor MkError
  errorName : String
  selector : Integer       -- 4-byte selector
  params : List Param

-- =============================================================================
-- Contract ABI
-- =============================================================================

||| Complete contract ABI
public export
record ContractABI where
  constructor MkContractABI
  contractName : String
  functions : List Function
  events : List Event
  errors : List Error

||| Empty contract ABI
export
emptyABI : String -> ContractABI
emptyABI name = MkContractABI name [] [] []

||| Add function to ABI
export
addFunction : Function -> ContractABI -> ContractABI
addFunction f abi = { functions $= (f ::) } abi

||| Add event to ABI
export
addEvent : Event -> ContractABI -> ContractABI
addEvent e abi = { events $= (e ::) } abi

||| Add error to ABI
export
addError : Error -> ContractABI -> ContractABI
addError e abi = { errors $= (e ::) } abi

-- =============================================================================
-- Type to String (for ABI encoding)
-- =============================================================================

||| Convert ABIType to Solidity type string
export
abiTypeToString : ABIType -> String
abiTypeToString ABI_Uint256 = "uint256"
abiTypeToString ABI_Uint128 = "uint128"
abiTypeToString ABI_Uint64 = "uint64"
abiTypeToString ABI_Uint32 = "uint32"
abiTypeToString ABI_Uint16 = "uint16"
abiTypeToString ABI_Uint8 = "uint8"
abiTypeToString ABI_Int256 = "int256"
abiTypeToString ABI_Int128 = "int128"
abiTypeToString ABI_Int64 = "int64"
abiTypeToString ABI_Int32 = "int32"
abiTypeToString ABI_Int16 = "int16"
abiTypeToString ABI_Int8 = "int8"
abiTypeToString ABI_Address = "address"
abiTypeToString ABI_Bool = "bool"
abiTypeToString ABI_Bytes32 = "bytes32"
abiTypeToString ABI_Bytes20 = "bytes20"
abiTypeToString ABI_Bytes4 = "bytes4"
abiTypeToString ABI_Bytes = "bytes"
abiTypeToString ABI_String = "string"
abiTypeToString (ABI_Array t) = abiTypeToString t ++ "[]"
abiTypeToString (ABI_FixedArray n t) = abiTypeToString t ++ "[" ++ show n ++ "]"
abiTypeToString (ABI_Tuple ts) = "(" ++ joinWith "," (map abiTypeToString ts) ++ ")"
  where
    joinWith : String -> List String -> String
    joinWith sep [] = ""
    joinWith sep [x] = x
    joinWith sep (x :: xs) = x ++ sep ++ joinWith sep xs

||| Convert Mutability to string
export
mutabilityToString : Mutability -> String
mutabilityToString Pure = "pure"
mutabilityToString View = "view"
mutabilityToString Nonpayable = "nonpayable"
mutabilityToString Payable = "payable"

-- =============================================================================
-- Selector Calculation Helper
-- =============================================================================

||| Generate function signature string for selector calculation
||| e.g., "transfer(address,uint256)"
export
functionSignature : Function -> String
functionSignature f =
  f.funcName ++ "(" ++ joinWith "," (map (abiTypeToString . paramType) f.inputs) ++ ")"
  where
    joinWith : String -> List String -> String
    joinWith sep [] = ""
    joinWith sep [x] = x
    joinWith sep (x :: xs) = x ++ sep ++ joinWith sep xs

||| Generate event signature string for topic0 calculation
||| e.g., "Transfer(address,address,uint256)"
export
eventSignature : Event -> String
eventSignature e =
  e.eventName ++ "(" ++ joinWith "," (map (abiTypeToString . paramType) e.params) ++ ")"
  where
    joinWith : String -> List String -> String
    joinWith sep [] = ""
    joinWith sep [x] = x
    joinWith sep (x :: xs) = x ++ sep ++ joinWith sep xs
