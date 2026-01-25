||| Ethereum ABI Encoding/Decoding
||| Solidity-compatible ABI for function calls and events
module Compiler.EVM.ABI

import Compiler.EVM.YulIR
import Data.List
import Data.String

%default covering

-- =============================================================================
-- Function Selector Calculation
-- =============================================================================

||| Keccak256 hash (stub - would need actual implementation or external call)
||| In practice, selectors are computed at compile time
export
keccak256 : String -> Integer
keccak256 _ = 0  -- TODO: Implement or use external tool

||| Compute function selector from signature
||| e.g., "transfer(address,uint256)" -> 0xa9059cbb
export
functionSelector : String -> Integer
functionSelector sig = keccak256 sig `mod` (256 * 256 * 256 * 256)

-- =============================================================================
-- ABI Types
-- =============================================================================

||| ABI type representation
public export
data ABIType
  = ABIUint Nat           -- uint8, uint16, ..., uint256
  | ABIInt Nat            -- int8, int16, ..., int256
  | ABIAddress            -- address (20 bytes)
  | ABIBool               -- bool
  | ABIBytes Nat          -- bytes1, bytes2, ..., bytes32
  | ABIDynBytes           -- bytes (dynamic)
  | ABIString             -- string (dynamic)
  | ABIArray ABIType      -- T[] (dynamic array)
  | ABIFixedArray Nat ABIType  -- T[k] (fixed array)
  | ABITuple (List ABIType)    -- (T1, T2, ...)

||| Check if type is dynamic (requires offset indirection)
export
isDynamic : ABIType -> Bool
isDynamic ABIDynBytes = True
isDynamic ABIString = True
isDynamic (ABIArray _) = True
isDynamic (ABITuple ts) = any isDynamic ts
isDynamic _ = False

||| Get head size of a type (32 bytes for static, 32 for pointer to dynamic)
export
headSize : ABIType -> Nat
headSize _ = 32  -- All types use 32 bytes in head

-- =============================================================================
-- ABI Encoding
-- =============================================================================

||| Generate Yul to encode a value to memory at given offset
export
encodeValue : ABIType -> (memOffset : YulExpr) -> (value : YulExpr) -> List YulStmt
encodeValue (ABIUint _) off val = [YExprStmt $ mstore off val]
encodeValue (ABIInt _) off val = [YExprStmt $ mstore off val]
encodeValue ABIAddress off val =
  [YExprStmt $ mstore off (yulCall "and" [val, yulNum 0xffffffffffffffffffffffffffffffffffffffff])]
encodeValue ABIBool off val =
  [YExprStmt $ mstore off (yulCall "iszero" [yulCall "iszero" [val]])]
encodeValue (ABIBytes _) off val = [YExprStmt $ mstore off val]
encodeValue _ off val = [YExprStmt $ mstore off val]  -- TODO: dynamic types

-- =============================================================================
-- ABI Decoding
-- =============================================================================

||| Generate Yul to decode a value from calldata at given offset
export
decodeCalldata : ABIType -> (cdOffset : Integer) -> YulExpr
decodeCalldata (ABIUint _) off = calldataload (yulNum off)
decodeCalldata (ABIInt _) off = calldataload (yulNum off)
decodeCalldata ABIAddress off = yulCall "and"
  [ calldataload (yulNum off)
  , yulNum 0xffffffffffffffffffffffffffffffffffffffff
  ]
decodeCalldata ABIBool off = yulCall "iszero" [yulCall "iszero" [calldataload (yulNum off)]]
decodeCalldata (ABIBytes _) off = calldataload (yulNum off)
decodeCalldata _ off = calldataload (yulNum off)  -- TODO: dynamic types

-- =============================================================================
-- Event Encoding
-- =============================================================================

||| Generate Yul for emitting an event
||| topic0 is typically keccak256(event signature)
export
emitEvent : (topic0 : Integer) -> (indexedTopics : List YulExpr) -> (dataFields : List (ABIType, YulExpr)) -> List YulStmt
emitEvent sig indexed dataFields =
  let -- Encode non-indexed data to memory
      dataSize = 32 * cast (length dataFields)
      encodeStmts = concat $ zipWith encodeField (take (length dataFields) [0..]) dataFields
      -- Generate log call based on number of topics
      numTopics = 1 + length indexed
      logCall = case numTopics of
        1 => yulCall "log1" [yulNum 0, yulNum dataSize, yulNum sig]
        2 => case indexed of
               [t1] => yulCall "log2" [yulNum 0, yulNum dataSize, yulNum sig, t1]
               _ => yulCall "log1" [yulNum 0, yulNum dataSize, yulNum sig]
        3 => case indexed of
               [t1, t2] => yulCall "log3" [yulNum 0, yulNum dataSize, yulNum sig, t1, t2]
               _ => yulCall "log1" [yulNum 0, yulNum dataSize, yulNum sig]
        _ => case indexed of
               [t1, t2, t3] => yulCall "log4" [yulNum 0, yulNum dataSize, yulNum sig, t1, t2, t3]
               _ => yulCall "log1" [yulNum 0, yulNum dataSize, yulNum sig]
  in encodeStmts ++ [YExprStmt logCall]
  where
    encodeField : Integer -> (ABIType, YulExpr) -> List YulStmt
    encodeField idx (ty, val) = encodeValue ty (yulNum (idx * 32)) val

-- =============================================================================
-- Common Selectors (Pre-computed)
-- =============================================================================

||| ERC-20 function selectors
export
ERC20_TRANSFER : Integer
ERC20_TRANSFER = 0xa9059cbb  -- transfer(address,uint256)

export
ERC20_APPROVE : Integer
ERC20_APPROVE = 0x095ea7b3  -- approve(address,uint256)

export
ERC20_TRANSFER_FROM : Integer
ERC20_TRANSFER_FROM = 0x23b872dd  -- transferFrom(address,address,uint256)

export
ERC20_BALANCE_OF : Integer
ERC20_BALANCE_OF = 0x70a08231  -- balanceOf(address)

export
ERC20_ALLOWANCE : Integer
ERC20_ALLOWANCE = 0xdd62ed3e  -- allowance(address,address)

export
ERC20_TOTAL_SUPPLY : Integer
ERC20_TOTAL_SUPPLY = 0x18160ddd  -- totalSupply()

||| ERC-20 event signatures (topic0)
export
ERC20_TRANSFER_EVENT : Integer
ERC20_TRANSFER_EVENT = 0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef

export
ERC20_APPROVAL_EVENT : Integer
ERC20_APPROVAL_EVENT = 0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925
