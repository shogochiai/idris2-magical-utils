||| EVM Foreign Function Interface
||| Bindings for EVM-specific operations
module Compiler.EVM.Foreign

import Compiler.EVM.YulIR
import Data.List
import Data.List1
import Data.String

%default covering

-- =============================================================================
-- Foreign Function Parsing
-- =============================================================================

||| Parse EVM foreign function specification
||| Format: "evm:opcode" or "evm:opcode,arg1,arg2"
export
parseEvmForeign : String -> Maybe (String, List String)
parseEvmForeign spec =
  case break (== ':') spec of
    ("evm", rest) =>
      case strUncons rest of
        Just (':', afterColon) =>
          let parts = forget $ split (== ',') afterColon
          in case parts of
               (op :: args) => Just (op, args)
               [] => Nothing
        _ => Nothing
    _ => Nothing

-- =============================================================================
-- EVM Opcode Generation
-- =============================================================================

||| Expected argument count for each EVM opcode
opcodeArity : String -> Nat
opcodeArity "sload" = 1
opcodeArity "sstore" = 2
opcodeArity "mstore" = 2
opcodeArity "mstore8" = 2
opcodeArity "mload" = 1
opcodeArity "calldataload" = 1
opcodeArity "calldatasize" = 0
opcodeArity "calldatacopy" = 3
opcodeArity "returndatasize" = 0
opcodeArity "returndatacopy" = 3
opcodeArity "return" = 2
opcodeArity "revert" = 2
opcodeArity "stop" = 0
opcodeArity "balance" = 1
opcodeArity "selfbalance" = 0
opcodeArity "caller" = 0
opcodeArity "callvalue" = 0
opcodeArity "origin" = 0
opcodeArity "address" = 0
opcodeArity "codesize" = 0
opcodeArity "blockhash" = 1
opcodeArity "coinbase" = 0
opcodeArity "timestamp" = 0
opcodeArity "number" = 0
opcodeArity "difficulty" = 0
opcodeArity "gaslimit" = 0
opcodeArity "chainid" = 0
opcodeArity "basefee" = 0
opcodeArity "gas" = 0
opcodeArity "gasprice" = 0
opcodeArity "keccak256" = 2
opcodeArity "log0" = 2
opcodeArity "log1" = 3
opcodeArity "log2" = 4
opcodeArity "log3" = 5
opcodeArity "log4" = 6
opcodeArity "selfdestruct" = 1
opcodeArity "call" = 7
opcodeArity "delegatecall" = 6
opcodeArity "staticcall" = 6
opcodeArity "create" = 3
opcodeArity "create2" = 4
-- Compound operations
opcodeArity "returnOrRevert" = 3  -- success, offset, length
-- Bitwise operations
opcodeArity "and" = 2
opcodeArity "or" = 2
opcodeArity "xor" = 2
opcodeArity "not" = 1
opcodeArity "shl" = 2
opcodeArity "shr" = 2
opcodeArity "sar" = 2
opcodeArity _ = 0

||| Generate Yul for EVM opcode call
||| Takes expected args and ignores extra (like world token)
||| Special handling for compound operations like returnOrRevert
export
evmOpcodeToYul : String -> List YulExpr -> YulExpr
evmOpcodeToYul "returnOrRevert" args =
  -- returnOrRevert(success, offset, length)
  -- Expands to: if success { return(offset, length) } else { revert(offset, length) }
  -- Since we can't return a statement here, we use a Yul function call pattern
  -- We'll inline the logic in a way that works in expression context
  case args of
    [success, off, len] =>
      -- This is a workaround: we use Yul's ternary-like pattern with functions
      -- Actually, we need to generate statements, not an expression
      -- For now, just return the call and handle it specially in Codegen
      yulCall "returnOrRevert" [success, off, len]
    _ => yulCall "returnOrRevert" args
evmOpcodeToYul op args =
  let arity = opcodeArity op
      relevantArgs = take arity args
  in yulCall op relevantArgs

-- =============================================================================
-- High-Level Contract Primitives
-- =============================================================================

||| Generate Yul for reading function selector from calldata
export
readSelector : YulExpr
readSelector = yulCall "shr" [yulNum 224, calldataload (yulNum 0)]

||| Generate Yul for a function dispatcher entry
export
dispatchCase : (selector : Integer) -> YulStmt -> YulCase
dispatchCase sel body = MkCase (YulNum sel) body

||| Generate Yul for checking minimum calldata size
export
checkCalldataSize : (minSize : Integer) -> YulStmt -> YulStmt
checkCalldataSize minSize onFail =
  YIf (yulCall "lt" [yulCall "calldatasize" [], yulNum minSize])
      onFail

-- =============================================================================
-- ABI Encoding/Decoding Helpers
-- =============================================================================

||| Read uint256 from calldata at offset
export
readUint256 : (offset : Integer) -> YulExpr
readUint256 off = calldataload (yulNum off)

||| Read address from calldata at offset (address is in lower 20 bytes)
export
readAddress : (offset : Integer) -> YulExpr
readAddress off = yulCall "and"
  [ calldataload (yulNum off)
  , yulNum 0xffffffffffffffffffffffffffffffffffffffff  -- 20 bytes mask
  ]

||| Write uint256 to memory for return
export
writeReturnUint256 : (value : YulExpr) -> List YulStmt
writeReturnUint256 val =
  [ YExprStmt $ mstore (yulNum 0) val
  , YExprStmt $ yulReturn (yulNum 0) (yulNum 32)
  ]

||| Write boolean to memory for return
export
writeReturnBool : (value : YulExpr) -> List YulStmt
writeReturnBool val =
  [ YExprStmt $ mstore (yulNum 0) (yulCall "iszero" [yulCall "iszero" [val]])
  , YExprStmt $ yulReturn (yulNum 0) (yulNum 32)
  ]
