||| EVM Memory Management
||| Bump allocator and data layout for Idris values on EVM
module Compiler.EVM.Memory

import Compiler.EVM.YulIR
import Data.List

%default covering

-- =============================================================================
-- Memory Layout Constants
-- =============================================================================

||| Free memory pointer location (Solidity convention)
public export
FREE_MEM_PTR : Integer
FREE_MEM_PTR = 0x40

||| Initial free memory (after reserved area)
public export
INITIAL_FREE_MEM : Integer
INITIAL_FREE_MEM = 0x80

||| Word size in bytes (EVM uses 256-bit words)
public export
WORD_SIZE : Integer
WORD_SIZE = 32

-- =============================================================================
-- Memory Allocation (Bump Allocator)
-- =============================================================================

||| Generate Yul code to allocate n words of memory
||| Returns pointer to allocated memory (assigns to "ptr" variable which must be pre-declared)
export
allocWords : (numWords : Integer) -> List YulStmt
allocWords n =
  [ yulAssign "ptr" (mload (yulNum FREE_MEM_PTR))
  , YExprStmt $ mstore (yulNum FREE_MEM_PTR)
      (yulCall "add" [yulVar "ptr", yulNum (n * WORD_SIZE)])
  ]

||| Generate Yul function for memory allocation
export
allocatorFunction : YulFun
allocatorFunction = MkYulFun
  { name = "alloc"
  , params = ["size"]
  , returns = ["ptr"]
  , body = YBlock
      [ yulAssign "ptr" (mload (yulNum FREE_MEM_PTR))
      , YExprStmt $ mstore (yulNum FREE_MEM_PTR)
          (yulCall "add" [yulVar "ptr", yulVar "size"])
      ]
  , sourceLoc = Nothing  -- Runtime helper, no source location
  }

-- =============================================================================
-- Data Representation
-- =============================================================================

||| Tag values for Idris data types
||| First word of any heap object is its constructor tag
public export
data Tag = MkTag Integer

||| Write a tagged value to memory
||| Layout: [tag (32 bytes)] [field1 (32 bytes)] [field2 (32 bytes)] ...
export
writeTaggedValue : (ptr : YulExpr) -> (tag : Integer) -> (fields : List YulExpr) -> List YulStmt
writeTaggedValue ptr tag fields =
  let tagStmt = YExprStmt $ mstore ptr (yulNum tag)
      indices = take (length fields) [1..]
      fieldStmts = zipWith writeField indices fields
  in tagStmt :: fieldStmts
  where
    writeField : Integer -> YulExpr -> YulStmt
    writeField idx val = YExprStmt $ mstore
      (yulCall "add" [ptr, yulNum (idx * WORD_SIZE)])
      val

||| Read tag from a heap object
export
readTag : (ptr : YulExpr) -> YulExpr
readTag ptr = mload ptr

||| Read field from a heap object (0-indexed)
export
readField : (ptr : YulExpr) -> (fieldIdx : Integer) -> YulExpr
readField ptr idx = mload (yulCall "add" [ptr, yulNum ((idx + 1) * WORD_SIZE)])

-- =============================================================================
-- Closure Representation
-- =============================================================================

||| Closure layout:
||| Word 0: Function pointer (or function ID)
||| Word 1: Arity (remaining arguments needed)
||| Word 2+: Captured/partially applied arguments
export
mkClosure : (funcId : Integer) -> (arity : Integer) -> (captured : List YulExpr) -> List YulStmt
mkClosure funcId arity captured =
  let numWords = 2 + cast (length captured)
      allocStmts = allocWords numWords
      writeStmts =
        [ YExprStmt $ mstore (yulVar "ptr") (yulNum funcId)
        , YExprStmt $ mstore (yulCall "add" [yulVar "ptr", yulNum WORD_SIZE]) (yulNum arity)
        ] ++ zipWith writeCaptured (take (length captured) [2..]) captured
  in allocStmts ++ writeStmts
  where
    writeCaptured : Integer -> YulExpr -> YulStmt
    writeCaptured idx val = YExprStmt $ mstore
      (yulCall "add" [yulVar "ptr", yulNum (idx * WORD_SIZE)])
      val

||| Read closure function ID
export
closureFuncId : YulExpr -> YulExpr
closureFuncId ptr = mload ptr

||| Read closure remaining arity
export
closureArity : YulExpr -> YulExpr
closureArity ptr = mload (yulCall "add" [ptr, yulNum WORD_SIZE])

||| Read closure captured argument (0-indexed)
export
closureCaptured : YulExpr -> Integer -> YulExpr
closureCaptured ptr idx = mload (yulCall "add" [ptr, yulNum ((idx + 2) * WORD_SIZE)])

-- =============================================================================
-- Initialization
-- =============================================================================

||| Initialize memory allocator (set free pointer)
export
initMemory : List YulStmt
initMemory =
  [ YExprStmt $ mstore (yulNum FREE_MEM_PTR) (yulNum INITIAL_FREE_MEM)
  ]
