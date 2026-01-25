||| EVM Primitive Operations
||| Mapping Idris primitives to EVM/Yul operations
module Compiler.EVM.Primitives

import Compiler.EVM.YulIR
import Core.TT
import Data.Vect

%default covering

-- =============================================================================
-- Arithmetic Primitives
-- =============================================================================

||| Compile a primitive function to Yul
export
compilePrimFn : PrimFn arity -> Vect arity YulExpr -> YulExpr
-- Integer arithmetic
compilePrimFn (Add IntType) [a, b] = yulCall "add" [a, b]
compilePrimFn (Sub IntType) [a, b] = yulCall "sub" [a, b]
compilePrimFn (Mul IntType) [a, b] = yulCall "mul" [a, b]
compilePrimFn (Div IntType) [a, b] = yulCall "sdiv" [a, b]  -- signed division
compilePrimFn (Mod IntType) [a, b] = yulCall "smod" [a, b]  -- signed modulo
compilePrimFn (Neg IntType) [a] = yulCall "sub" [yulNum 0, a]

-- Integer comparison
compilePrimFn (LT IntType) [a, b] = yulCall "slt" [a, b]
compilePrimFn (LTE IntType) [a, b] = yulCall "iszero" [yulCall "sgt" [a, b]]
compilePrimFn (EQ IntType) [a, b] = yulCall "eq" [a, b]
compilePrimFn (GTE IntType) [a, b] = yulCall "iszero" [yulCall "slt" [a, b]]
compilePrimFn (GT IntType) [a, b] = yulCall "sgt" [a, b]

-- Integer operations (for EVM 256-bit values, use IntegerType)
compilePrimFn (Add IntegerType) [a, b] = yulCall "add" [a, b]
compilePrimFn (Sub IntegerType) [a, b] = yulCall "sub" [a, b]
compilePrimFn (Mul IntegerType) [a, b] = yulCall "mul" [a, b]
compilePrimFn (Div IntegerType) [a, b] = yulCall "div" [a, b]   -- unsigned
compilePrimFn (Mod IntegerType) [a, b] = yulCall "mod" [a, b]   -- unsigned

compilePrimFn (LT IntegerType) [a, b] = yulCall "lt" [a, b]
compilePrimFn (LTE IntegerType) [a, b] = yulCall "iszero" [yulCall "gt" [a, b]]
compilePrimFn (EQ IntegerType) [a, b] = yulCall "eq" [a, b]
compilePrimFn (GTE IntegerType) [a, b] = yulCall "iszero" [yulCall "lt" [a, b]]
compilePrimFn (GT IntegerType) [a, b] = yulCall "gt" [a, b]

-- Bits64 operations (closest native type)
compilePrimFn (Add Bits64Type) [a, b] = yulCall "add" [a, b]
compilePrimFn (Sub Bits64Type) [a, b] = yulCall "sub" [a, b]
compilePrimFn (Mul Bits64Type) [a, b] = yulCall "mul" [a, b]
compilePrimFn (Div Bits64Type) [a, b] = yulCall "div" [a, b]
compilePrimFn (Mod Bits64Type) [a, b] = yulCall "mod" [a, b]

-- Bitwise operations (Bits64)
compilePrimFn (BAnd Bits64Type) [a, b] = yulCall "and" [a, b]
compilePrimFn (BOr Bits64Type) [a, b] = yulCall "or" [a, b]
compilePrimFn (BXOr Bits64Type) [a, b] = yulCall "xor" [a, b]
compilePrimFn (ShiftL Bits64Type) [a, b] = yulCall "shl" [b, a]  -- Note: shl(shift, value)
compilePrimFn (ShiftR Bits64Type) [a, b] = yulCall "shr" [b, a]

-- Cast operations
compilePrimFn (Cast IntType IntegerType) [a] = a  -- Same representation on EVM
compilePrimFn (Cast IntegerType IntType) [a] = a  -- Same representation on EVM

-- String operations (limited support - strings are expensive on EVM)
compilePrimFn StrLength [s] = yulCall "mload" [s]  -- Assume first word is length
compilePrimFn StrHead [s] = yulCall "byte" [yulNum 0, yulCall "mload" [yulCall "add" [s, yulNum 32]]]

-- Believe me (unsafe cast)
compilePrimFn BelieveMe [_, _, x] = x

-- Default: crash (unsupported primitive)
compilePrimFn _ _ = yulCall "revert" [yulNum 0, yulNum 0]

-- =============================================================================
-- Constant Compilation
-- =============================================================================

||| Compile a constant to Yul expression
export
compileConstant : Constant -> YulExpr
compileConstant (I x) = yulNum (cast x)
compileConstant (I8 x) = yulNum (cast x)
compileConstant (I16 x) = yulNum (cast x)
compileConstant (I32 x) = yulNum (cast x)
compileConstant (I64 x) = yulNum (cast x)
compileConstant (BI x) = yulNum x
compileConstant (B8 x) = yulNum (cast x)
compileConstant (B16 x) = yulNum (cast x)
compileConstant (B32 x) = yulNum (cast x)
compileConstant (B64 x) = yulNum (cast x)
compileConstant (Ch c) = yulNum (cast (ord c))
compileConstant (Db d) = yulNum 0  -- Floats not supported on EVM
compileConstant (PrT _) = yulNum 0  -- Type erased at runtime
compileConstant WorldVal = yulNum 0  -- World token erased
compileConstant (Str s) = yulNum 0  -- TODO: String handling

-- =============================================================================
-- Type Representation
-- =============================================================================

||| Check if a type can be represented as a single EVM word
export
isWordType : Constant -> Bool
isWordType (I _) = True
isWordType (I8 _) = True
isWordType (I16 _) = True
isWordType (I32 _) = True
isWordType (I64 _) = True
isWordType (BI _) = True
isWordType (B8 _) = True
isWordType (B16 _) = True
isWordType (B32 _) = True
isWordType (B64 _) = True
isWordType (Ch _) = True
isWordType WorldVal = True
isWordType _ = False
