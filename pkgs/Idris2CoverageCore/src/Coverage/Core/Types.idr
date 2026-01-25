||| Core Types for Idris2 Coverage Analysis
|||
||| Shared type definitions for dumpcases parsing across backends:
|||   - Chez Scheme (idris2-coverage)
|||   - WASM/ICP (idris2-dfx-coverage)
|||   - WASM/EVM (idris2-evm-coverage)
|||
||| Based on Idris2's --dumpcases output format:
|||   NAME = [args]: (%case EXPR [CASES] DEFAULT)
|||   (%concase [tag] TYPE Just N [bindings] BODY)
|||   (%constcase VALUE BODY)
module Coverage.Core.Types

import Data.List
import Data.List1
import Data.Maybe
import Data.String

%default total

-- =============================================================================
-- Case Alternative Types
-- =============================================================================

||| Type of case alternative in dumpcases output
public export
data AltType
  = ConCase     -- Constructor case (%concase)
  | ConstCase   -- Constant case (%constcase)

public export
Show AltType where
  show ConCase = "concase"
  show ConstCase = "constcase"

public export
Eq AltType where
  ConCase == ConCase = True
  ConstCase == ConstCase = True
  _ == _ = False

||| A single case alternative
public export
record CaseAlt where
  constructor MkCaseAlt
  altType : AltType
  constructorName : Maybe String   -- For ConCase: constructor name
  constValue : Maybe Integer       -- For ConstCase: constant value

public export
Show CaseAlt where
  show alt = show alt.altType ++
             maybe "" (\n => ":" ++ n) alt.constructorName ++
             maybe "" (\v => "=" ++ show v) alt.constValue

public export
Eq CaseAlt where
  a == b = a.altType == b.altType &&
           a.constructorName == b.constructorName &&
           a.constValue == b.constValue

-- =============================================================================
-- Case Expression
-- =============================================================================

||| A case expression with its alternatives
public export
record CaseExpr where
  constructor MkCaseExpr
  alternatives : List CaseAlt
  hasDefault : Bool               -- True if default case exists (Just expr)

public export
Show CaseExpr where
  show ce = "case[" ++ show (length ce.alternatives) ++ " alts" ++
            (if ce.hasDefault then ", default" else "") ++ "]"

public export
Eq CaseExpr where
  a == b = a.alternatives == b.alternatives && a.hasDefault == b.hasDefault

||| Number of branch targets in a case expression
public export
caseTargetCount : CaseExpr -> Nat
caseTargetCount ce =
  let base = length ce.alternatives
  in if ce.hasDefault then base + 1 else base

-- =============================================================================
-- Function Cases
-- =============================================================================

||| Function with its case expressions from dumpcases
public export
record FuncCases where
  constructor MkFuncCases
  funcName : String
  cases : List CaseExpr
  totalBranches : Nat             -- Sum of all alternatives + defaults

public export
Show FuncCases where
  show fc = fc.funcName ++ ": " ++
            show (length fc.cases) ++ " cases, " ++
            show fc.totalBranches ++ " branches"

public export
Eq FuncCases where
  a == b = a.funcName == b.funcName

||| Calculate total branches from case expressions
public export
calcTotalBranches : List CaseExpr -> Nat
calcTotalBranches = foldl (\acc, ce => acc + caseTargetCount ce) 0

-- =============================================================================
-- Branch Identifier (for coverage aggregation)
-- =============================================================================

||| Unique identifier for a branch in compiled code
||| Enables OR-aggregation across multiple test runs
public export
record BranchId where
  constructor MkBranchId
  moduleName : String     -- "Main"
  funcName : String       -- "dispatchWith"
  caseIndex : Nat         -- Which %case in the function (0-indexed)
  branchIndex : Nat       -- Which %concase/%constcase in the %case (0-indexed)

public export
Show BranchId where
  show b = b.moduleName ++ "." ++ b.funcName ++ ":case" ++ show b.caseIndex
        ++ ":branch" ++ show b.branchIndex

public export
Eq BranchId where
  b1 == b2 = b1.moduleName == b2.moduleName
          && b1.funcName == b2.funcName
          && b1.caseIndex == b2.caseIndex
          && b1.branchIndex == b2.branchIndex

public export
Ord BranchId where
  compare b1 b2 =
    case compare b1.moduleName b2.moduleName of
      EQ => case compare b1.funcName b2.funcName of
              EQ => case compare b1.caseIndex b2.caseIndex of
                      EQ => compare b1.branchIndex b2.branchIndex
                      x  => x
              x  => x
      x  => x

-- =============================================================================
-- Module Name Utilities
-- =============================================================================

||| Extract module name from fully qualified function name
||| e.g., "Prelude.Types.null" -> "Prelude.Types"
public export
getModuleName : String -> String
getModuleName funcName =
  let parts = forget $ split (== '.') funcName
  in case parts of
       [] => ""
       [_] => ""
       xs => joinBy "." (initList xs)
  where
    initList : List a -> List a
    initList [] = []
    initList [_] = []
    initList (x :: xs@(_ :: _)) = x :: initList xs

||| Extract simple function name from fully qualified name
||| e.g., "Prelude.Types.null" -> "null"
public export
getSimpleFuncName : String -> String
getSimpleFuncName funcName =
  let parts = forget $ split (== '.') funcName
  in fromMaybe funcName (last' parts)

||| Parse fully qualified name into (module, func)
public export
parseFullName : String -> (String, String)
parseFullName fullName = (getModuleName fullName, getSimpleFuncName fullName)
