||| Yul Intermediate Representation
||| AST for Yul language (Solidity's low-level IR)
module Compiler.EVM.YulIR

import Data.List
import Data.SortedMap
import Data.SortedSet
import Data.String

%default covering

-- =============================================================================
-- Yul AST Types
-- =============================================================================

||| Yul identifier (variable/function name)
public export
YulId : Type
YulId = String

||| Yul literal values
public export
data YulLiteral
  = YulNum Integer           -- Numeric literal (256-bit)
  | YulHex String            -- Hex literal 0x...
  | YulStr String            -- String literal
  | YulBool Bool             -- Boolean (true/false)

public export
Show YulLiteral where
  show (YulNum n) = show n
  show (YulHex h) = "0x" ++ h
  show (YulStr s) = show s
  show (YulBool True) = "true"
  show (YulBool False) = "false"

||| Yul expression
public export
data YulExpr
  = YLit YulLiteral                        -- Literal value
  | YVar YulId                             -- Variable reference
  | YCall YulId (List YulExpr)             -- Function call: f(a, b, ...)

mutual
  ||| Yul statement
  public export
  data YulStmt
    = YBlock (List YulStmt)                  -- { stmt1 stmt2 ... }
    | YComment String                        -- /* comment */ for sourcemap
    | YLet (List YulId) YulExpr              -- let x, y := expr
    | YAssign (List YulId) YulExpr           -- x, y := expr
    | YIf YulExpr YulStmt                    -- if cond { body }
    | YSwitch YulExpr (List YulCase) (Maybe YulStmt)  -- switch expr { cases } default
    | YFor YulStmt YulExpr YulStmt YulStmt   -- for { init } cond { post } { body }
    | YBreak                                 -- break
    | YContinue                              -- continue
    | YLeave                                 -- leave (return from function)
    | YExprStmt YulExpr                      -- expr (for side effects)

  ||| Switch case
  public export
  data YulCase = MkCase YulLiteral YulStmt   -- case lit { body }

||| Source location for coverage mapping
public export
record SourceLoc where
  constructor MkSourceLoc
  moduleName : String    -- e.g., "TextDAO.Functions.Propose"
  functionName : String  -- e.g., "propose"
  startLine : Int
  startCol : Int
  endLine : Int
  endCol : Int

public export
Show SourceLoc where
  show loc = loc.moduleName ++ ":" ++ show loc.startLine ++ ":" ++ show loc.startCol
          ++ "--" ++ show loc.endLine ++ ":" ++ show loc.endCol

||| Format as Yul comment for sourcemap
export
sourceLocComment : SourceLoc -> String
sourceLocComment loc = "/* " ++ show loc ++ " */"

||| Yul function definition
public export
record YulFun where
  constructor MkYulFun
  name : YulId
  params : List YulId
  returns : List YulId
  body : YulStmt
  sourceLoc : Maybe SourceLoc  -- Source location for coverage

||| Yul object (top-level compilation unit)
public export
record YulObject where
  constructor MkYulObject
  name : YulId
  code : List YulStmt
  functions : List YulFun
  subObjects : List YulObject  -- For contract deployment

-- =============================================================================
-- Call Graph Analysis (for dead code elimination)
-- =============================================================================

mutual
  ||| Collect all function names called in an expression
  export
  collectCallsExpr : YulExpr -> SortedSet String
  collectCallsExpr (YLit _) = empty
  collectCallsExpr (YVar _) = empty
  collectCallsExpr (YCall fname args) =
    insert fname (foldl (\acc, e => union acc (collectCallsExpr e)) empty args)

  ||| Collect all function names called in a statement
  export
  collectCallsStmt : YulStmt -> SortedSet String
  collectCallsStmt (YBlock stmts) = foldl (\acc, s => union acc (collectCallsStmt s)) empty stmts
  collectCallsStmt (YLet _ expr) = collectCallsExpr expr
  collectCallsStmt (YAssign _ expr) = collectCallsExpr expr
  collectCallsStmt (YIf cond body) = union (collectCallsExpr cond) (collectCallsStmt body)
  collectCallsStmt (YSwitch sc cases mdef) =
    let scCalls = collectCallsExpr sc
        caseCalls = foldl (\acc, c => union acc (collectCallsCase c)) empty cases
        defCalls = maybe empty collectCallsStmt mdef
    in union scCalls (union caseCalls defCalls)
  collectCallsStmt (YFor init cond post body) =
    union (collectCallsStmt init) $
    union (collectCallsExpr cond) $
    union (collectCallsStmt post) (collectCallsStmt body)
  collectCallsStmt YBreak = empty
  collectCallsStmt YContinue = empty
  collectCallsStmt YLeave = empty
  collectCallsStmt (YExprStmt e) = collectCallsExpr e
  collectCallsStmt (YComment _) = empty

  collectCallsCase : YulCase -> SortedSet String
  collectCallsCase (MkCase _ body) = collectCallsStmt body

||| Collect all function names called in a function
export
collectCallsFun : YulFun -> SortedSet String
collectCallsFun f = collectCallsStmt f.body

||| Find all reachable functions starting from entry points
export
findReachable : List String -> List YulFun -> SortedSet String
findReachable entryPoints funs = go (SortedSet.fromList entryPoints) (SortedSet.fromList entryPoints)
  where
    funMap : SortedMap String YulFun
    funMap = foldl (\m, f => insert f.name f m) empty funs

    go : SortedSet String -> SortedSet String -> SortedSet String
    go visited worklist =
      case SortedSet.toList worklist of
        [] => visited
        (fname :: rest) =>
          case lookup fname funMap of
            Nothing => go visited (SortedSet.fromList rest)
            Just f =>
              let calls = collectCallsFun f
                  newCalls = difference calls visited
                  newVisited = union visited newCalls
                  newWorklist = union (SortedSet.fromList rest) newCalls
              in go newVisited newWorklist

||| Filter functions to only those reachable from entry points
export
eliminateDeadCode : List String -> List YulFun -> List YulFun
eliminateDeadCode entryPoints funs =
  let reachable = findReachable entryPoints funs
  in filter (\f => contains f.name reachable) funs

-- =============================================================================
-- Pretty Printing
-- =============================================================================

mutual
  export
  showExpr : YulExpr -> String
  showExpr (YLit lit) = show lit
  showExpr (YVar v) = v
  showExpr (YCall f args) = f ++ "(" ++ joinBy ", " (map showExpr args) ++ ")"

  export
  showStmt : Nat -> YulStmt -> String
  showStmt ind (YBlock stmts) =
    let inner = unlines (map (showStmt (ind + 2)) stmts)
    in indent ind ++ "{\n" ++ inner ++ indent ind ++ "}"
  showStmt ind (YLet vars expr) =
    indent ind ++ "let " ++ joinBy ", " vars ++ " := " ++ showExpr expr
  showStmt ind (YAssign vars expr) =
    indent ind ++ joinBy ", " vars ++ " := " ++ showExpr expr
  showStmt ind (YIf cond body) =
    indent ind ++ "if " ++ showExpr cond ++ " " ++ showStmt 0 body
  showStmt ind (YSwitch sc cases def) =
    indent ind ++ "switch " ++ showExpr sc ++ "\n" ++
    unlines (map (showCase ind) cases) ++
    maybe "" (\d => indent ind ++ "default " ++ showStmt 0 d) def
  showStmt ind (YFor init cond post body) =
    indent ind ++ "for " ++ showStmt 0 init ++ " " ++
    showExpr cond ++ " " ++ showStmt 0 post ++ " " ++ showStmt 0 body
  showStmt ind YBreak = indent ind ++ "break"
  showStmt ind YContinue = indent ind ++ "continue"
  showStmt ind YLeave = indent ind ++ "leave"
  showStmt ind (YExprStmt e) = indent ind ++ showExpr e
  showStmt ind (YComment c) = indent ind ++ "/* " ++ c ++ " */"

  showCase : Nat -> YulCase -> String
  showCase ind (MkCase lit body) =
    indent ind ++ "case " ++ show lit ++ " " ++ showStmt 0 body

  indent : Nat -> String
  indent n = pack (replicate n ' ')

||| Show block body with given indentation (without leading indent for opening brace)
showBlockBody : Nat -> YulStmt -> String
showBlockBody ind (YBlock stmts) =
  let inner = unlines (map (showStmt (ind + 2)) stmts)
  in "{\n" ++ inner ++ pack (replicate ind ' ') ++ "}"
showBlockBody ind stmt = showStmt ind stmt

||| Show function with given base indentation
||| Includes @source comment for coverage mapping if sourceLoc is present
showFunIndented : Nat -> YulFun -> String
showFunIndented baseInd f =
  let ind = pack (replicate baseInd ' ')
      sourceComment = case f.sourceLoc of
                        Just loc => ind ++ "// @source: " ++ show loc ++ "\n"
                        Nothing => ind ++ "// @source: NONE\n"  -- Debug: always output
  in sourceComment ++
     ind ++ "function " ++ f.name ++
     "(" ++ joinBy ", " f.params ++ ")" ++
     (if null f.returns then "" else " -> " ++ joinBy ", " f.returns) ++
     " " ++ showBlockBody baseInd f.body

export
showFun : YulFun -> String
showFun = showFunIndented 0

||| Show object with indentation level
showObjectIndented : Nat -> YulObject -> String
showObjectIndented baseInd obj =
  let ind = pack (replicate baseInd ' ')
      innerInd = pack (replicate (baseInd + 2) ' ')
      codeInd = baseInd + 4
  in ind ++ "object \"" ++ obj.name ++ "\" {\n" ++
     innerInd ++ "code {\n" ++
     unlines (map (showStmt codeInd) obj.code) ++
     unlines (map (showFunIndented codeInd) obj.functions) ++
     innerInd ++ "}\n" ++
     unlines (map (showObjectIndented (baseInd + 2)) obj.subObjects) ++
     ind ++ "}"

export
showObject : YulObject -> String
showObject = showObjectIndented 0

-- =============================================================================
-- Builder Helpers
-- =============================================================================

||| Create a function call expression
export
yulCall : YulId -> List YulExpr -> YulExpr
yulCall = YCall

||| Create a variable reference
export
yulVar : YulId -> YulExpr
yulVar = YVar

||| Create a numeric literal
export
yulNum : Integer -> YulExpr
yulNum n = YLit (YulNum n)

||| Create a let binding
export
yulLet : YulId -> YulExpr -> YulStmt
yulLet v e = YLet [v] e

||| Create assignment
export
yulAssign : YulId -> YulExpr -> YulStmt
yulAssign v e = YAssign [v] e

||| EVM opcodes as Yul calls
export
sstore : YulExpr -> YulExpr -> YulExpr
sstore key val = yulCall "sstore" [key, val]

export
sload : YulExpr -> YulExpr
sload key = yulCall "sload" [key]

export
mstore : YulExpr -> YulExpr -> YulExpr
mstore offset val = yulCall "mstore" [offset, val]

export
mload : YulExpr -> YulExpr
mload offset = yulCall "mload" [offset]

export
calldataload : YulExpr -> YulExpr
calldataload offset = yulCall "calldataload" [offset]

export
revert : YulExpr -> YulExpr -> YulExpr
revert offset size = yulCall "revert" [offset, size]

export
yulReturn : YulExpr -> YulExpr -> YulExpr
yulReturn offset size = yulCall "return" [offset, size]
