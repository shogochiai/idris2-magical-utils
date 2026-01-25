||| EVM Backend Code Generator
||| Compiles Idris2 ANF to Yul
module Compiler.EVM.Codegen

import Compiler.EVM.YulIR
import Compiler.EVM.Memory
import Compiler.EVM.Primitives
import Compiler.EVM.Foreign

import Core.CompileExpr
import Core.CompileExpr.Pretty
import Core.Context
import Core.Core
import Core.Name
import Core.TT
import Core.FC

import Compiler.ANF
import Compiler.Common

import Data.List
import Data.List1
import Data.SortedMap
import Data.SortedSet
import Data.String
import Data.Vect
import Control.Monad.Either
import System.File

%default covering

-- =============================================================================
-- Name Mangling
-- =============================================================================

||| Show namespace as underscore-separated string
showNS : Namespace -> String
showNS ns = joinBy "_" (reverse $ unsafeUnfoldNamespace ns)

||| Extract EVM opcode from foreign string directly
||| Handles format like "evm:sload" by splitting on ":"
extractEvmOp : String -> Maybe (String, List String)
extractEvmOp s =
  if isPrefixOf "evm:" s
    then Just (substr 4 (length s) s, [])
    else Nothing

||| Sanitize string for Yul identifier (remove invalid characters)
sanitize : String -> String
sanitize s = pack $ map sanitizeChar (unpack s)
  where
    sanitizeChar : Char -> Char
    sanitizeChar c =
      if isAlphaNum c || c == '_'
        then c
        else '_'

||| Convert Idris name to valid Yul identifier
export
mangleName : Name -> YulId
mangleName (NS ns n) = showNS ns ++ "_" ++ mangleName n
mangleName (UN (Basic n)) = "u_" ++ sanitize n
mangleName (UN (Field n)) = "f_" ++ sanitize n
mangleName (UN Underscore) = "_"
mangleName (MN n i) = "m_" ++ sanitize n ++ "_" ++ show i
mangleName (PV n i) = "pv_" ++ mangleName n ++ "_" ++ show i
mangleName (DN _ n) = mangleName n
mangleName (Nested (i, j) n) = "n_" ++ show i ++ "_" ++ show j ++ "_" ++ mangleName n
mangleName (CaseBlock outer i) = "case_" ++ sanitize outer ++ "_" ++ show i
mangleName (WithBlock outer i) = "with_" ++ sanitize outer ++ "_" ++ show i
mangleName (Resolved i) = "r_" ++ show i

||| Convert variable index to Yul variable name
varName : Int -> YulId
varName i = "v" ++ show i

-- Find the "u_" or "m_" prefix to split module from function
findFuncPart : List String -> List String -> Maybe (List String, String)
findFuncPart acc [] = Nothing
findFuncPart acc ("u" :: funcName :: rest) = Just (reverse acc, funcName)
findFuncPart acc ("m" :: funcName :: _ :: rest) = Just (reverse acc, funcName)
findFuncPart acc ("f" :: funcName :: rest) = Just (reverse acc, funcName)
findFuncPart acc (p :: rest) = findFuncPart (p :: acc) rest

||| Extract source location from Idris name for coverage mapping
||| Always returns a SourceLoc - falls back to mangled name
||| Note: Line/col info is 0 when derived from Name only (no FC)
nameToSourceLoc : Name -> Maybe SourceLoc
nameToSourceLoc n =
  let mangled = mangleName n
      parts = forget $ split (== '_') mangled
  in case findFuncPart [] parts of
       Just (modParts, funcName) =>
         let modName = if null modParts then "<generated>" else joinBy "." modParts
         in Just $ MkSourceLoc modName funcName 0 0 0 0
       Nothing => Just $ MkSourceLoc "<unknown>" mangled 0 0 0 0  -- Fallback

||| Extract module name from OriginDesc
originToModule : OriginDesc -> String
originToModule (PhysicalIdrSrc ident) = show ident
originToModule (PhysicalPkgSrc fname) = fname
originToModule (Virtual ident) = show ident

||| Convert FC to SourceLoc with line/col info
||| Returns Nothing for EmptyFC
fcToSourceLoc : FC -> Maybe SourceLoc
fcToSourceLoc EmptyFC = Nothing
fcToSourceLoc (MkFC origin (sl, sc) (el, ec)) =
  Just $ MkSourceLoc (originToModule origin) "" sl sc el ec
fcToSourceLoc (MkVirtualFC origin (sl, sc) (el, ec)) =
  Just $ MkSourceLoc (originToModule origin) "" sl sc el ec

||| Generate a YComment from FC for sourcemap
||| Format: /* Module:startLine:startCol--endLine:endCol */
fcToComment : FC -> Maybe YulStmt
fcToComment fc = map (\loc => YComment (show loc)) (fcToSourceLoc fc)

||| Convert AVar to Yul expression
compileAVar : AVar -> YulExpr
compileAVar (ALocal i) = yulVar (varName i)
compileAVar ANull = yulNum 0

-- =============================================================================
-- Function Arity Tracking
-- =============================================================================

||| Map from mangled function name to its arity
FnArityMap : Type
FnArityMap = SortedMap String Nat

||| Set of functions that don't return values
NoReturnSet : Type
NoReturnSet = SortedSet String

||| Map from function name to unique closure ID
ClosureIdMap : Type
ClosureIdMap = SortedMap String Integer

||| Collect function arities from all definitions
collectArities : List (Name, ANFDef) -> FnArityMap
collectArities defs = foldl addArity empty defs
  where
    addArity : FnArityMap -> (Name, ANFDef) -> FnArityMap
    addArity m (n, MkAFun args _) = insert (mangleName n) (length args) m
    addArity m (n, MkACon _ arity _) = insert (mangleName n) (cast arity) m
    addArity m (n, MkAForeign _ fargs _) = insert (mangleName n) (length fargs) m
    addArity m (n, MkAError _) = insert (mangleName n) 0 m

||| Collect functions that don't return values
collectNoReturn : List (Name, ANFDef) -> NoReturnSet
collectNoReturn defs = foldl addNoReturn empty defs
  where
    addNoReturn : NoReturnSet -> (Name, ANFDef) -> NoReturnSet
    addNoReturn s (n, MkAForeign _ _ ret) =
      case ret of
        CFUnit => insert (mangleName n) s
        CFIORes CFUnit => insert (mangleName n) s
        _ => s
    addNoReturn s (n, MkAError _) = insert (mangleName n) s
    addNoReturn s _ = s

||| Pad arguments to match expected arity
padArgs : FnArityMap -> String -> List YulExpr -> List YulExpr
padArgs arities fnName args =
  case lookup fnName arities of
    Nothing => args  -- Unknown function, use as-is
    Just expectedArity =>
      let currentLen = length args
          needed = minus expectedArity currentLen
      in args ++ replicate needed (yulNum 0)

-- =============================================================================
-- Closure Collection (for dispatch table generation)
-- =============================================================================

mutual
  ||| Collect functions used as closures in an ANF expression
  collectClosuresExpr : ANF -> SortedSet String
  collectClosuresExpr (AV _ _) = empty
  collectClosuresExpr (AAppName _ _ _ _) = empty
  collectClosuresExpr (AUnderApp _ n _ _) = singleton (mangleName n)
  collectClosuresExpr (AApp _ _ _ _) = empty
  collectClosuresExpr (ACon _ _ _ _ _) = empty
  collectClosuresExpr (AOp _ _ _ _) = empty
  collectClosuresExpr (AExtPrim _ _ _ _) = empty
  collectClosuresExpr (APrimVal _ _) = empty
  collectClosuresExpr (AErased _) = empty
  collectClosuresExpr (ACrash _ _) = empty
  collectClosuresExpr (ALet _ _ val body) =
    union (collectClosuresExpr val) (collectClosuresExpr body)
  collectClosuresExpr (AConCase _ _ alts def) =
    let altClosures = foldl (\acc, alt => union acc (collectClosuresAlt alt)) empty alts
        defClosures = maybe empty collectClosuresExpr def
    in union altClosures defClosures
  collectClosuresExpr (AConstCase _ _ alts def) =
    let altClosures = foldl (\acc, alt => union acc (collectClosuresConstAlt alt)) empty alts
        defClosures = maybe empty collectClosuresExpr def
    in union altClosures defClosures

  collectClosuresAlt : AConAlt -> SortedSet String
  collectClosuresAlt (MkAConAlt _ _ _ _ body) = collectClosuresExpr body

  collectClosuresConstAlt : AConstAlt -> SortedSet String
  collectClosuresConstAlt (MkAConstAlt _ body) = collectClosuresExpr body

||| Collect all closure targets from definitions
collectClosureTargets : List (Name, ANFDef) -> SortedSet String
collectClosureTargets defs = foldl addClosures empty defs
  where
    addClosures : SortedSet String -> (Name, ANFDef) -> SortedSet String
    addClosures s (_, MkAFun _ body) = union s (collectClosuresExpr body)
    addClosures s _ = s

||| Build closure ID map from set of closure targets
buildClosureIdMap : SortedSet String -> ClosureIdMap
buildClosureIdMap targets =
  let names = SortedSet.toList targets
      indexed = zip (take (length names) [1..]) names
  in foldl (\m, (idx, name) => insert name idx m) empty indexed

-- =============================================================================
-- ANF to Yul Compilation
-- =============================================================================

||| Compilation context
record CompileCtx where
  constructor MkCompileCtx
  arities : FnArityMap
  closureIds : ClosureIdMap
  nextCaseId : Nat  -- Unique ID counter for case_result variables

||| Generate next unique case result variable name and return updated context
nextCaseVar : CompileCtx -> (CompileCtx, String)
nextCaseVar ctx =
  let varName = "case_result_" ++ show ctx.nextCaseId
  in ({ nextCaseId $= (+1) } ctx, varName)

mutual
  ||| Compile ANF expression to (context, statements, expression)
  ||| Returns updated context (with incremented case ID), statements, and expression
  compileANFExprWithStmts : CompileCtx -> ANF -> Core (CompileCtx, List YulStmt, YulExpr)

  -- Variable reference
  compileANFExprWithStmts ctx (AV _ var) = pure (ctx, [], compileAVar var)

  -- Application to named function
  compileANFExprWithStmts ctx (AAppName _ _ n args) = do
    let fnName = mangleName n
    let compiledArgs = map compileAVar args
    let paddedArgs = padArgs ctx.arities fnName compiledArgs
    pure (ctx, [], yulCall fnName paddedArgs)

  -- Partial application - create a closure
  compileANFExprWithStmts ctx (AUnderApp _ n missing args) = do
    let fnName = mangleName n
    let compiledArgs = map compileAVar args
    case lookup fnName ctx.closureIds of
      Just closureId =>
        -- mk_closure requires exactly 6 args: func_id, arity, arg0, arg1, arg2, arg3
        let paddedClosureArgs = take 4 (compiledArgs ++ replicate 4 (yulNum 0))
        in pure (ctx, [], yulCall "mk_closure" ([yulNum closureId, yulNum (cast missing)] ++ paddedClosureArgs))
      Nothing =>
        let paddedArgs = padArgs ctx.arities fnName compiledArgs
        in pure (ctx, [], yulCall fnName paddedArgs)

  -- Application of closure to argument
  compileANFExprWithStmts ctx (AApp _ _ closure arg) = do
    pure (ctx, [], yulCall "apply_closure" [compileAVar closure, compileAVar arg])

  -- Constructor application
  compileANFExprWithStmts ctx (ACon _ n ci (Just tag) args) = do
    pure (ctx, [], yulNum (cast tag))

  compileANFExprWithStmts ctx (ACon _ n ci Nothing args) = do
    pure (ctx, [], yulNum 0)

  -- Primitive operation
  compileANFExprWithStmts ctx (AOp _ _ fn args) = do
    let compiledArgs = map compileAVar args
    let expr = compilePrimFn fn compiledArgs
    -- Check if result is a void opcode (like revert)
    case expr of
      YCall fname _ => if isVoidOp fname
                         then pure (ctx, [YExprStmt expr], yulNum 0)
                         else pure (ctx, [], expr)
      _ => pure (ctx, [], expr)
    where
      isVoidOp : String -> Bool
      isVoidOp "revert" = True
      isVoidOp "return" = True
      isVoidOp "stop" = True
      isVoidOp _ = False

  -- External primitive (FFI)
  compileANFExprWithStmts ctx (AExtPrim _ _ n args) = do
    let compiledArgs = map compileAVar args
    case parseEvmForeign (show n) of
      Just (op, _) =>
        let expr = evmOpcodeToYul op compiledArgs
        in case expr of
             YCall fname _ => if isVoidOp fname
                                then pure (ctx, [YExprStmt expr], yulNum 0)
                                else pure (ctx, [], expr)
             _ => pure (ctx, [], expr)
      Nothing =>
        let fnName = mangleName n
            paddedArgs = padArgs ctx.arities fnName compiledArgs
        in pure (ctx, [], yulCall fnName paddedArgs)
    where
      isVoidOp : String -> Bool
      isVoidOp "revert" = True
      isVoidOp "return" = True
      isVoidOp "stop" = True
      isVoidOp "sstore" = True
      isVoidOp "mstore" = True
      isVoidOp "mstore8" = True
      isVoidOp "log0" = True
      isVoidOp "log1" = True
      isVoidOp "log2" = True
      isVoidOp "log3" = True
      isVoidOp "log4" = True
      isVoidOp _ = False

  -- Constant
  compileANFExprWithStmts ctx (APrimVal _ c) = pure (ctx, [], compileConstant c)

  -- Erased value
  compileANFExprWithStmts ctx (AErased _) = pure (ctx, [], yulNum 0)

  -- Crash - revert doesn't return a value, so emit as statement then return 0 (unreachable)
  compileANFExprWithStmts ctx (ACrash _ msg) =
    pure (ctx, [YExprStmt (yulCall "revert" [yulNum 0, yulNum 0])], yulNum 0)

  -- Let binding - THIS IS THE KEY FIX
  -- Generate the assignment statement, then compile the body
  -- This handles nested lets by hoisting inner assignments before the outer one
  compileANFExprWithStmts ctx (ALet fc var val body) = do
    (ctx1, valStmts, valExpr) <- compileANFExprWithStmts ctx val
    (ctx2, bodyStmts, bodyExpr) <- compileANFExprWithStmts ctx1 body
    -- The assignment for this let binding
    -- For void opcodes (revert, return, etc.), emit as statement instead of assignment
    let assignStmt = case valExpr of
          YCall fname _ => if isVoidOpcode fname
                             then YExprStmt valExpr
                             else yulAssign (varName var) valExpr
          _ => yulAssign (varName var) valExpr
    -- Add source location comment before assignment
    let comment = fcToComment fc
    let stmtsWithComment = case comment of
          Just c  => valStmts ++ [c, assignStmt] ++ bodyStmts
          Nothing => valStmts ++ [assignStmt] ++ bodyStmts
    pure (ctx2, stmtsWithComment, bodyExpr)
    where
      isVoidOpcode : String -> Bool
      isVoidOpcode "revert" = True
      isVoidOpcode "return" = True
      isVoidOpcode "stop" = True
      isVoidOpcode "selfdestruct" = True
      isVoidOpcode "log0" = True
      isVoidOpcode "log1" = True
      isVoidOpcode "log2" = True
      isVoidOpcode "log3" = True
      isVoidOpcode "log4" = True
      isVoidOpcode "sstore" = True
      isVoidOpcode "mstore" = True
      isVoidOpcode "mstore8" = True
      isVoidOpcode _ = False

  -- Case expression: generate switch with result assignment in each branch
  -- Uses unique case_result variable name to avoid nested case collisions
  compileANFExprWithStmts ctx (AConCase fc sc alts mdef) = do
    let scExpr = compileAVar sc
    let (ctx1, resultVar) = nextCaseVar ctx  -- Get unique variable name
    (ctx2, cases) <- compileConAltsExpr ctx1 scExpr resultVar alts
    (ctx3, defStmt) <- the (Core (CompileCtx, Maybe YulStmt)) $ case mdef of
      Just d => do
        (ctxD, dStmts, dExpr) <- compileANFExprWithStmts ctx2 d
        pure (ctxD, Just (YBlock (dStmts ++ [yulAssign resultVar dExpr])))
      Nothing => pure (ctx2, Nothing)
    let declStmt = yulLet resultVar (YLit (YulNum 0))  -- declare variable before switch
    let switchStmt = YSwitch (readTag scExpr) cases defStmt
    -- Add source location comment before switch
    let comment = fcToComment fc
    let stmts = case comment of
          Just c  => [c, declStmt, switchStmt]
          Nothing => [declStmt, switchStmt]
    pure (ctx3, stmts, YVar resultVar)
    where
      -- Generate field extraction for constructor arguments
      extractFields : YulExpr -> List Int -> List YulStmt
      extractFields ptr args =
        zipWith (\idx, var => yulAssign (varName var) (readField ptr idx))
                (take (length args) [0..]) args

      compileConAltExpr : CompileCtx -> YulExpr -> String -> AConAlt -> Core (CompileCtx, YulCase)
      compileConAltExpr ctxIn scPtr resVar (MkAConAlt n ci (Just tag) args body) = do
        let fieldExtracts = extractFields scPtr args
        (ctxOut, bodyStmts, bodyExpr) <- compileANFExprWithStmts ctxIn body
        pure (ctxOut, MkCase (YulNum (cast tag)) (YBlock (fieldExtracts ++ bodyStmts ++ [yulAssign resVar bodyExpr])))
      compileConAltExpr ctxIn scPtr resVar (MkAConAlt n ci Nothing args body) = do
        let fieldExtracts = extractFields scPtr args
        (ctxOut, bodyStmts, bodyExpr) <- compileANFExprWithStmts ctxIn body
        pure (ctxOut, MkCase (YulNum 0) (YBlock (fieldExtracts ++ bodyStmts ++ [yulAssign resVar bodyExpr])))

      -- Compile alternatives, threading context through
      compileConAltsExpr : CompileCtx -> YulExpr -> String -> List AConAlt -> Core (CompileCtx, List YulCase)
      compileConAltsExpr ctxIn scPtr resVar [] = pure (ctxIn, [])
      compileConAltsExpr ctxIn scPtr resVar (alt :: alts) = do
        (ctx', c) <- compileConAltExpr ctxIn scPtr resVar alt
        (ctx'', cs) <- compileConAltsExpr ctx' scPtr resVar alts
        pure (ctx'', c :: cs)

  compileANFExprWithStmts ctx (AConstCase fc sc alts mdef) = do
    let scExpr = compileAVar sc
    let (ctx1, resultVar) = nextCaseVar ctx  -- Get unique variable name
    (ctx2, cases) <- compileConstAltsExpr ctx1 resultVar alts
    (ctx3, defStmt) <- the (Core (CompileCtx, Maybe YulStmt)) $ case mdef of
      Just d => do
        (ctxD, dStmts, dExpr) <- compileANFExprWithStmts ctx2 d
        pure (ctxD, Just (YBlock (dStmts ++ [yulAssign resultVar dExpr])))
      Nothing => pure (ctx2, Nothing)
    let declStmt = yulLet resultVar (YLit (YulNum 0))  -- declare variable before switch
    let switchStmt = YSwitch scExpr cases defStmt
    -- Add source location comment before switch
    let comment = fcToComment fc
    let stmts = case comment of
          Just c  => [c, declStmt, switchStmt]
          Nothing => [declStmt, switchStmt]
    pure (ctx3, stmts, YVar resultVar)
    where
      constToLit : Constant -> YulLiteral
      constToLit (I x) = YulNum (cast x)
      constToLit (BI x) = YulNum x
      constToLit (B8 x) = YulNum (cast x)
      constToLit (B16 x) = YulNum (cast x)
      constToLit (B32 x) = YulNum (cast x)
      constToLit (B64 x) = YulNum (cast x)
      constToLit (Ch c) = YulNum (cast (ord c))
      constToLit _ = YulNum 0

      compileConstAltExpr : CompileCtx -> String -> AConstAlt -> Core (CompileCtx, YulCase)
      compileConstAltExpr ctxIn resVar (MkAConstAlt c body) = do
        (ctxOut, bodyStmts, bodyExpr) <- compileANFExprWithStmts ctxIn body
        pure (ctxOut, MkCase (constToLit c) (YBlock (bodyStmts ++ [yulAssign resVar bodyExpr])))

      -- Compile alternatives, threading context through
      compileConstAltsExpr : CompileCtx -> String -> List AConstAlt -> Core (CompileCtx, List YulCase)
      compileConstAltsExpr ctxIn resVar [] = pure (ctxIn, [])
      compileConstAltsExpr ctxIn resVar (alt :: alts) = do
        (ctx', c) <- compileConstAltExpr ctxIn resVar alt
        (ctx'', cs) <- compileConstAltsExpr ctx' resVar alts
        pure (ctx'', c :: cs)

||| Compile ANF expression to Yul expression (legacy interface)
compileANFExpr : CompileCtx -> ANF -> Core YulExpr
compileANFExpr ctx anf = do
  (_, _, expr) <- compileANFExprWithStmts ctx anf
  pure expr

||| Compile ANF expression (with arity map for proper call padding)
compileANFWithArities : FnArityMap -> ANF -> Core YulExpr
compileANFWithArities arities = compileANFExpr (MkCompileCtx arities empty 0)

||| Legacy wrapper (for backward compatibility)
export
compileANF : ANF -> Core YulExpr
compileANF = compileANFWithArities empty

||| Compile ANF to Yul statement (for let bindings and cases)
compileANFStmtCtx : CompileCtx -> ANF -> Core (List YulStmt)
compileANFStmtCtx ctx (ALet _ var val body) = do
  -- Use compileANFExprWithStmts to capture any nested let bindings
  (ctx1, valStmts, valExpr) <- compileANFExprWithStmts ctx val
  bodyStmts <- compileANFStmtCtx ctx1 body
  -- Include hoisted statements, then the assignment, then body
  pure $ valStmts ++ [yulAssign (varName var) valExpr] ++ bodyStmts

compileANFStmtCtx ctx (AConCase _ sc alts mdef) = do
  let scExpr = compileAVar sc
  cases <- traverse (compileConAlt scExpr) alts
  defStmt <- case mdef of
    Just d => do
      stmts <- compileANFStmtCtx ctx d
      pure $ Just (YBlock stmts)
    Nothing => pure Nothing
  pure [YSwitch (readTag scExpr) cases defStmt]
  where
    -- Generate field extraction for constructor arguments
    extractFields : YulExpr -> List Int -> List YulStmt
    extractFields ptr args =
      zipWith (\idx, var => yulAssign (varName var) (readField ptr idx))
              (take (length args) [0..]) args

    compileConAlt : YulExpr -> AConAlt -> Core YulCase
    compileConAlt scPtr (MkAConAlt n ci (Just tag) args body) = do
      let fieldExtracts = extractFields scPtr args
      bodyStmts <- compileANFStmtCtx ctx body
      pure $ MkCase (YulNum (cast tag)) (YBlock (fieldExtracts ++ bodyStmts))
    compileConAlt scPtr (MkAConAlt n ci Nothing args body) = do
      let fieldExtracts = extractFields scPtr args
      bodyStmts <- compileANFStmtCtx ctx body
      pure $ MkCase (YulNum 0) (YBlock (fieldExtracts ++ bodyStmts))

compileANFStmtCtx ctx (AConstCase _ sc alts mdef) = do
  let scExpr = compileAVar sc
  cases <- traverse compileConstAlt alts
  defStmt <- case mdef of
    Just d => do
      stmts <- compileANFStmtCtx ctx d
      pure $ Just (YBlock stmts)
    Nothing => pure Nothing
  pure [YSwitch scExpr cases defStmt]
  where
    constToLit : Constant -> YulLiteral
    constToLit (I x) = YulNum (cast x)
    constToLit (BI x) = YulNum x
    constToLit (B8 x) = YulNum (cast x)
    constToLit (B16 x) = YulNum (cast x)
    constToLit (B32 x) = YulNum (cast x)
    constToLit (B64 x) = YulNum (cast x)
    constToLit (Ch c) = YulNum (cast (ord c))
    constToLit _ = YulNum 0

    compileConstAlt : AConstAlt -> Core YulCase
    compileConstAlt (MkAConstAlt c body) = do
      bodyStmts <- compileANFStmtCtx ctx body
      pure $ MkCase (constToLit c) (YBlock bodyStmts)

compileANFStmtCtx ctx anf = do
  -- Use compileANFExprWithStmts to capture any nested let bindings
  (_, stmts, expr) <- compileANFExprWithStmts ctx anf
  -- Include hoisted statements, then the final expression/assignment
  let finalStmt = case expr of
        YCall fname _ =>
          if isVoidOpcode fname
            then YExprStmt expr
            else yulAssign "result" expr
        _ => yulAssign "result" expr
  pure $ stmts ++ [finalStmt]
  where
    isVoidOpcode : String -> Bool
    isVoidOpcode "revert" = True
    isVoidOpcode "return" = True
    isVoidOpcode "stop" = True
    isVoidOpcode "selfdestruct" = True
    isVoidOpcode "log0" = True
    isVoidOpcode "log1" = True
    isVoidOpcode "log2" = True
    isVoidOpcode "log3" = True
    isVoidOpcode "log4" = True
    isVoidOpcode "sstore" = True
    isVoidOpcode "mstore" = True
    isVoidOpcode "mstore8" = True
    isVoidOpcode _ = False

||| Compile ANF to Yul statement (for let bindings and cases)
compileANFStmtWithArities : FnArityMap -> ANF -> Core (List YulStmt)
compileANFStmtWithArities arities = compileANFStmtCtx (MkCompileCtx arities empty 0)

||| Legacy wrapper
export
compileANFStmt : ANF -> Core (List YulStmt)
compileANFStmt = compileANFStmtWithArities empty

-- =============================================================================
-- Definition Compilation
-- =============================================================================

||| Get variable index from AVar
avarIdx : AVar -> Int
avarIdx (ALocal i) = i
avarIdx ANull = -1

||| Find the maximum variable index used in an ANF expression
maxVarIndex : ANF -> Int
maxVarIndex (AV _ v) = avarIdx v
maxVarIndex (AAppName _ _ _ args) = foldl max (-1) (map avarIdx args)
maxVarIndex (AUnderApp _ _ _ args) = foldl max (-1) (map avarIdx args)
maxVarIndex (AApp _ _ c a) = max (avarIdx c) (avarIdx a)
maxVarIndex (ACon _ _ _ _ args) = foldl max (-1) (map avarIdx args)
maxVarIndex (AOp _ _ _ args) = foldl max (-1) (toList $ map avarIdx args)
maxVarIndex (AExtPrim _ _ _ args) = foldl max (-1) (map avarIdx args)
maxVarIndex (APrimVal _ _) = -1
maxVarIndex (AErased _) = -1
maxVarIndex (ACrash _ _) = -1
maxVarIndex (ALet _ var val body) = max var (max (maxVarIndex val) (maxVarIndex body))
maxVarIndex (AConCase _ sc alts def) =
  let scIdx = avarIdx sc
      altMax = foldl max (-1) (map altIdx alts)
      defMax = maybe (-1) maxVarIndex def
  in max scIdx (max altMax defMax)
  where
    altIdx : AConAlt -> Int
    altIdx (MkAConAlt _ _ _ args body) = max (foldl max (-1) args) (maxVarIndex body)
maxVarIndex (AConstCase _ sc alts def) =
  let scIdx = avarIdx sc
      altMax = foldl max (-1) (map constAltIdx alts)
      defMax = maybe (-1) maxVarIndex def
  in max scIdx (max altMax defMax)
  where
    constAltIdx : AConstAlt -> Int
    constAltIdx (MkAConstAlt _ body) = maxVarIndex body

||| Initialize undefined variables (those used but not in params or let-bound)
initUndefinedVars : List Int -> ANF -> List YulStmt
initUndefinedVars params body =
  let maxIdx = maxVarIndex body
      paramSet = SortedSet.fromList params
      allVars = if maxIdx >= 0 then [0..maxIdx] else []
      undefined = filter (\i => i >= 0 && not (contains i paramSet)) allVars
  in map (\i => yulLet (varName i) (yulNum 0)) undefined

||| Compile an ANF definition to a Yul function (with context)
compileDefCtx : CompileCtx -> (Name, ANFDef) -> Core (Maybe YulFun)
compileDefCtx ctx (n, MkAFun args body) = do
  let initStmts = initUndefinedVars args body
  bodyStmts <- compileANFStmtCtx ctx body
  -- The body already assigns to 'result' via compileANFStmt, no need to append extra assignment
  pure $ Just $ MkYulFun
    { name = mangleName n
    , params = map varName args
    , returns = ["result"]
    , body = YBlock (initStmts ++ bodyStmts)
    , sourceLoc = nameToSourceLoc n
    }

compileDefCtx ctx (n, MkACon tag arity nt) = do
  -- Constructor: generate allocation function
  let argNames = map (\i => "arg" ++ show i) (take arity [0..])
  pure $ Just $ MkYulFun
    { name = mangleName n
    , params = argNames
    , returns = ["ptr"]
    , body = YBlock $
        allocWords (cast $ 1 + arity) ++
        [YExprStmt $ mstore (yulVar "ptr") (yulNum (cast (fromMaybe 0 tag)))] ++
        zipWith (\i, arg => YExprStmt $ mstore
          (yulCall "add" [yulVar "ptr", yulNum ((i + 1) * 32)])
          (yulVar arg)) (take (length argNames) [0..]) argNames
    , sourceLoc = nameToSourceLoc n
    }

compileDefCtx ctx (n, MkAForeign ccs fargs ret) = do
  -- Foreign function: generate FFI wrapper
  -- ccs contains the %foreign annotations, e.g. ["evm:sload"]
  let ccStr = fromMaybe "" (head' ccs)
  -- Try direct opcode extraction if parseEvmForeign fails
  let mbOp = parseEvmForeign ccStr <|> extractEvmOp ccStr
  let argNames = map (\i => "arg" ++ show i) (take (length fargs) [0..])
  -- Determine if the opcode returns a value
  let hasReturn = case ret of
                    CFUnit => False
                    CFIORes CFUnit => False
                    _ => True
  -- All functions return "result" for consistency - void functions return 0
  case mbOp of
    Just ("returnOrRevert", _) => do
      -- Special compound opcode: returnOrRevert(success, offset, length)
      -- Expands to: switch success { case 1: return(off,len) default: revert(off,len) }
      pure $ Just $ MkYulFun
        { name = mangleName n
        , params = argNames
        , returns = ["result"]
        , body = YBlock
            [ YSwitch (yulVar "arg0")  -- success
                [ MkCase (YulNum 1) (YBlock [YExprStmt $ yulCall "return" [yulVar "arg1", yulVar "arg2"]])
                ]
                (Just (YBlock [YExprStmt $ yulCall "revert" [yulVar "arg1", yulVar "arg2"]]))
            , yulAssign "result" (yulNum 0)  -- Never reached
            ]
        , sourceLoc = nameToSourceLoc n
        }
    Just (op, _) => do
      pure $ Just $ MkYulFun
        { name = mangleName n
        , params = argNames
        , returns = ["result"]  -- Always return result
        , body = YBlock $
            if hasReturn
              then [yulAssign "result" (evmOpcodeToYul op (map yulVar argNames))]
              else [YExprStmt (evmOpcodeToYul op (map yulVar argNames))
                   , yulAssign "result" (yulNum 0)]  -- Return 0 for void
        , sourceLoc = nameToSourceLoc n
        }
    Nothing =>
      -- Try to extract opcode from function name if foreign spec failed
      -- Look for patterns like prim__sload -> sload
      let fnName = show n
          mbBuiltin = extractBuiltinOp fnName
      in case mbBuiltin of
        Just op =>
          pure $ Just $ MkYulFun
            { name = mangleName n
            , params = argNames
            , returns = ["result"]  -- Always return result
            , body = YBlock $
                if hasReturn
                  then [yulAssign "result" (evmOpcodeToYul op (map yulVar argNames))]
                  else [YExprStmt (evmOpcodeToYul op (map yulVar argNames))
                       , yulAssign "result" (yulNum 0)]  -- Return 0 for void
            , sourceLoc = nameToSourceLoc n
            }
        Nothing =>
          -- Generate a no-op stub for unrecognized foreign (e.g., putStr, putStrLn)
          -- These are IO primitives that don't exist in EVM; just return 0
          pure $ Just $ MkYulFun
            { name = mangleName n
            , params = argNames
            , returns = ["result"]
            , body = YBlock [yulAssign "result" (yulNum 0)]
            , sourceLoc = nameToSourceLoc n
            }
  where
    -- List of valid EVM opcodes that can be extracted from prim__ names
    isEvmOpcode : String -> Bool
    isEvmOpcode "sload" = True
    isEvmOpcode "sstore" = True
    isEvmOpcode "mload" = True
    isEvmOpcode "mstore" = True
    isEvmOpcode "mstore8" = True
    isEvmOpcode "calldataload" = True
    isEvmOpcode "calldatasize" = True
    isEvmOpcode "calldatacopy" = True
    isEvmOpcode "returndatasize" = True
    isEvmOpcode "returndatacopy" = True
    isEvmOpcode "return" = True
    isEvmOpcode "revert" = True
    isEvmOpcode "stop" = True
    isEvmOpcode "caller" = True
    isEvmOpcode "callvalue" = True
    isEvmOpcode "origin" = True
    isEvmOpcode "address" = True
    isEvmOpcode "balance" = True
    isEvmOpcode "selfbalance" = True
    isEvmOpcode "timestamp" = True
    isEvmOpcode "number" = True
    isEvmOpcode "chainid" = True
    isEvmOpcode "gas" = True
    isEvmOpcode "gasprice" = True
    isEvmOpcode "keccak256" = True
    isEvmOpcode "log0" = True
    isEvmOpcode "log1" = True
    isEvmOpcode "log2" = True
    isEvmOpcode "log3" = True
    isEvmOpcode "log4" = True
    isEvmOpcode "call" = True
    isEvmOpcode "delegatecall" = True
    isEvmOpcode "staticcall" = True
    isEvmOpcode "create" = True
    isEvmOpcode "create2" = True
    isEvmOpcode "returnOrRevert" = True
    isEvmOpcode _ = False

    extractBuiltinOp : String -> Maybe String
    extractBuiltinOp name =
      -- Extract opcode from names like "Main.prim__sload" or "prim__sload"
      let parts = Data.List1.forget $ Data.String.split (== '_') name
          filtered = filter (\p => p /= "" && p /= "prim") parts
      in case filtered of
           [] => Nothing
           (x :: xs) =>
             let op = fromMaybe x (last' xs)
             in if isEvmOpcode op then Just op else Nothing

compileDefCtx ctx (n, MkAError exp) = do
  -- Error case: generate revert
  pure $ Just $ MkYulFun
    { name = mangleName n
    , params = []
    , returns = []
    , body = YBlock [YExprStmt $ yulCall "revert" [yulNum 0, yulNum 0]]
    , sourceLoc = nameToSourceLoc n
    }

||| Compile an ANF definition to a Yul function (with arity map)
compileDefWithArities : FnArityMap -> (Name, ANFDef) -> Core (Maybe YulFun)
compileDefWithArities arities = compileDefCtx (MkCompileCtx arities empty 0)

||| Compile an ANF definition to a Yul function
export
compileDef : (Name, ANFDef) -> Core (Maybe YulFun)
compileDef = compileDefWithArities empty

-- =============================================================================
-- Main Code Generation
-- =============================================================================

||| Find the main function from definitions
findMain : List (Name, ANFDef) -> Maybe Name
findMain [] = Nothing
findMain ((n, MkAFun _ _) :: rest) =
  case n of
    NS _ (UN (Basic "main")) => Just n
    UN (Basic "main") => Just n
    _ => findMain rest
findMain (_ :: rest) = findMain rest

||| Deduplicate functions by name (keep first occurrence)
deduplicateFuns : List YulFun -> List YulFun
deduplicateFuns funs = go SortedSet.empty funs
  where
    go : SortedSet String -> List YulFun -> List YulFun
    go seen [] = []
    go seen (f :: fs) =
      if contains f.name seen
        then go seen fs  -- Skip duplicate
        else f :: go (insert f.name seen) fs

||| Generate complete Yul object from ANF definitions
||| Creates proper EVM contract structure with constructor and runtime
export
generateYul : String -> List (Name, ANFDef) -> Core YulObject
generateYul name defs = do
  -- First pass: collect all function arities and closure targets
  let arities = collectArities defs
  let closureTargets = collectClosureTargets defs
  let closureIds = buildClosureIdMap closureTargets
  let ctx = MkCompileCtx arities closureIds 0
  -- Second pass: compile with context
  allFuns <- catMaybes <$> traverse (compileDefCtx ctx) defs
  -- Deduplicate functions (ANF may produce duplicates from typeclass instances)
  let dedupedFuns = deduplicateFuns allFuns
  let mainFn = fromMaybe (UN (Basic "main")) (findMain defs)
  let mainName = mangleName mainFn
  -- Generate closure support functions
  let closureFuns = mkClosureFunction :: applyClosureFunction closureIds arities :: []
  -- Add built-in functions
  let builtinFuns = allocatorFunction :: closureFuns
  let allWithBuiltins = builtinFuns ++ dedupedFuns
  -- Dead code elimination: only keep functions reachable from main
  let reachableFuns = eliminateDeadCode [mainName] allWithBuiltins

  -- Create runtime object (the actual contract logic)
  let runtimeObject = MkYulObject
        { name = "runtime"
        , code = initMemory ++ [YExprStmt $ yulCall "pop" [yulCall mainName [yulNum 0]]]
        , functions = reachableFuns
        , subObjects = []
        }

  -- Constructor: copy runtime code to memory and return it
  -- datacopy(0, dataoffset("runtime"), datasize("runtime"))
  -- return(0, datasize("runtime"))
  let constructorCode =
        [ YExprStmt $ yulCall "datacopy"
            [ yulNum 0
            , yulCall "dataoffset" [YLit (YulStr "runtime")]
            , yulCall "datasize" [YLit (YulStr "runtime")]
            ]
        , YExprStmt $ yulCall "return"
            [ yulNum 0
            , yulCall "datasize" [YLit (YulStr "runtime")]
            ]
        ]

  pure $ MkYulObject
    { name = name
    , code = constructorCode
    , functions = []  -- No functions in constructor
    , subObjects = [runtimeObject]
    }
  where
    ||| Create a closure in memory
    ||| Layout: [funcId, arity, arg0, arg1, ...]
    ||| For simplicity, we allocate a fixed-size closure (funcId + arity + up to 8 args)
    mkClosureFunction : YulFun
    mkClosureFunction = MkYulFun
      { name = "mk_closure"
      , params = ["func_id", "arity", "arg0", "arg1", "arg2", "arg3"]
      , returns = ["ptr"]
      , body = YBlock
          [ yulAssign "ptr" (mload (yulNum FREE_MEM_PTR))
          , YExprStmt $ mstore (yulNum FREE_MEM_PTR)
              (yulCall "add" [yulVar "ptr", yulNum (6 * 32)])  -- 6 words
          , YExprStmt $ mstore (yulVar "ptr") (yulVar "func_id")
          , YExprStmt $ mstore (yulCall "add" [yulVar "ptr", yulNum 32]) (yulVar "arity")
          , YExprStmt $ mstore (yulCall "add" [yulVar "ptr", yulNum 64]) (yulVar "arg0")
          , YExprStmt $ mstore (yulCall "add" [yulVar "ptr", yulNum 96]) (yulVar "arg1")
          , YExprStmt $ mstore (yulCall "add" [yulVar "ptr", yulNum 128]) (yulVar "arg2")
          , YExprStmt $ mstore (yulCall "add" [yulVar "ptr", yulNum 160]) (yulVar "arg3")
          ]
      , sourceLoc = Nothing  -- Runtime helper, no source location
      }

    ||| Generate dispatch case for a single closure target
    mkDispatchCase : FnArityMap -> (String, Integer) -> YulCase
    mkDispatchCase arities (fnName, closureId) =
      let arity = fromMaybe 1 (lookup fnName arities)
          -- Generate call with captured args from closure + new arg
          -- Closure layout: [funcId, arity, arg0, arg1, arg2, arg3]
          capturedArgs = map (\i => mload (yulCall "add" [yulVar "closure", yulNum ((i + 2) * 32)]))
                             (take arity [0..])
          -- The new arg replaces the last captured slot based on remaining arity
          callArgs = capturedArgs ++ [yulVar "arg"]
      in MkCase (YulNum closureId) (YBlock
          -- For arity 1: call function directly with all args
          -- For arity > 1: create new closure with arity - 1
          [ YIf (yulCall "eq" [mload (yulCall "add" [yulVar "closure", yulNum 32]), yulNum 1])
              (YBlock
                [ yulAssign "result" (yulCall fnName (take (cast arity) callArgs))
                , YLeave
                ])
          , yulAssign "result" (yulCall "mk_closure"
              [ mload (yulVar "closure")  -- same func_id
              , yulCall "sub" [mload (yulCall "add" [yulVar "closure", yulNum 32]), yulNum 1]  -- arity - 1
              , mload (yulCall "add" [yulVar "closure", yulNum 64])   -- existing arg0
              , mload (yulCall "add" [yulVar "closure", yulNum 96])   -- existing arg1
              , mload (yulCall "add" [yulVar "closure", yulNum 128])  -- existing arg2
              , yulVar "arg"  -- new arg becomes arg3
              ])
          ])

    ||| Apply closure: dispatch based on function ID
    applyClosureFunction : ClosureIdMap -> FnArityMap -> YulFun
    applyClosureFunction closureIdMap arities =
      let entries = SortedMap.toList closureIdMap
          cases = map (mkDispatchCase arities) entries
      in MkYulFun
        { name = "apply_closure"
        , params = ["closure", "arg"]
        , returns = ["result"]
        , body = YBlock
            [ yulLet "func_id" (mload (yulVar "closure"))
            , YSwitch (yulVar "func_id") cases
                (Just $ YBlock [yulAssign "result" (yulNum 0)])  -- default: return 0
            ]
        , sourceLoc = Nothing  -- Runtime helper, no source location
        }

||| Write Yul output to file
export
writeYulFile : String -> YulObject -> Core ()
writeYulFile path obj = do
  let content = showObject obj
  Right () <- coreLift $ writeFile path content
    | Left err => throw (FileErr path err)
  pure ()
