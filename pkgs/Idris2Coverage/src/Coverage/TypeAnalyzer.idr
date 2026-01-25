||| Type analysis using Idris2 TTImp output
||| Extracts type information from --dump-tt output
module Coverage.TypeAnalyzer

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System
import System.File

import Coverage.Types
import Coverage.Linearity

%default total

-- =============================================================================
-- String Utilities
-- =============================================================================

||| Drop first n characters from a string
strDrop : Nat -> String -> String
strDrop n s = substr n (length s `minus` n) s

||| Safe last element of a list
export
safeLast : List a -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_ :: xs) = safeLast xs

||| Safe init (all but last) of a list
export
safeInit : List a -> List a
safeInit [] = []
safeInit [_] = []
safeInit (x :: xs) = x :: safeInit xs

-- =============================================================================
-- Idris2 Output Parsing
-- =============================================================================

||| Result of running idris2 --dump-tt
public export
record TTDump where
  constructor MkTTDump
  moduleName : String
  functions  : List (String, String)    -- (name, signature)
  dataTypes  : List (String, List String)  -- (name, constructors)

||| Parse a function signature line from TTImp output
||| e.g., "add : Int -> Int -> Int"
export
parseFunctionSig : String -> Maybe (String, String)
parseFunctionSig line =
  let trimmed = trim line
      parts = break (== ':') trimmed
  in case parts of
    (name, rest) =>
      if length rest > 1
        then Just (trim name, trim $ strDrop 1 rest)
        else Nothing

||| Parse a data type definition
||| e.g., "data Bool = True | False"
export
parseDataType : String -> Maybe (String, List String)
parseDataType line =
  let trimmed = trim line
  in if isPrefixOf "data " trimmed
     then let rest = strDrop 5 trimmed
              parts = break (== '=') rest
          in case parts of
               (name, ctors) =>
                 if length ctors > 1
                   then let ctorList = map trim $ forget $ split (== '|') (strDrop 1 ctors)
                        in Just (trim name, ctorList)
                   else Nothing
     else Nothing

-- =============================================================================
-- Signature Parsing
-- =============================================================================

||| Parse function parameters from signature
||| "Int -> Bool -> String" -> [Int, Bool] (return type excluded)
export
parseParams : String -> List String
parseParams sig =
  let parts = map trim $ forget $ split (== '-') sig
      -- Filter out empty and ">" parts, recombine arrows
      cleaned = recombineArrows parts
  in safeInit cleaned
  where
    recombineArrows : List String -> List String
    recombineArrows [] = []
    recombineArrows [x] = [x]
    recombineArrows (x :: ">" :: rest) = recombineArrows rest
    recombineArrows (x :: rest) =
      if x == ">" then recombineArrows rest
      else x :: recombineArrows rest

||| Parse a single parameter with potential linearity
||| "(1 x : A)" -> LinearParam with Q1
||| "A" -> LinearParam with QW (unrestricted)
export
parseParamWithLinearity : String -> LinearParam
parseParamWithLinearity s =
  let trimmed = trim s
  in case parseLinearParam trimmed of
    Just lp => lp
    Nothing =>
      -- Simple type without annotation
      MkLinearParam Nothing trimmed QW Nothing

||| Extract all parameters with linearity from signature
export
extractLinearParams : String -> List LinearParam
extractLinearParams sig =
  map parseParamWithLinearity (parseParams sig)

-- =============================================================================
-- Type Information Extraction
-- =============================================================================

||| Known primitive types with their state counts
export
primitiveStateCount : String -> Maybe StateCount
primitiveStateCount "Bool"    = Just $ Finite 2
primitiveStateCount "Ordering"= Just $ Finite 3
primitiveStateCount "()"      = Just $ Finite 1
primitiveStateCount "Unit"    = Just $ Finite 1
primitiveStateCount "Void"    = Just $ Finite 0
primitiveStateCount "Nat"     = Just $ Bounded 3   -- 0, 1, n
primitiveStateCount "Int"     = Just $ Bounded 3   -- negative, 0, positive
primitiveStateCount "Integer" = Just $ Bounded 3
primitiveStateCount "Double"  = Just $ Bounded 4   -- negative, 0, positive, NaN
primitiveStateCount "Char"    = Just $ Bounded 3   -- typical char, boundary, special
primitiveStateCount "String"  = Just $ Bounded 3   -- empty, single, multi
primitiveStateCount _         = Nothing

||| Analyze Maybe type: Nothing + Just variants
export
analyzeMaybe : String -> TypeInfo -> TypeInfo
analyzeMaybe innerType innerInfo =
  let nothingCtor = MkConstructorInfo "Nothing" []
      justCtor = MkConstructorInfo "Just" [(innerType, QW)]
      -- Maybe A = 1 + |A|
      newCount = addStateCount (Finite 1) innerInfo.stateCount
  in MkTypeInfo
       ("Maybe " ++ innerType)
       [nothingCtor, justCtor]
       innerInfo.isRecursive
       (TCBounded 0)  -- Will be computed
       newCount

||| Analyze Either type: Left + Right variants
export
analyzeEither : String -> String -> TypeInfo -> TypeInfo -> TypeInfo
analyzeEither leftType rightType leftInfo rightInfo =
  let leftCtor = MkConstructorInfo "Left" [(leftType, QW)]
      rightCtor = MkConstructorInfo "Right" [(rightType, QW)]
      -- Either A B = |A| + |B|
      newCount = addStateCount leftInfo.stateCount rightInfo.stateCount
  in MkTypeInfo
       ("Either " ++ leftType ++ " " ++ rightType)
       [leftCtor, rightCtor]
       (leftInfo.isRecursive || rightInfo.isRecursive)
       (TCBounded 0)
       newCount

||| Analyze List type (bounded depth)
export
analyzeList : String -> TypeInfo -> Nat -> TypeInfo
analyzeList elemType elemInfo maxDepth =
  let nilCtor = MkConstructorInfo "Nil" []
      consCtor = MkConstructorInfo "::" [(elemType, QW), ("List " ++ elemType, QW)]
      -- List with bounded depth: 1 + |A| + |A|^2 + ... bounded
      -- Simplified: empty, singleton, multi
      newCount = Bounded 3
  in MkTypeInfo
       ("List " ++ elemType)
       [nilCtor, consCtor]
       True  -- List is recursive
       (TCRecursive maxDepth)
       newCount

||| Analyze a pair/tuple type
export
analyzePair : String -> String -> TypeInfo -> TypeInfo -> TypeInfo
analyzePair aType bType aInfo bInfo =
  let pairCtor = MkConstructorInfo "MkPair" [(aType, QW), (bType, QW)]
      -- (A, B) = |A| * |B|
      newCount = multStateCount aInfo.stateCount bInfo.stateCount
  in MkTypeInfo
       ("(" ++ aType ++ ", " ++ bType ++ ")")
       [pairCtor]
       (aInfo.isRecursive || bInfo.isRecursive)
       (TCBounded 0)
       newCount

-- =============================================================================
-- Type Resolution
-- =============================================================================

||| Create TypeInfo from type name (basic resolution)
||| Uses bounded recursion to ensure termination
export
covering
resolveType : String -> TypeInfo
resolveType typeName =
  case primitiveStateCount typeName of
    Just sc => MkTypeInfo typeName [] False (TCFinite 0) sc
    Nothing =>
      -- Check for compound types
      if isPrefixOf "Maybe " typeName
        then let inner = strDrop 6 typeName
                 innerInfo = resolveType inner
             in analyzeMaybe inner innerInfo
      else if isPrefixOf "Either " typeName
        then -- Simplified: assume both sides unknown
             MkTypeInfo typeName [] False TCUnbounded Unbounded
      else if isPrefixOf "List " typeName
        then let inner = strDrop 5 typeName
                 innerInfo = resolveType inner
             in analyzeList inner innerInfo 3
      else if isPrefixOf "(" typeName && isSuffixOf ")" typeName
        then -- Tuple - simplified
             MkTypeInfo typeName [] False TCUnbounded Unbounded
      else
        -- Unknown type, assume bounded with equivalence classes
        MkTypeInfo typeName [] False (TCBounded 3) (Bounded 3)

||| Resolve all parameters in a list
export
covering
resolveParams : List LinearParam -> List LinearParam
resolveParams = map resolveParam
  where
    resolveParam : LinearParam -> LinearParam
    resolveParam p =
      { typeInfo := Just (resolveType p.paramType) } p

-- =============================================================================
-- Function Analysis
-- =============================================================================

||| Analyzed function with full type information
public export
record AnalyzedFunction where
  constructor MkAnalyzedFunction
  name       : String
  signature  : String
  params     : List LinearParam
  returnType : String
  stateSpace : StateCount

export
Show AnalyzedFunction where
  show f = f.name ++ " : " ++ f.signature ++ " [" ++ show f.stateSpace ++ " cases]"

||| Analyze a function from its signature
export
covering
analyzeFunction : String -> String -> AnalyzedFunction
analyzeFunction name sig =
  let rawParams = extractLinearParams sig
      resolvedParams = resolveParams rawParams
      paramParts = map trim $ forget $ split (== '-') sig
      -- Extract return type (last part after ->)
      returnType = fromMaybe "Unknown" $ safeLast $
                     filter (\s => not (s == ">") && length s > 0) paramParts
      -- Calculate state space
      stateSpace = effectiveStateSpace resolvedParams
  in MkAnalyzedFunction name sig resolvedParams returnType stateSpace

-- =============================================================================
-- Module Analysis
-- =============================================================================

||| Result of analyzing a module
public export
record ModuleAnalysis where
  constructor MkModuleAnalysis
  moduleName : String
  functions  : List AnalyzedFunction
  dataTypes  : List TypeInfo

export
Show ModuleAnalysis where
  show m = "Module " ++ m.moduleName ++ " (" ++ show (length m.functions) ++ " functions)"

||| Parse TTImp dump output (simplified)
export
parseTTDump : String -> TTDump
parseTTDump content =
  let ls = lines content
      funcs = mapMaybe parseFunctionSig ls
      types = mapMaybe parseDataType ls
  in MkTTDump "" funcs types

||| Analyze all functions in a TTDump
export
covering
analyzeModule : String -> TTDump -> ModuleAnalysis
analyzeModule modName dump =
  let funcs = map (\(n, s) => analyzeFunction n s) dump.functions
      types = map (\(n, ctors) => MkTypeInfo n [] False TCUnbounded Unbounded) dump.dataTypes
  in MkModuleAnalysis modName funcs types
