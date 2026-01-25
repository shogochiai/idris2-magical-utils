||| Profile data collector from Chez Scheme profiler output
||| REQ_COV_COL_001 - REQ_COV_COL_004
module Coverage.Collector

import Coverage.Types
import Coverage.DumpcasesParser
import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System.File

%default total

-- =============================================================================
-- Helper Functions
-- =============================================================================

||| Get first character of string, or Nothing if empty
firstChar : String -> Maybe Char
firstChar s = case unpack s of
                [] => Nothing
                (c :: _) => Just c

||| Index into a list safely
indexList : List a -> Nat -> Maybe a
indexList [] _ = Nothing
indexList (x :: _) Z = Just x
indexList (_ :: xs) (S n) = indexList xs n

||| Zip a list with indices starting from 1
zipWithIndex : List a -> List (Nat, a)
zipWithIndex = go 1
  where
    go : Nat -> List a -> List (Nat, a)
    go _ [] = []
    go n (x :: xs) = (n, x) :: go (S n) xs

-- =============================================================================
-- Scheme Source Parsing
-- =============================================================================

||| Parse (define ModuleName-funcName ...) from .ss file
||| Returns list of (schemeFunc, lineNumber)
export
parseSchemeDefs : String -> List (String, Nat)
parseSchemeDefs content =
  let ls = zipWithIndex (lines content)
  in mapMaybe parseLine ls
  where
    parseLine : (Nat, String) -> Maybe (String, Nat)
    parseLine (lineNum, line) =
      if isPrefixOf "(define " (trim line)
         then
           let afterDefine = pack $ drop 8 $ unpack (trim line)
               funcName = pack $ takeWhile (\c => c /= ' ' && c /= '(') (unpack afterDefine)
           in if funcName /= "" && isInfixOf "-" funcName
                 then Just (funcName, lineNum)
                 else Nothing
         else Nothing

-- =============================================================================
-- Profile HTML Parsing
-- =============================================================================

||| Parse hot spots from profile.html content
||| REQ_COV_COL_001: Parse profile.html Hot Spots table
||| REQ_COV_COL_002: Extract function name and hit count
export
parseProfileHtml : String -> List ProfileHit
parseProfileHtml content =
  -- Hot Spots are in table rows with class pc1-pc12
  let ls = lines content
      hotSpotLines = filter isHotSpotLine ls
  in mapMaybe parseHotSpotRow hotSpotLines
  where
    isHotSpotLine : String -> Bool
    isHotSpotLine line = isInfixOf "pc" line && isInfixOf "line" line && isInfixOf "(" line

    parseParenCount : String -> Maybe Nat
    parseParenCount s =
      let stripped = pack $ filter (\c => c >= '0' && c <= '9') (unpack s)
      in parsePositive stripped

    parseHotSpotRow : String -> Maybe ProfileHit
    parseHotSpotRow line =
      -- Extract: line number and count from "line N (count)"
      let parts = words line
      in findLineCount parts
      where
        findLineCount : List String -> Maybe ProfileHit
        findLineCount [] = Nothing
        findLineCount (x :: xs) =
          if x == "line"
             then case xs of
                    (numStr :: rest) =>
                      case parsePositive numStr of
                        Nothing => findLineCount xs
                        Just lineNum =>
                          let countStr = fromMaybe "" $ head' rest
                              count = parseParenCount countStr
                          in case count of
                               Nothing => findLineCount xs
                               Just c => Just $ MkProfileHit "" c "" lineNum
                    _ => findLineCount xs
             else findLineCount xs

-- =============================================================================
-- Annotated Scheme HTML Parsing (.ss.html)
-- =============================================================================

||| Expression-level coverage from annotated .ss.html file
||| Format: <span class=pcN title="line L char C count N">...</span>
public export
record ExprCoverage where
  constructor MkExprCoverage
  line : Nat
  char : Nat
  count : Nat

||| Parse title="line L char C count N"
parseSpanTitle : String -> Maybe ExprCoverage
parseSpanTitle s =
  -- title="line 747 char 77 count 6"
  let parts = words s
  in findParts parts Nothing Nothing Nothing
  where
    findParts : List String -> Maybe Nat -> Maybe Nat -> Maybe Nat -> Maybe ExprCoverage
    findParts [] (Just l) (Just c) (Just cnt) = Just $ MkExprCoverage l c cnt
    findParts [] _ _ _ = Nothing
    findParts (x :: xs) ml mc mcnt =
      if x == "line"
         then case xs of
                (n :: rest) => case parsePositive n of
                                 Just ln => findParts rest (Just ln) mc mcnt
                                 Nothing => findParts xs ml mc mcnt
                _ => findParts xs ml mc mcnt
         else if x == "char"
         then case xs of
                (n :: rest) => case parsePositive n of
                                 Just ch => findParts rest ml (Just ch) mcnt
                                 Nothing => findParts xs ml mc mcnt
                _ => findParts xs ml mc mcnt
         else if x == "count"
         then case xs of
                (n :: rest) => case parsePositive n of
                                 Just ct => findParts rest ml mc (Just ct)
                                 Nothing => findParts xs ml mc mcnt
                _ => findParts xs ml mc mcnt
         else findParts xs ml mc mcnt

||| Extract all title="..." attributes from HTML
||| Note: Uses assert_total for recursion on String parsing
export
extractTitles : String -> List String
extractTitles content = go (unpack content) []
  where
    go : List Char -> List String -> List String
    go [] acc = reverse acc
    go ('t' :: 'i' :: 't' :: 'l' :: 'e' :: '=' :: '"' :: rest) acc =
      let (title, remaining) = break (== '"') rest
      in assert_total $ go remaining (pack title :: acc)
    go (_ :: rest) acc = assert_total $ go rest acc

||| Parse annotated .ss.html for expression-level coverage
export
parseAnnotatedHtml : String -> List ExprCoverage
parseAnnotatedHtml content =
  let titles = extractTitles content
  in mapMaybe parseSpanTitle titles

||| Group expressions by Scheme line and calculate coverage
||| Returns: List (schemeLineNum, executedExprs, totalExprs)
export
groupByLine : List ExprCoverage -> List (Nat, Nat, Nat)
groupByLine exprs =
  let sorted = sortBy (\a, b => compare a.line b.line) exprs
      grouped = groupBy (\a, b => a.line == b.line) sorted
  in map summarize grouped
  where
    summarize : List1 ExprCoverage -> (Nat, Nat, Nat)
    summarize (e ::: es) =
      let expList = e :: es
      in (e.line, length (filter (\x => x.count > 0) expList), length expList)

||| Calculate function-level coverage from expression data
||| Given: function definitions (name, startLine) and expression coverage
export
calculateFunctionCoverage : List (String, Nat) -> List ExprCoverage -> List (String, Nat, Nat, Double)
calculateFunctionCoverage defs exprs =
  -- Sort definitions by line number
  let sortedDefs = sortBy (\(_, l1), (_, l2) => compare l1 l2) defs
  in zipWithRanges sortedDefs
  where
    -- Pair each function with its line range
    calcPct : List ExprCoverage -> (Nat, Nat, Double)
    calcPct funcExprs =
      let totl = length funcExprs
          exec = length (filter (\e => e.count > 0) funcExprs)
          pct = if totl == 0 then 100.0 else cast exec / cast totl * 100.0
      in (exec, totl, pct)

    zipWithRanges : List (String, Nat) -> List (String, Nat, Nat, Double)
    zipWithRanges [] = []
    zipWithRanges [(name, start)] =
      -- Last function extends to end
      let funcExprs = filter (\e => e.line >= start) exprs
          result = calcPct funcExprs
      in [(name, fst result, fst (snd result), snd (snd result))]
    zipWithRanges ((name, start) :: (next, nextStart) :: rest) =
      let funcExprs = filter (\e => e.line >= start && e.line < nextStart) exprs
          result = calcPct funcExprs
      in (name, fst result, fst (snd result), snd (snd result)) :: zipWithRanges ((next, nextStart) :: rest)

-- =============================================================================
-- Combined Collection
-- =============================================================================

||| Collect profile data by matching HTML hits with Scheme definitions
||| REQ_COV_COL_003: Parse .ss file for definitions
||| REQ_COV_COL_004: Return List ProfileHit
export
collectProfile : (htmlContent : String) -> (schemeContent : String) -> List ProfileHit
collectProfile htmlContent schemeContent =
  let htmlHits = parseProfileHtml htmlContent
      schemeDefs = parseSchemeDefs schemeContent
      -- Match line numbers to function names
  in mapMaybe (matchHit schemeDefs) htmlHits
  where
    matchHit : List (String, Nat) -> ProfileHit -> Maybe ProfileHit
    matchHit defs hit =
      -- Find the definition that matches this line (or closest before)
      let matching = filter (\(_, l) => l <= hit.line) defs
      in case last' matching of
           Nothing => Just hit  -- Keep hit even without match
           Just (funcName, _) => Just $ { schemeFunc := funcName } hit

-- =============================================================================
-- Branch Coverage Parsing
-- =============================================================================

||| Raw span data from HTML (for branch analysis)
record SpanData where
  constructor MkSpanData
  line    : Nat
  char    : Nat
  count   : Nat
  content : String

||| Extract spans with their content for branch analysis
||| Format: <span class=pcN title="line L char C count N">content</span>
export
extractSpansWithContent : String -> List SpanData
extractSpansWithContent content = go (unpack content) []
  where
    parseTitle : List Char -> Maybe (Nat, Nat, Nat)
    parseTitle cs =
      let s = pack cs
          parts = words s
      in findParts parts Nothing Nothing Nothing
      where
        findParts : List String -> Maybe Nat -> Maybe Nat -> Maybe Nat -> Maybe (Nat, Nat, Nat)
        findParts [] (Just l) (Just c) (Just cnt) = Just (l, c, cnt)
        findParts [] _ _ _ = Nothing
        findParts (x :: xs) ml mc mcnt =
          if x == "line"
             then case xs of
                    (n :: rest) => case parsePositive n of
                                     Just ln => findParts rest (Just ln) mc mcnt
                                     Nothing => findParts xs ml mc mcnt
                    _ => findParts xs ml mc mcnt
             else if x == "char"
             then case xs of
                    (n :: rest) => case parsePositive n of
                                     Just ch => findParts rest ml (Just ch) mcnt
                                     Nothing => findParts xs ml mc mcnt
                    _ => findParts xs ml mc mcnt
             else if x == "count"
             then case xs of
                    (n :: rest) => case parsePositive n of
                                     Just ct => findParts rest ml mc (Just ct)
                                     Nothing => findParts xs ml mc mcnt
                    _ => findParts xs ml mc mcnt
             else findParts xs ml mc mcnt

    extractContent : List Char -> (List Char, List Char)
    extractContent cs = break (== '<') cs

    go : List Char -> List SpanData -> List SpanData
    go [] acc = reverse acc
    -- Match: title="..."
    go ('t' :: 'i' :: 't' :: 'l' :: 'e' :: '=' :: '"' :: rest) acc =
      let (titleChars, afterTitle) = break (== '"') rest
          afterQuote = drop 1 afterTitle
          -- Skip to > then extract content until </span>
          (_, afterGt) = break (== '>') afterQuote
          afterGtSkip = drop 1 afterGt
          (contentChars, remaining) = extractContent afterGtSkip
      in case parseTitle titleChars of
           Nothing => assert_total $ go remaining acc
           Just (l, c, cnt) =>
             let sd = MkSpanData l c cnt (pack contentChars)
             in assert_total $ go remaining (sd :: acc)
    go (_ :: rest) acc = assert_total $ go rest acc

||| Identify branch points from spans
||| Looks for patterns like "(if ", "(case ", "(cond "
export
identifyBranchPoints : List SpanData -> List BranchPoint
identifyBranchPoints spans =
  let -- Find all branch-starting spans
      branchStarts = filter isBranchStart spans
  in mapMaybe (analyzeBranch spans) branchStarts
  where
    isBranchStart : SpanData -> Bool
    isBranchStart sd =
      isPrefixOf "(if " sd.content
      || isPrefixOf "(case " sd.content
      || isPrefixOf "(cond " sd.content

    getBranchType : String -> BranchType
    getBranchType s =
      if isPrefixOf "(if " s then IfBranch
      else if isPrefixOf "(case " s then CaseBranch
      else CondBranch

    countBranches : BranchType -> List SpanData -> Nat -> (Nat, Nat, List (String, Nat))
    countBranches IfBranch spans startLine =
      -- For if: assume 2 branches (then/else)
      -- Look for expressions after the condition
      let afterCond = filter (\sd => sd.line >= startLine) spans
          -- Simple heuristic: count unique execution counts as branch indicator
          counts = map (\sd => sd.count) afterCond
          hasZero = any (== 0) counts
          hasNonZero = any (> 0) counts
          covered = if hasZero && hasNonZero then 1
                    else if hasNonZero then 2 else 0
      in (2, covered, [("then", if hasNonZero then 1 else 0), ("else", if hasZero then 0 else 1)])
    countBranches CaseBranch spans startLine =
      -- For case: count [pattern ...] occurrences
      let caseSpans = filter (\sd => sd.line >= startLine && isPrefixOf "[" sd.content) spans
          totNum = max 2 (length caseSpans)
          covNum = length $ filter (\sd => sd.count > 0) caseSpans
      in (totNum, covNum, map (\sd => (sd.content, sd.count)) caseSpans)
    countBranches CondBranch spans startLine =
      -- Similar to case
      let condSpans = filter (\sd => sd.line >= startLine && isPrefixOf "[" sd.content) spans
          totNum = max 2 (length condSpans)
          covNum = length $ filter (\sd => sd.count > 0) condSpans
      in (totNum, covNum, map (\sd => (sd.content, sd.count)) condSpans)

    makeBranchPoint : Nat -> Nat -> BranchType -> (Nat, Nat, List (String, Nat)) -> Maybe BranchPoint
    makeBranchPoint ln ch bt result =
      let totBranches = fst result
          covBranches = fst (snd result)
          branchDetails = snd (snd result)
      in if totBranches > 0
            then Just $ MkBranchPoint ln ch bt totBranches covBranches branchDetails
            else Nothing

    -- For an if-branch, we need to find then and else branches
    -- This is simplified: we look at spans on the same/nearby lines
    analyzeBranch : List SpanData -> SpanData -> Maybe BranchPoint
    analyzeBranch allSpans branchSpan =
      let btype = getBranchType branchSpan.content
          nearbySpans = filter (\sd => sd.line >= branchSpan.line
                                    && sd.line <= branchSpan.line + 10) allSpans
          branchResult = countBranches btype nearbySpans branchSpan.line
      in makeBranchPoint branchSpan.line branchSpan.char btype branchResult

||| Parse branch coverage from annotated HTML
export
parseBranchCoverage : String -> List BranchPoint
parseBranchCoverage htmlContent =
  let spans = extractSpansWithContent htmlContent
  in identifyBranchPoints spans

||| Calculate branch coverage summary from branch points
export
summarizeBranchCoverage : List BranchPoint -> BranchCoverageSummary
summarizeBranchCoverage bps =
  let totalPoints = length bps
      totalBranches = sum $ map (.totalBranches) bps
      coveredBranches = sum $ map (.coveredBranches) bps
      pct = if totalBranches == 0 then 100.0
            else cast coveredBranches / cast totalBranches * 100.0
      uncovered = filter (\bp => bp.coveredBranches < bp.totalBranches) bps
  in MkBranchCoverageSummary totalPoints totalBranches coveredBranches pct
       (map (\bp => ("unknown", bp)) uncovered)

||| Associate branch points with function names based on Scheme definitions
export
associateBranchesWithFunctions : List (String, Nat) -> List BranchPoint -> List (String, BranchPoint)
associateBranchesWithFunctions funcDefs bps =
  map (findFunction funcDefs) bps
  where
    -- Find the function that contains this branch point
    findFunction : List (String, Nat) -> BranchPoint -> (String, BranchPoint)
    findFunction defs bp =
      let -- Functions that start before this branch
          candidates = filter (\(_, startLine) => startLine <= bp.line) defs
          -- Sort by start line descending to get the closest one
          sorted = sortBy (\(_, l1), (_, l2) => compare l2 l1) candidates
      in case sorted of
           [] => ("unknown", bp)
           ((name, _) :: _) => (name, bp)

||| REQ_COV_AGG_005: Check if a Scheme function name belongs to a test module
||| Test modules follow pattern: *C-45TestsC-45* (corresponds to *.Tests.* in Idris)
export
isTestModule : String -> Bool
isTestModule funcName = isInfixOf "C-45Tests" funcName || isInfixOf "-Tests-" funcName

||| Filter out test module functions from function definitions
export
excludeTestModules : List (String, Nat) -> List (String, Nat)
excludeTestModules = filter (\(name, _) => not (isTestModule name))

||| Filter out branch points that belong to test modules
export
excludeTestBranchPoints : List (String, Nat) -> List BranchPoint -> List BranchPoint
excludeTestBranchPoints funcDefs bps =
  filter (isProductionBranch funcDefs) bps
  where
    -- Check if branch point belongs to a production function (not test)
    isProductionBranch : List (String, Nat) -> BranchPoint -> Bool
    isProductionBranch allFuncs bp =
      let candidates = filter (\(_, l) => l <= bp.line) allFuncs
          sorted = sortBy (\(_, l1), (_, l2) => compare l2 l1) candidates
      in case sorted of
           [] => True  -- Include if can't determine (e.g., Prelude functions)
           ((name, _) :: _) => not (isTestModule name)

||| Calculate branch coverage summary with function associations
export
summarizeBranchCoverageWithFunctions : List (String, Nat) -> List BranchPoint -> BranchCoverageSummary
summarizeBranchCoverageWithFunctions funcDefs bps =
  let totalPoints = length bps
      totalBranches = sum $ map (.totalBranches) bps
      coveredBranches = sum $ map (.coveredBranches) bps
      pct = if totalBranches == 0 then 100.0
            else cast coveredBranches / cast totalBranches * 100.0
      uncovered = filter (\bp => bp.coveredBranches < bp.totalBranches) bps
      associated = associateBranchesWithFunctions funcDefs uncovered
  in MkBranchCoverageSummary totalPoints totalBranches coveredBranches pct associated

||| REQ_COV_AGG_005: Calculate branch coverage excluding test modules
export
summarizeBranchCoverageExcludingTests : List (String, Nat) -> List BranchPoint -> BranchCoverageSummary
summarizeBranchCoverageExcludingTests funcDefs bps =
  let prodFuncs = excludeTestModules funcDefs
      prodBps = excludeTestBranchPoints funcDefs bps
  in summarizeBranchCoverageWithFunctions prodFuncs prodBps

-- =============================================================================
-- File Reading (partial - involves IO)
-- =============================================================================

||| Read profile.html and .ss file, return collected profile hits
export
covering
collectFromFiles : (htmlPath : String) -> (schemePath : String) -> IO (Either String (List ProfileHit))
collectFromFiles htmlPath schemePath = do
  Right htmlContent <- readFile htmlPath
    | Left err => pure $ Left "Failed to read \{htmlPath}: \{show err}"
  Right schemeContent <- readFile schemePath
    | Left err => pure $ Left "Failed to read \{schemePath}: \{show err}"
  pure $ Right $ collectProfile htmlContent schemeContent

-- =============================================================================
-- Chez Scheme Name Mangling (Compiler-equivalent, deterministic)
-- =============================================================================

||| Encode a single character using Chez Scheme rules
||| Alphanumeric and underscore pass through; others become C-<ord>
||| Based on Idris2 src/Compiler/Scheme/Common.idr schString
chezEncodeChar : Char -> String
chezEncodeChar c =
  if isAlphaNum c || c == '_'
     then singleton c
     else "C-" ++ show (ord c)

||| Encode a string using Chez Scheme mangling rules
||| This is equivalent to Idris2's schString function
export
chezEncodeString : String -> String
chezEncodeString s = fastConcat $ map chezEncodeChar (unpack s)

||| Convert Idris qualified function name to Chez Scheme identifier
||| "Prelude.EqOrd.==" -> "PreludeC-45EqOrd-C-61C-61"
||| "Sample.add" -> "Sample-add"
|||
||| Algorithm (from Idris2 schName):
|||   1. Split on '.' to get namespace segments + function name
|||   2. Join namespace segments with '-'
|||   3. Encode the joined namespace string (- becomes C-45)
|||   4. Append '-' and encoded function name
export
chezMangle : String -> String
chezMangle idrisName =
  let parts = forget $ split (== '.') idrisName
  in case (init' parts, last' parts) of
       (_, Nothing) => ""  -- empty string
       (Nothing, Just funcName) =>
         -- No namespace, just function name
         chezEncodeString funcName
       (Just [], Just funcName) =>
         -- No namespace, just function name
         chezEncodeString funcName
       (Just nsParts, Just funcName) =>
         -- Namespace + function name
         let nsJoined = fastConcat $ intersperse "-" nsParts
             nsEncoded = chezEncodeString nsJoined
             funcEncoded = chezEncodeString funcName
         in nsEncoded ++ "-" ++ funcEncoded

-- =============================================================================
-- FunctionRuntimeHit Matching (CLI/API Shared Logic)
-- =============================================================================

||| Convert Idris function name to Scheme function name pattern
||| DEPRECATED: Use chezMangle for deterministic matching
||| Kept for backward compatibility
export
idrisFuncToSchemePattern : String -> String
idrisFuncToSchemePattern = chezMangle

||| Match a CompiledFunction with expression coverage data
||| Returns FunctionRuntimeHit with combined static + runtime data
|||
||| @func - Static analysis from --dumpcases
||| @lineToExprs - Line-based expression coverage: (line, executedExprs, totalExprs)
||| @funcDefs - Scheme function definitions: (schemeFunc, startLine)
export
matchFunctionWithCoverage : (func : CompiledFunction)
                          -> (lineToExprs : List (Nat, Nat, Nat))
                          -> (funcDefs : List (String, Nat))
                          -> FunctionRuntimeHit
matchFunctionWithCoverage func lineToExprs funcDefs =
  let -- Find Scheme function name that matches this Idris function
      schemePattern = idrisFuncToSchemePattern func.fullName
      matchingScheme = findMatchingScheme schemePattern funcDefs
      schemeFunc = fromMaybe func.fullName matchingScheme

      -- Get expression coverage for this function's line range
      startLine = findFuncStartLine schemeFunc funcDefs
      (coveredExprs, totalExprs) = sumExprCoverage startLine lineToExprs funcDefs

      -- Count canonical cases from static analysis
      canonicalCount = countCanonical func.cases

      -- Approximate executed branches from expression coverage ratio
      -- If 80% of expressions are covered, estimate 80% of branches are covered
      executedCount : Nat
      executedCount = if totalExprs == 0 then 0
                      else let ratio : Double = cast coveredExprs / cast totalExprs
                           in cast (ratio * cast canonicalCount)

  in MkFunctionRuntimeHit
       func.fullName
       schemeFunc
       canonicalCount
       executedCount
       totalExprs
       coveredExprs
  where
    countCanonical : List CompiledCase -> Nat
    countCanonical = length . filter (\c => c.kind == Canonical)

    -- Find Scheme function name using deterministic matching
    -- Priority: 1) Exact match, 2) Suffix match (for u-- prefixed names)
    -- Soundness: Never use isInfixOf to avoid mis-attribution
    findMatchingScheme : String -> List (String, Nat) -> Maybe String
    findMatchingScheme expected [] = Nothing
    findMatchingScheme expected ((name, _) :: rest) =
      if name == expected then Just name
      else if isSuffixOf expected name then Just name  -- handles u--Main-func -> Main-func
      else findMatchingScheme expected rest

    -- Find start line for a Scheme function
    findFuncStartLine : String -> List (String, Nat) -> Nat
    findFuncStartLine target [] = 0
    findFuncStartLine target ((name, line) :: rest) =
      if name == target then line
      else findFuncStartLine target rest

    -- Find next function's start line
    findNextStart : Nat -> List (String, Nat) -> Nat
    findNextStart start [] = 999999
    findNextStart start ((_, line) :: rest) =
      if line > start then line
      else findNextStart start rest

    -- Sum expression coverage for lines belonging to this function
    sumExprCoverage : Nat -> List (Nat, Nat, Nat) -> List (String, Nat) -> (Nat, Nat)
    sumExprCoverage startLine exprs allDefs =
      let sortedDefs' = sortBy (\(_, l1), (_, l2) => compare l1 l2) allDefs
          nextStart = findNextStart startLine sortedDefs'
          inRange = filter (\(l, _, _) => l >= startLine && l < nextStart) exprs
          coveredSum = sum $ map (\(_, c, _) => c) inRange
          totalSum = sum $ map (\(_, _, t) => t) inRange
      in (coveredSum, totalSum)

||| Match all CompiledFunctions with profiler data to produce FunctionRuntimeHits
||| This is the main entry point for CLI/API shared coverage calculation
|||
||| @funcs - Static analysis results from --dumpcases
||| @ssHtmlContent - Content of .ss.html profiler output
||| @ssContent - Content of .ss Scheme source
export
matchAllFunctionsWithCoverage : (funcs : List CompiledFunction)
                               -> (ssHtmlContent : String)
                               -> (ssContent : String)
                               -> List FunctionRuntimeHit
matchAllFunctionsWithCoverage funcs ssHtmlContent ssContent =
  let -- Parse profiler data
      exprCoverage = parseAnnotatedHtml ssHtmlContent
      lineToExprs = groupByLine exprCoverage
      funcDefs = parseSchemeDefs ssContent
  in map (\f => matchFunctionWithCoverage f lineToExprs funcDefs) funcs
