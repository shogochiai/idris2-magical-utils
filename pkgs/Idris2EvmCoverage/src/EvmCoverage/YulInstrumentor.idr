||| Yul Instrumentation for Runtime Profiling
|||
||| Phase 2 of EVM coverage: Runtime instrumentation
|||
||| Strategy:
||| 1. Parse Yul to identify function entries and switch branches
||| 2. Insert counter increments at each control point
||| 3. Add profile flush function that emits events
||| 4. Counters stored in memory (low gas cost)
|||
||| Memory layout for counters:
|||   0x1000 - 0x1FFF: Counter array (max 128 functions * 32 bytes)
|||   Counter[i] = mload(0x1000 + i * 32)
|||
||| Event signature:
|||   ProfileFlush(uint256[] counters)
|||   topic = keccak256("ProfileFlush(uint256[])")
module EvmCoverage.YulInstrumentor

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System.File
import System

import EvmCoverage.Runtime
import EvmCoverage.Types
import EvmCoverage.YulMapper

%default covering

-- =============================================================================
-- Helper Functions
-- =============================================================================

||| Zip a list with indices starting from 0
zipWithIndex : List a -> List (Nat, a)
zipWithIndex xs = go 0 xs
  where
    go : Nat -> List a -> List (Nat, a)
    go _ [] = []
    go n (x :: rest) = (n, x) :: go (S n) rest

-- =============================================================================
-- Constants
-- =============================================================================

||| Base memory address for profile counters
COUNTER_BASE : Integer
COUNTER_BASE = 0x1000

||| Maximum number of instrumentation points
MAX_COUNTERS : Nat
MAX_COUNTERS = 128

||| Event topic for ProfileFlush
||| keccak256("ProfileFlush(uint256[])")
PROFILE_FLUSH_TOPIC : Integer
PROFILE_FLUSH_TOPIC = 0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925

PROFILE_FLUSH_TOPIC_HEX : String
PROFILE_FLUSH_TOPIC_HEX = "0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925"

-- =============================================================================
-- Yul Parsing Types
-- =============================================================================

||| Represents a function found in Yul
record YulFunction where
  constructor MkYulFunction
  name : String
  startLine : Nat
  hasSwitch : Bool  -- Contains switch statement (branching)

||| Result of Yul parsing
record YulParseResult where
  constructor MkYulParseResult
  functions : List YulFunction
  runtimeStart : Nat  -- Line where runtime code starts
  runtimeEnd : Nat    -- Line where runtime code ends

-- =============================================================================
-- Yul Parser
-- =============================================================================

||| Check if line is a function definition
||| Pattern: "function Name(args) -> result {"
isFunctionDef : String -> Bool
isFunctionDef line =
  let trimmed = trim line
  in isPrefixOf "function " trimmed && isInfixOf "(" trimmed

||| Extract function name from definition line
extractFuncName : String -> String
extractFuncName line =
  let trimmed = trim line
      afterFunc = substr 9 (length trimmed) trimmed  -- Skip "function "
      parts = words afterFunc
  in case parts of
       (name :: _) =>
         -- Remove everything after '('
         let chars = unpack name
         in pack $ takeWhile (/= '(') chars
       _ => ""

||| Check if line contains switch statement
hasSwitch : String -> Bool
hasSwitch line = isInfixOf "switch " line

||| Parse Yul content to find functions and structure
parseYul : String -> YulParseResult
parseYul content =
  let ls = lines content
      indexed = zipWithIndex ls
      funcs = findFunctions indexed
      (rStart, rEnd) = findRuntime indexed
  in MkYulParseResult funcs rStart rEnd
  where
    findFunctions : List (Nat, String) -> List YulFunction
    findFunctions [] = []
    findFunctions ((n, line) :: rest) =
      if isFunctionDef line
        then let name = extractFuncName line
                 hasS = any (hasSwitch . snd) (take 50 rest)  -- Check next 50 lines
             in MkYulFunction name n hasS :: findFunctions rest
        else findFunctions rest

    findRuntime : List (Nat, String) -> (Nat, Nat)
    findRuntime indexed =
      let starts = filter (\(_, l) => isInfixOf "object \"runtime\"" l) indexed
          -- Find the end of the last function definition, not the last }
          -- We'll look for lines that are just "}" with 6 spaces indent (function end)
          funcEnds = filter (\(_, l) => trim l == "}" && isPrefixOf "      }" l) indexed
      in case starts of
           ((s, _) :: _) =>
             -- Use the last function end, or fall back to a reasonable position
             case reverse funcEnds of
               ((e, _) :: _) => (s, e)
               _ => (s, length indexed `minus` 3)  -- Before last few closing braces
           _ => (0, length indexed)

-- =============================================================================
-- String Replacement Helpers
-- =============================================================================

||| Match prefix of character lists
matchPfx : List Char -> List Char -> Bool
matchPfx [] _ = True
matchPfx _ [] = False
matchPfx (n :: ns) (h :: hs) = n == h && matchPfx ns hs

||| Replace first occurrence of needle with replacement in character list
replaceCharsOnce : List Char -> List Char -> List Char -> List Char
replaceCharsOnce _ _ [] = []
replaceCharsOnce needle repl hs@(h :: rest) =
  if matchPfx needle hs
    then repl ++ drop (length needle) hs  -- Replace and keep the rest unchanged
    else h :: replaceCharsOnce needle repl rest

||| Replace first occurrence of needle with replacement in string
replaceOnce : String -> String -> String -> String
replaceOnce needle repl haystack =
  pack $ replaceCharsOnce (unpack needle) (unpack repl) (unpack haystack)

-- =============================================================================
-- Counter Increment Generation
-- =============================================================================

||| Generate Yul code to increment counter at index i
||| counter[i] = counter[i] + 1
generateCounterIncrement : Nat -> String
generateCounterIncrement idx =
  let offset = show (cast {to=Integer} COUNTER_BASE + cast idx * 32)
  in "        // PROFILE: increment counter " ++ show idx ++ "\n" ++
     "        mstore(" ++ offset ++ ", add(mload(" ++ offset ++ "), 1))\n"

generateCounterIncrementLines : Nat -> List String
generateCounterIncrementLines idx = lines (generateCounterIncrement idx)

-- =============================================================================
-- Profile Flush Function Generation
-- =============================================================================

||| Generate the __profile_flush function
||| Emits all counters as a single event
generateFlushFunction : Nat -> String
generateFlushFunction numCounters =
  let counterReads = generateCounterReads numCounters
      eventEmit = generateEventEmit numCounters
  in "      // @source: ProfileInstrumentation:0:0--0:0\n" ++
     "      function __profile_flush(__profile_arg0) -> __profile_result {\n" ++
     "        // Copy counters to memory starting at 0x2000 for event data\n" ++
     "        // Format: offset (32) + length (32) + data (numCounters * 32)\n" ++
     counterReads ++
     "        // Emit ProfileFlush event\n" ++
     eventEmit ++
     "        __profile_result := 0\n" ++
     "      }\n"
  where
    generateCounterReads : Nat -> String
    generateCounterReads Z = ""
    generateCounterReads n@(S k) =
      let idx = minus n 1
          srcOffset = show (cast {to=Integer} COUNTER_BASE + cast idx * 32)
          dstOffset = show (0x2040 + cast idx * 32)  -- After offset + length
      in "        mstore(" ++ dstOffset ++ ", mload(" ++ srcOffset ++ "))\n" ++
         generateCounterReads k

    generateEventEmit : Nat -> String
    generateEventEmit n =
      let dataSize = show (64 + cast n * 32)  -- offset + length + data
          topic = PROFILE_FLUSH_TOPIC_HEX
      in "        mstore(0x2000, 32)  // offset to array\n" ++
         "        mstore(0x2020, " ++ show n ++ ")  // array length\n" ++
         "        log1(0x2000, " ++ dataSize ++ ", " ++ topic ++ ")\n"

-- =============================================================================
-- Yul Instrumentation
-- =============================================================================

||| Insert counter increment after function definition line
instrumentFunction : Nat -> Nat -> String -> String
instrumentFunction funcIdx lineNum line =
  if isFunctionDef line
    then line ++ "\n" ++ generateCounterIncrement funcIdx
    else line

||| Instrument entire Yul content
||| Adds counter increments at function entries and adds flush function
shortStaticFuncName : String -> String
shortStaticFuncName funcName =
  case reverse (forget $ split (== '.') funcName) of
    (x :: _) => x
    [] => funcName

lastSegmentBy : Char -> String -> String
lastSegmentBy ch s =
  case reverse (forget $ split (== ch) s) of
    (x :: _) => x
    [] => s

dropLeadingUPrefix : String -> String
dropLeadingUPrefix s =
  if isPrefixOf "u_" s
     then substr 2 (length s `minus` 2) s
     else s

normalizeStaticFuncName : String -> String
normalizeStaticFuncName funcName =
  case reverse (forget $ split (== '.') funcName) of
    [] => lastSegmentBy ':' funcName
    (rawFunc :: revModules) =>
      let baseFunc = lastSegmentBy ':' rawFunc
          modulePath = joinBy "." (reverse revModules)
      in if null modulePath then baseFunc else modulePath ++ "." ++ baseFunc

operatorAliases : String -> String -> String -> List String
operatorAliases mangled modulePath name =
  if (modulePath == "Prelude.Types" || modulePath == "Prelude.EqOrd") && isInfixOf "Eq" name
     then ["=="]
  else if modulePath == "Prelude.EqOrd" && isInfixOf "_u____Ord_" mangled
     then [">="]
  else if modulePath == "Prelude.EqOrd" && isInfixOf "_u___Ord_" mangled
     then [">"]
  else if modulePath == "Prelude.Types.SnocList" && name == "___"
     then ["(<>>)"]
  else if modulePath == "Prelude.Interfaces.Bool.Semigroup" && isInfixOf "Semigroup" name
     then ["<+>"]
  else []

specializationBase : String -> List String
specializationBase name =
  if isPrefixOf "prim__" name
     then []
     else case forget (split (== '_') name) of
            (base :: _) =>
              if base /= name && not (null base)
                 then [base]
                 else []
            [] => []

runtimeNameCandidates : String -> List String
runtimeNameCandidates mangled =
  case parseYulFuncName mangled of
    Nothing => [mangled]
    Just f =>
      let base = dropLeadingUPrefix f.funcName
          shortNames = [base, f.funcName] ++ operatorAliases mangled f.modulePath base ++ specializationBase base
          qualify : String -> String
          qualify nm =
            if null f.modulePath then nm else f.modulePath ++ "." ++ nm
          qualifiedNames = map qualify shortNames
      in nub $ filter (not . null) (qualifiedNames ++ shortNames)

data BranchTag = BTCase0 | BTCase1 | BTDefault | BTUnknown

Eq BranchTag where
  BTCase0 == BTCase0 = True
  BTCase1 == BTCase1 = True
  BTDefault == BTDefault = True
  BTUnknown == BTUnknown = True
  _ == _ = False

record QueuedBranch where
  constructor MkQueuedBranch
  branchId : String
  branchTag : BranchTag
  materializable : Bool

branchPatternLabel : String -> String
branchPatternLabel pattern =
  let beforeSpan = fst (break (== '@') pattern)
      beforeOrigin = fst (break (== '[') beforeSpan)
  in trim beforeOrigin

tagFromBranchLabel : String -> BranchTag
tagFromBranchLabel raw =
  let label = trim raw in
  if label == "1" || label == "True"
     then BTCase1
  else if label == "0" || label == "False"
     then BTCase0
  else if label == "default"
     then BTDefault
  else if label == "_builtin.JUST" || label == "_builtin.CONS"
       || label == "Prelude.Types.Right"
       || label == "Subcontract.Core.Outcome.Fail"
     then BTCase1
  else if label == "_builtin.NOTHING" || label == "_builtin.NIL"
       || label == "Prelude.Types.Left"
       || label == "Subcontract.Core.Outcome.Ok"
     then BTCase0
  else BTUnknown

tagForBranch : ClassifiedBranch -> BranchTag
tagForBranch branch = tagFromBranchLabel (branchPatternLabel branch.pattern)

isMaterializableBranchLabel : String -> Bool
isMaterializableBranchLabel raw =
  let label = trim raw in
  label == "1" || label == "0" || label == "default" || label == "True" || label == "False"

isMaterializableBranch : ClassifiedBranch -> Bool
isMaterializableBranch branch = isMaterializableBranchLabel (branchPatternLabel branch.pattern)

lineBranchTag : String -> BranchTag
lineBranchTag line =
  let trimmed = ltrim line in
  if isPrefixOf "case 1 {" trimmed
     then BTCase1
  else if isPrefixOf "case 0 {" trimmed
     then BTCase0
  else if isPrefixOf "default {" trimmed
     then BTDefault
  else BTUnknown

insertQueuedBranch : String -> QueuedBranch -> List (String, List QueuedBranch) -> List (String, List QueuedBranch)
insertQueuedBranch fn q [] = [(fn, [q])]
insertQueuedBranch fn q ((k, vs) :: rest) =
  if k == fn then (k, vs ++ [q]) :: rest
  else (k, vs) :: insertQueuedBranch fn q rest

buildStaticBranchQueues : List ClassifiedBranch -> List (String, List QueuedBranch)
buildStaticBranchQueues branches =
  foldl addQueue [] branches
  where
    addQueue : List (String, List QueuedBranch) -> ClassifiedBranch -> List (String, List QueuedBranch)
    addQueue acc branch =
      if isCanonical branch.branchClass
         then insertQueuedBranch
                (normalizeStaticFuncName branch.branchId.funcName)
                (MkQueuedBranch (show branch.branchId) (tagForBranch branch) (isMaterializableBranch branch))
                acc
         else acc

lookupQueue : String -> List (String, List QueuedBranch) -> List QueuedBranch
lookupQueue _ [] = []
lookupQueue fn ((k, vs) :: rest) = if fn == k then vs else lookupQueue fn rest

selectQueueKey : List String -> List (String, List QueuedBranch) -> Maybe String
selectQueueKey [] _ = Nothing
selectQueueKey (candidate :: rest) queues =
  if null (lookupQueue candidate queues)
     then selectQueueKey rest queues
     else Just candidate

takeMatchingBranch : BranchTag -> List QueuedBranch -> (Maybe QueuedBranch, List QueuedBranch)
takeMatchingBranch wanted [] = (Nothing, [])
takeMatchingBranch wanted (x :: xs) =
  if x.branchTag == wanted
     then (Just x, xs)
  else let (found, rest) = takeMatchingBranch wanted xs in
       (found, x :: rest)

takeFirstBranch : List QueuedBranch -> (Maybe QueuedBranch, List QueuedBranch)
takeFirstBranch [] = (Nothing, [])
takeFirstBranch (x :: xs) = (Just x, xs)

takeBestBranch : BranchTag -> List QueuedBranch -> (Maybe QueuedBranch, List QueuedBranch)
takeBestBranch wanted vs =
  let (exact, restExact) = takeMatchingBranch wanted vs in
  case exact of
    Just q => (Just q, restExact)
    Nothing =>
      case wanted of
        BTDefault =>
          let (zeroLike, restZeroLike) = takeMatchingBranch BTCase0 vs in
          case zeroLike of
            Just q => (Just q, restZeroLike)
            Nothing => takeFirstBranch vs
        _ => takeFirstBranch vs

consumeQueue : String -> BranchTag -> List (String, List QueuedBranch) -> (Maybe QueuedBranch, List (String, List QueuedBranch))
consumeQueue _ _ [] = (Nothing, [])
consumeQueue fn wanted ((k, vs) :: rest) =
  if fn == k
     then let (picked, remaining) = takeBestBranch wanted vs in
          (picked, (k, remaining) :: rest)
     else let (bid, updated) = consumeQueue fn wanted rest in (bid, (k, vs) :: updated)

consumeFirstQueue : String -> List (String, List QueuedBranch) -> (Maybe QueuedBranch, List (String, List QueuedBranch))
consumeFirstQueue _ [] = (Nothing, [])
consumeFirstQueue fn ((k, vs) :: rest) =
  if fn == k
     then let (picked, remaining) = takeFirstBranch vs in
          (picked, (k, remaining) :: rest)
     else let (bid, updated) = consumeFirstQueue fn rest in (bid, (k, vs) :: updated)

isCaseBranchLine : String -> Bool
isCaseBranchLine line =
  let trimmed = ltrim line
  in isPrefixOf "case " trimmed || isPrefixOf "default {" trimmed

demangledShortName : String -> String
demangledShortName mangled =
  case parseYulFuncName mangled of
    Just f => f.funcName
    Nothing => mangled

record BranchRuntimeLabel where
  constructor MkBranchRuntimeLabel
  labelIndex : Nat
  mangledName : String
  demangledName : String
  branchId : String

formatBranchLabel : BranchRuntimeLabel -> String
formatBranchLabel lbl =
  show lbl.labelIndex ++ ",branch," ++ lbl.mangledName ++ "," ++ lbl.demangledName ++ "," ++ lbl.branchId

generateBranchLabelMap : List BranchRuntimeLabel -> String
generateBranchLabelMap labels =
  "index,kind,mangled_name,demangled_name,branch_id\n" ++
  unlines (map formatBranchLabel labels)

instrumentBranchLines : List String ->
                        List (String, List QueuedBranch) ->
                        Maybe (String, String, String) ->
                        Nat ->
                        (List String, List BranchRuntimeLabel)
instrumentBranchLines [] _ _ _ = ([], [])
instrumentBranchLines (line :: rest) queues current nextIdx =
  if isFunctionDef line
     then let mangled = extractFuncName line
              candidates = runtimeNameCandidates mangled
              selectedKey = selectQueueKey candidates queues
              demangled = fromMaybe (demangledShortName mangled) selectedKey
              queue = maybe [] (\key => lookupQueue key queues) selectedKey
              (mbid, queues') =
                if length queue == 1
                   then maybe (Nothing, queues) (\key => consumeFirstQueue key queues) selectedKey
                   else (Nothing, queues)
              (restOut, restLabels) =
                instrumentBranchLines rest queues' (map (\key => (mangled, demangled, key)) selectedKey)
                                     (case mbid of
                                        Just qb => if qb.materializable then S nextIdx else nextIdx
                                        Nothing => nextIdx)
              thisLines =
                case mbid of
                  Just qb => if qb.materializable then line :: generateCounterIncrementLines nextIdx else [line]
                  Nothing => [line]
              thisLabels =
                case mbid of
                  Just qb => if qb.materializable then [MkBranchRuntimeLabel nextIdx mangled demangled qb.branchId] else []
                  Nothing => []
          in (thisLines ++ restOut, thisLabels ++ restLabels)
     else if isCaseBranchLine line
             then case current of
                    Just (mangled, demangled, queueKey) =>
                      let (mbid, queues') = consumeQueue queueKey (lineBranchTag line) queues
                          (restOut, restLabels) =
                            instrumentBranchLines rest queues' current
                              (case mbid of
                                 Just qb => if qb.materializable then S nextIdx else nextIdx
                                 Nothing => nextIdx)
                          thisLines =
                            case mbid of
                              Just qb => if qb.materializable then line :: generateCounterIncrementLines nextIdx else [line]
                              Nothing => [line]
                          thisLabels =
                            case mbid of
                              Just qb => if qb.materializable then [MkBranchRuntimeLabel nextIdx mangled demangled qb.branchId] else []
                              Nothing => []
                      in (thisLines ++ restOut, thisLabels ++ restLabels)
                    Nothing =>
                      let (restOut, restLabels) = instrumentBranchLines rest queues current nextIdx
                      in (line :: restOut, restLabels)
             else let (restOut, restLabels) = instrumentBranchLines rest queues current nextIdx
                  in (line :: restOut, restLabels)

instrumentYul : List ClassifiedBranch -> String -> (String, String)
instrumentYul staticBranches content =
  let parseResult = parseYul content
      staticQueues = buildStaticBranchQueues staticBranches
      ls = lines content
      -- FIRST: Add flush calls on hard exits in runtime primitives before counter
      -- instrumentation to preserve line structure for branch matching.
      withExitFlush = insertFlushBeforeExit ls Nothing
      -- Re-split to flatten any embedded newlines from flush insertion
      flattenedLines = lines (unlines withExitFlush)
      -- Re-parse after modification (line numbers may have shifted)
      parseResult2 = parseYul (unlines flattenedLines)
      (instrumentedLines, branchLabels) = instrumentBranchLines flattenedLines staticQueues Nothing 0
      numLabels = length branchLabels
      -- Add flush function near the top of the runtime code block so it is in scope
      -- before any call sites.
      flushInsertAt = S parseResult2.runtimeStart
      withFlush = insertFlushFunction instrumentedLines flushInsertAt numLabels
      -- Add flush call in runtime entry block after main
      withFlushCall = insertFlushCall withFlush
  in (unlines withFlushCall, generateBranchLabelMap branchLabels)
  where
    insertFlushFunction : List String -> Nat -> Nat -> List String
    insertFlushFunction ls insertAt numFuncs =
      -- Insert after the selected line. For runtime instrumentation this should be the
      -- `code {` line so the helper is in scope before any calls.
      let (before, after) = splitAt (S insertAt) ls
          flushFunc = lines $ generateFlushFunction numFuncs
      in before ++ flushFunc ++ after

    -- Insert flush call after pop(main(0)) in runtime entry
    insertFlushCall : List String -> List String
    insertFlushCall ls = map addFlushAfterMain ls
      where
        addFlushAfterMain : String -> String
        addFlushAfterMain line =
          -- Look for pop(DumpcasesWrapper_u_main(0)) or similar main call
          if isInfixOf "_u_main(" line && isInfixOf "pop(" line
            then line ++ "\n                pop(__profile_flush(0))"
            else line

    insertFlushBeforeExit : List String -> Maybe String -> List String
    insertFlushBeforeExit [] _ = []
    insertFlushBeforeExit (l :: rest) current =
      let trimmed = ltrim l in
      if isFunctionDef l
         then let fname = extractFuncName l
              in l :: insertFlushBeforeExit rest (Just fname)
      else if trim l == "}" && isPrefixOf "      }" l
         then l :: insertFlushBeforeExit rest Nothing
      else if (current == Just "EVM_Primitives_u_prim__revert" && isPrefixOf "revert(" trimmed)
              || (current == Just "EVM_Primitives_u_prim__return" && isPrefixOf "return(" trimmed)
         then "        pop(__profile_flush(0))" :: l :: insertFlushBeforeExit rest current
      else l :: insertFlushBeforeExit rest current

-- =============================================================================
-- Function Label Mapping
-- =============================================================================

||| Generate function label mapping for off-chain analysis
||| Format: CSV with index,function_name
generateLabelMap : String -> String
generateLabelMap content =
  let parseResult = parseYul content
      funcs = parseResult.functions
      indexed = zipWithIndex funcs
  in "index,function_name,has_switch\n" ++
     unlines (map formatEntry indexed)
  where
    formatEntry : (Nat, YulFunction) -> String
    formatEntry (idx, func) =
      show idx ++ "," ++ func.name ++ "," ++ show func.hasSwitch

-- =============================================================================
-- Public API
-- =============================================================================

||| Instrument a Yul file for profiling
||| Returns: (instrumented Yul, label map CSV)
export
instrumentYulFile : String -> List ClassifiedBranch -> IO (Either String (String, String))
instrumentYulFile path staticBranches = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read: " ++ show err
  let (instrumented, labelMap) = instrumentYul staticBranches content
  pure $ Right (instrumented, labelMap)

||| Write instrumented Yul and label map to files
export
writeInstrumentedYul : String -> String -> String -> List ClassifiedBranch -> IO (Either String ())
writeInstrumentedYul inputPath outputPath labelMapPath staticBranches = do
  Right (instrumented, labelMap) <- instrumentYulFile inputPath staticBranches
    | Left err => pure $ Left err
  Right () <- writeFile outputPath instrumented
    | Left err => pure $ Left $ "Failed to write Yul: " ++ show err
  Right () <- writeFile labelMapPath labelMap
    | Left err => pure $ Left $ "Failed to write label map: " ++ show err
  pure $ Right ()

-- =============================================================================
-- Analysis Integration
-- =============================================================================

||| Parse profile event data from hex string
||| Returns list of (function_index, hit_count)
export
parseProfileEvent : String -> List (Nat, Integer)
parseProfileEvent hexData =
  -- Skip first 64 bytes (offset + length)
  let dataOnly = substr 128 (length hexData) hexData  -- 128 hex chars = 64 bytes
  in parseCounters dataOnly 0
  where
    parseCounters : String -> Nat -> List (Nat, Integer)
    parseCounters s idx =
      if length s < 64 then []
      else let chunk = substr 0 64 s  -- 32 bytes = 64 hex chars
               rest = substr 64 (length s) s
               -- Parse hex to integer (simplified)
               count = 0  -- TODO: implement hex parsing
           in (idx, count) :: parseCounters rest (S idx)

||| Merge static analysis (dumpcases) with runtime profile
||| Returns coverage gaps: functions with branches but zero hits
export
findCoverageGaps : List (String, Nat) -> List (Nat, Integer) -> List String
findCoverageGaps staticFuncs runtimeHits =
  let hitIndices = map (\p => fst p) $ filter (\p => snd p > 0) runtimeHits
  in mapMaybe (checkGap hitIndices) (zipWithIndex staticFuncs)
  where
    checkGap : List Nat -> (Nat, (String, Nat)) -> Maybe String
    checkGap hits (idx, (name, branches)) =
      if branches > 0 && not (idx `elem` hits)
        then Just name
        else Nothing

-- =============================================================================
-- Yul Generation + Instrumentation Pipeline
-- =============================================================================

||| Extract base name from ipkg path
getBaseName : String -> String
getBaseName path =
  let parts = forget $ split (== '/') path
      fileName = fromMaybe "" (last' parts)
  in if isSuffixOf ".ipkg" fileName
       then substr 0 (length fileName `minus` 5) fileName
       else fileName

||| Extract project directory from ipkg path
getProjectDir : String -> String
getProjectDir path =
  let parts = forget $ split (== '/') path
      dirParts = fromMaybe [] (init' parts)
  in if null dirParts then "." else joinBy "/" dirParts

||| Make output path relative to the package root when possible.
||| idris2-yul interprets --output-dir from the package build context, so
||| passing a repo-relative path like "pkgs/Foo/build/exec" from inside
||| "pkgs/Foo" creates a nested "pkgs/Foo/pkgs/Foo/build/exec" tree.
stripProjectPrefix : String -> String -> String
stripProjectPrefix projectDir outputDir =
  let projectPrefix = projectDir ++ "/"
  in if isPrefixOf projectPrefix outputDir
       then substr (length projectPrefix) (length outputDir `minus` length projectPrefix) outputDir
       else outputDir

||| Find pack install base directory
||| Pack installs to ~/.local/state/pack/install/<hash>/
findPackInstallBase : IO (Maybe String)
findPackInstallBase = do
  let tmpFile = "/tmp/pack-install-base.txt"
  -- Find the most recent pack install directory
  let cmd = "ls -td ~/.local/state/pack/install/*/ 2>/dev/null | head -1 > " ++ tmpFile
  _ <- system cmd
  Right content <- readFile tmpFile
    | Left _ => pure Nothing
  let path = trim content
  if null path
    then pure Nothing
    else pure $ Just path

||| Build IDRIS2_PACKAGE_PATH from pack install directory
||| Collects all package directories under the install base
buildPackagePath : String -> IO String
buildPackagePath basePath = do
  let tmpFile = "/tmp/pack-pkg-paths.txt"
  -- Find all package directories (they contain ipkg metadata)
  let cmd = "find " ++ basePath ++ " -type d -name 'idris2-*' 2>/dev/null > " ++ tmpFile
  _ <- system cmd
  Right content <- readFile tmpFile
    | Left _ => pure ""
  let paths = filter (not . null) $ map trim $ lines content
  pure $ joinBy ":" paths

readLogTail : String -> IO String
readLogTail path = do
  Right log <- readFile path | Left _ => pure ""
  let ls = lines log
  pure $ unlines $ reverse $ take 20 $ reverse ls

||| Discover package paths from pack install directory
||| Falls back to idris2 --list-packages if pack not found
discoverPackagePaths : IO (List String)
discoverPackagePaths = do
  -- First try pack install directory
  mBase <- findPackInstallBase
  case mBase of
    Just basePath => do
      let tmpFile = "/tmp/pack-pkg-paths.txt"
      -- Find all package directories under pack install
      let cmd = "find " ++ basePath ++ " -type d -name 'idris2-0.8.0' 2>/dev/null | head -20 > " ++ tmpFile
      _ <- system cmd
      Right content <- readFile tmpFile
        | Left _ => fallbackDiscovery
      let paths = filter (not . null) $ map trim $ lines content
      if null paths
        then fallbackDiscovery
        else pure paths
    Nothing => fallbackDiscovery
  where
    fallbackDiscovery : IO (List String)
    fallbackDiscovery = do
      let tmpFile = "/tmp/idris2-pkg-paths.txt"
      -- Run idris2 --list-packages and extract paths to temp file
      let cmd = "idris2 --list-packages 2>/dev/null | grep '└' | sed 's/.*└ //' > " ++ tmpFile
      _ <- system cmd
      Right content <- readFile tmpFile
        | Left _ => pure []
      let paths = filter (not . null) $ map trim $ lines content
      pure paths

||| Find the latest dumpcases-temp-*.ipkg file in project directory
||| This is created by Phase 1 (static analysis) and contains the main wrapper
findLatestTempIpkg : String -> IO (Maybe String)
findLatestTempIpkg projectDir = do
  let tmpFile = "/tmp/latest-temp-ipkg.txt"
  let cmd = "ls -t " ++ projectDir ++ "/dumpcases-temp-*.ipkg 2>/dev/null"
         ++ " | grep -v -- '-yul-' | head -1 > " ++ tmpFile
  _ <- system cmd
  Right content <- readFile tmpFile
    | Left _ => pure Nothing
  let path = trim content
  if null path
    then pure Nothing
    else pure $ Just path

sanitizeIpkgForYul : String -> IO (Either String String)
sanitizeIpkgForYul ipkgPath = do
  Right content <- readFile ipkgPath
    | Left err => pure $ Left $ "Failed to read build ipkg: " ++ show err
  t <- time
  let yulIpkgPath = substr 0 (length ipkgPath `minus` 5) ipkgPath ++ "-yul-" ++ show t ++ ".ipkg"
  let rewriteYulDep : String -> String
      rewriteYulDep line =
        let trimmed = trim line in
        if trimmed == "idris2-yul"
           then replaceOnce "idris2-yul" "idris2-evm" line
        else if trimmed == ", idris2-yul"
                then replaceOnce "idris2-yul" "idris2-evm" line
                else line
  let sanitized =
        unlines $
          map rewriteYulDep $
            filter (\line => not (isPrefixOf "opts =" (trim line))) (lines content)
  Right () <- writeFile yulIpkgPath sanitized
    | Left err => pure $ Left $ "Failed to write Yul build ipkg: " ++ show err
  pure $ Right yulIpkgPath

hasExecutableMain : String -> IO Bool
hasExecutableMain ipkgPath = do
  Right content <- readFile ipkgPath
    | Left _ => pure False
  let ls = map trim (lines content)
  pure (any (isPrefixOf "main =") ls && any (isPrefixOf "executable =") ls)

cleanupYulArtifacts : String -> IO ()
cleanupYulArtifacts outputDir = do
  _ <- system $ "rm -f " ++ outputDir ++ "/*.yul " ++ outputDir ++ "/*.anf"
  pure ()

findLatestGeneratedYul : String -> IO (Maybe String)
findLatestGeneratedYul outputDir = do
  let tmpFile = "/tmp/latest-generated-yul.txt"
  let cmd = "ls -t " ++ outputDir ++ "/*.yul 2>/dev/null | grep -v -- '-instrumented.yul' | head -1 > " ++ tmpFile
  _ <- system cmd
  Right content <- readFile tmpFile
    | Left _ => pure Nothing
  let path = trim content
  if null path then pure Nothing else pure (Just path)

||| Generate Yul from Idris2 project using idris2-yul --codegen yul
||| Note: idris2-yul is a custom compiler with yul backend built-in
||| Uses the dumpcases-temp-*.ipkg from Phase 1 (which has main wrapper)
||| Automatically discovers pack's package paths via idris2 --list-packages
||| Returns path to generated .yul file
export
generateYul : String -> String -> IO (Either String String)
generateYul ipkgPath outputDir = do
  -- Extract project directory from ipkg path
  let projectDir = getProjectDir ipkgPath
  originalHasMain <- hasExecutableMain ipkgPath
  -- Runtime coverage should prefer the project's real executable when it exists.
  -- Fall back to the dumpcases wrapper only for library-style packages with no main.
  mTempIpkg <- if originalHasMain then pure Nothing else findLatestTempIpkg projectDir
  let actualIpkg = fromMaybe ipkgPath mTempIpkg
  Right yulIpkg <- sanitizeIpkgForYul actualIpkg
    | Left err => pure $ Left err
  let buildIpkg = getBaseName yulIpkg ++ ".ipkg"
  let outputDirRel = stripProjectPrefix projectDir outputDir
  let outputDirForBuild = if null outputDirRel then "." else outputDirRel
  -- Discover package paths from pack's idris2 installation
  paths <- discoverPackagePaths
  let pkgPath = joinBy ":" paths
  cleanupYulArtifacts outputDir
  -- Set IDRIS2_PACKAGE_PATH and call idris2-yul
  let buildLog = outputDir ++ "/idris2-yul-build.log"
  let cmd = if null pkgPath
              then "mkdir -p " ++ outputDir
                   ++ " && cd " ++ projectDir
                   ++ " && idris2-yul --codegen yul --output-dir " ++ outputDirForBuild ++ " --build " ++ buildIpkg
                   ++ " > " ++ buildLog ++ " 2>&1"
              else "mkdir -p " ++ outputDir
                   ++ " && cd " ++ projectDir
                   ++ " && IDRIS2_PACKAGE_PATH=\"" ++ pkgPath ++ "\" idris2-yul --codegen yul --output-dir " ++ outputDirForBuild ++ " --build " ++ buildIpkg
                   ++ " > " ++ buildLog ++ " 2>&1"
  exitCode <- system cmd
  if exitCode == 0
    then do
      mYulPath <- findLatestGeneratedYul outputDir
      case mYulPath of
        Just yulPath => pure $ Right yulPath
        Nothing => pure $ Left "Yul generation succeeded but no .yul file was produced"
    else do
      logTail <- readLogTail buildLog
      pure $ Left $
        "Yul generation failed (exit " ++ show exitCode ++ ")"
        ++ (if null logTail then "" else "\nYul build log tail:\n" ++ logTail)

||| Full pipeline: Generate Yul → Instrument → Write output files
||| Returns: (instrumented yul path, label map path)
export
generateAndInstrumentYul : String -> String -> List ClassifiedBranch -> IO (Either String (String, String))
generateAndInstrumentYul ipkgPath outputDir staticBranches = do
  Right yulPath <- generateYul ipkgPath outputDir
    | Left err => pure $ Left err
  let baseName = substr 0 (length yulPath `minus` 4) yulPath  -- Remove .yul
  let instrPath = baseName ++ "-instrumented.yul"
  let labelPath = baseName ++ "-labels.csv"
  Right () <- writeInstrumentedYul yulPath instrPath labelPath staticBranches
    | Left err => pure $ Left err
  pure $ Right (instrPath, labelPath)

-- =============================================================================
-- Phase 2.3: Solc Compilation
-- =============================================================================

||| Compile instrumented Yul to bytecode using solc
||| Returns path to compiled binary output file
export
compileYulToBytecode : String -> String -> IO (Either String String)
compileYulToBytecode yulPath outputDir = do
  let baseName = getBaseName yulPath
  let binPath = outputDir ++ "/" ++ baseName ++ ".bin"
  let solcLog = outputDir ++ "/solc-build.log"
  -- For strict-assembly mode, solc outputs to stdout, redirect to file
  let cmd = "solc --strict-assembly --optimize " ++ yulPath ++ " --bin > " ++ binPath ++ " 2> " ++ solcLog
  -- First check for syntax errors
  let checkCmd = "solc --strict-assembly " ++ yulPath ++ " > /dev/null 2> " ++ solcLog
  checkResult <- system checkCmd
  if checkResult /= 0
    then do
      logTail <- readLogTail solcLog
      pure $ Left $
        "Yul syntax error (instrumentation bug)"
        ++ (if null logTail then "" else "\nsolc log tail:\n" ++ logTail)
    else do
      exitCode <- system cmd
      if exitCode == 0
        then pure $ Right binPath
        else do
          logTail <- readLogTail solcLog
          pure $ Left $
            "solc compilation failed (exit " ++ show exitCode ++ ")"
            ++ (if null logTail then "" else "\nsolc log tail:\n" ++ logTail)

-- =============================================================================
-- Phase 2.4: Idris2 EVM Test Execution
-- =============================================================================

||| Extract runtime bytecode from solc output (skips init code header)
findRuntimeSeparator : List Char -> Nat -> Maybe Nat
findRuntimeSeparator [] _ = Nothing
findRuntimeSeparator [_] _ = Nothing
findRuntimeSeparator ('f' :: 'e' :: rest) idx = Just idx
findRuntimeSeparator (_ :: rest) idx = findRuntimeSeparator rest (S idx)

extractRuntimeSegment : String -> String
extractRuntimeSegment hex =
  case findRuntimeSeparator (unpack hex) 0 of
    Just idx => substr (idx + 2) (length hex `minus` (idx + 2)) hex
    Nothing => hex

extractRuntimeBytecode : String -> String
extractRuntimeBytecode content =
  -- Find "Binary representation:" line and get next line
  let ls = lines content
      afterBinary = dropWhile (\l => not $ isInfixOf "Binary representation:" l) ls
  in case afterBinary of
       (_ :: byteLine :: _) => extractRuntimeSegment (trim byteLine)
       _ => ""

||| Run instrumented bytecode via idris2-evm-run and capture trace
||| Returns path to trace output
export
runIdrisEvmTest : String -> String -> IO (Either String String)
runIdrisEvmTest binPath traceOutput = do
  -- Extract runtime bytecode (skip init code)
  Right content <- readFile binPath
    | Left _ => pure $ Left "Cannot read bytecode file"
  let runtimeHex = extractRuntimeBytecode content
  -- Write runtime bytecode to temp file
  let runtimePath = "/tmp/runtime.bin"
  Right () <- writeFile runtimePath runtimeHex
    | Left _ => pure $ Left "Cannot write runtime bytecode"
  mTimeout <- getEnv "IDRIS2_EVM_RUN_TIMEOUT_SECS"
  let timeoutSecs = fromMaybe "20" mTimeout
  let cmd =
        "python3 -c 'import os,signal,subprocess,sys\n"
        ++ "out=open(sys.argv[1],\"wb\")\n"
        ++ "try:\n"
        ++ " p=subprocess.Popen([\"idris2-evm-run\",\"--trace\",sys.argv[2],\"--gas\",\"100000000\"], stdout=out, stderr=subprocess.STDOUT, start_new_session=True)\n"
        ++ " sys.exit(p.wait(timeout=int(sys.argv[3])))\n"
        ++ "except subprocess.TimeoutExpired:\n"
        ++ " os.killpg(p.pid, signal.SIGKILL)\n"
        ++ " sys.exit(124)\n"
        ++ "' \"" ++ traceOutput ++ "\" \"" ++ runtimePath ++ "\" \"" ++ timeoutSecs ++ "\""
  exitCode <- system cmd
  Right trace <- readFile traceOutput
    | Left _ => pure $ Left "Cannot read trace output"
  if exitCode == 124
     then if null (trim trace)
             then pure $ Left ("idris2-evm-run timed out after " ++ timeoutSecs ++ "s with no trace")
             else pure $ Right traceOutput
     else pure $ Right traceOutput

-- =============================================================================
-- Phase 3: Coverage Analysis Result
-- =============================================================================

||| Coverage analysis result
public export
record CoverageAnalysis where
  constructor MkCoverageAnalysis
  staticBranches : Nat
  materializedBranches : Nat
  branchHits : Nat
  functionsHit : Nat
  totalFunctions : Nat
  coveragePercent : Nat
  coveredBranchIds : List String
  unobservableBranchIds : List String
  materializedBranchIds : List String
  uncoveredFunctions : List String

||| Run full coverage pipeline: Yul生成 → 計装 → solc → test → gap analysis
groupStaticFunctions : List ClassifiedBranch -> List String
groupStaticFunctions branches =
  foldl addFn [] (filter (\b => isCanonical b.branchClass) branches)
  where
    addFn : List String -> ClassifiedBranch -> List String
    addFn acc branch =
      let fn = shortStaticFuncName branch.branchId.funcName
      in if elem fn acc then acc else fn :: acc

export
runFullPipeline : String -> String -> List ClassifiedBranch -> IO (Either String CoverageAnalysis)
runFullPipeline ipkgPath outputDir staticBranches = do
  let canonicalBranches = filter (\b => isCanonical b.branchClass) staticBranches
  let staticDenominatorIds = map (show . (.branchId)) canonicalBranches
  let totalFunctionsStatic = length (groupStaticFunctions canonicalBranches)
  putStrLn $ "[runFullPipeline] canonical_branches=" ++ show (length canonicalBranches)
  putStrLn "[runFullPipeline] stage=generate_and_instrument"
  -- Step 1: Generate and instrument Yul
  Right (instrPath, labelPath) <- generateAndInstrumentYul ipkgPath outputDir canonicalBranches
    | Left err => pure $ Left $ "Instrumentation failed: " ++ err

  putStrLn $ "[runFullPipeline] generated=" ++ instrPath
  putStrLn "[runFullPipeline] stage=compile"
  -- Step 2: Compile with solc
  Right _ <- compileYulToBytecode instrPath outputDir
    | Left err => pure $ Left $ "Compilation failed: " ++ err

  putStrLn "[runFullPipeline] stage=execute"
  -- Step 3: Run instrumented bytecode with idris2-evm-run
  let binPath = instrPath ++ ".bin"
  let traceOutput = outputDir ++ "/coverage-trace.csv"
  testResult <- runIdrisEvmTest binPath traceOutput

  putStrLn "[runFullPipeline] stage=parse"
  -- Step 4: Parse events and analyze gaps
  case testResult of
    Left err => do
      -- No test execution, return static-only analysis
      let uncovered = groupStaticFunctions canonicalBranches
      pure $ Right $ MkCoverageAnalysis
                       (length canonicalBranches)
                       0
                       0
                       0
                       totalFunctionsStatic
                       0
                       []
                       staticDenominatorIds
                       []
                       uncovered
    Right tracePath => do
      -- Parse ProfileFlush events from trace output
      coverageResult <- analyzeBranchCoverageFromFile tracePath labelPath staticDenominatorIds
      case coverageResult of
        Left err => do
          putStrLn $ "Coverage analysis error: " ++ err
          let uncovered = groupStaticFunctions canonicalBranches
          pure $ Right $ MkCoverageAnalysis
                           (length canonicalBranches)
                           0
                           0
                           0
                           totalFunctionsStatic
                           0
                           []
                           staticDenominatorIds
                           []
                           uncovered
        Right result => do
          let diagnosticsCsvPath = outputDir ++ "/branch-counter-diagnostics.csv"
          _ <- writeBranchCounterDiagnostics tracePath labelPath staticDenominatorIds diagnosticsCsvPath
          let hitFuncs = length result.coveredFunctions
          let totalFuncs = totalFunctionsStatic
          let pct = cast {to=Nat} (floor result.coveragePercent)
          pure $ Right $ MkCoverageAnalysis
                           (length canonicalBranches)
                           result.materializedTotalBranches
                           result.hitBranches
                           hitFuncs
                           totalFuncs
                           pct
                           result.coveredBranchIds
                           result.unobservableBranchIds
                           result.materializedBranchIds
                           result.uncoveredFunctions
