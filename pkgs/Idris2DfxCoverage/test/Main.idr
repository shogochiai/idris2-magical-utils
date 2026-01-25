||| Test WasmBranchParser with OUC WASM
module Main

import System
import System.File
import Data.String
import Data.List
import Data.Maybe

import DfxCoverage.WasmMapper.WasmBranchParser
import DfxCoverage.IcWasm.ProfilingParser
import DfxCoverage.Exclusions

||| Parse function names from wasm-objdump output
||| Format: " - func[N] <name>"
parseNameLine : String -> Maybe (Nat, String)
parseNameLine line =
  let trimmed = trim line
  in if isPrefixOf "- func[" trimmed
       then
         let afterBracket = drop 7 (unpack trimmed)  -- Skip "- func["
             (numChars, rest) = break (== ']') afterBracket
         in case parsePositive (pack numChars) of
              Just n =>
                let afterClose = drop 2 rest  -- Skip "] "
                    (_, nameRest) = break (== '<') afterClose
                    (nameChars, _) = break (== '>') (drop 1 nameRest)
                in Just (n, pack nameChars)
              Nothing => Nothing
       else Nothing

||| Build name lookup from list of (idx, name) pairs
buildNameLookup : List (Nat, String) -> Nat -> Maybe String
buildNameLookup names idx =
  map snd $ find (\(i, _) => i == idx) names

||| Load names from wasm-objdump -j name -x
loadWasmNames : String -> IO (List (Nat, String))
loadWasmNames wasmPath = do
  let tmpFile = "/tmp/wasm_names.txt"
  let cmd = "wasm-objdump -j name -x \"" ++ wasmPath ++ "\" 2>/dev/null > " ++ tmpFile
  _ <- system cmd
  Right content <- readFile tmpFile
    | Left _ => pure []
  _ <- system $ "rm -f " ++ tmpFile
  pure $ mapMaybe parseNameLine (lines content)

main : IO ()
main = do
  args <- getArgs
  let (watPath, wasmPath, profilingPath) = case args of
                  [_, wat] => (wat, Nothing, Nothing)
                  [_, wat, wasm] => (wat, Just wasm, Nothing)
                  [_, wat, wasm, prof] => (wat, Just wasm, Just prof)
                  _ => ("/tmp/ouc_stubbed.wat", Just "/Users/bob/code/idris2-ouc/build/ouc.wasm", Nothing)

  putStrLn $ "Analyzing: " ++ watPath
  putStrLn "================================================"

  -- Load function names if WASM path provided
  nameList <- case wasmPath of
                Nothing => pure []
                Just wasm => do
                  putStrLn $ "Loading names from: " ++ wasm
                  loadWasmNames wasm

  let nameLookup = buildNameLookup nameList
  putStrLn $ "Loaded " ++ show (length nameList) ++ " function names"

  result <- readWatBranches watPath
  case result of
    Left err => putStrLn $ "Error: " ++ err
    Right analysis => do
      putStrLn $ "Total branch instructions: " ++ show analysis.totalBranches
      putStrLn $ "Total branch targets: " ++ show analysis.totalBranchTargets
      putStrLn ""

      -- Show unfiltered High Impact Targets
      putStrLn "=== ALL High Impact Targets (raw, top 10) ==="
      traverse_ (showNamedTarget nameLookup) analysis.highImpactTargets

      -- Show filtered High Impact Targets
      putStrLn ""
      putStrLn "=== FILTERED High Impact Targets (excluding libc/runtime) ==="
      let filtered = getFilteredHighImpact 10 nameLookup icpFullExclusions analysis.funcBranchCounts
      traverse_ showFiltered filtered

      -- If profiling data provided, show coverage priorities
      case profilingPath of
        Nothing => do
          putStrLn ""
          putStrLn "(No profiling data - pass profiling output as third argument)"
        Just profPath => do
          putStrLn ""
          putStrLn "Loading profiling data..."
          Right profContent <- readFile profPath
            | Left err => putStrLn $ "Error reading profiling: " ++ show err
          let profResult = parseProfilingOutput profContent
          let executedIds = getExecutedFuncIds profResult
          putStrLn $ "Executed functions: " ++ show (length executedIds)

          -- Filter then calculate coverage
          let filteredTargets = filterByExclusions nameLookup icpFullExclusions analysis.funcBranchCounts
          let priorities = calculateCoveragePriorities executedIds filteredTargets
          let uncovered = filter (\p => not p.isCovered) priorities
          putStrLn ""
          putStrLn $ "=== UNCOVERED High Impact Targets (filtered, top 10) ==="
          traverse_ (showPriorityNamed nameLookup) (take 10 uncovered)

      putStrLn ""
      putStrLn "Done."
  where
    showNamedTarget : (Nat -> Maybe String) -> FuncBranchCount -> IO ()
    showNamedTarget lookup f =
      let name = fromMaybe "?" (lookup f.funcIdx)
      in putStrLn $ "  func[" ++ show f.funcIdx ++ "] <" ++ name ++ ">: " ++
                   show f.branchCount ++ " branches"

    showFiltered : NamedFuncBranchCount -> IO ()
    showFiltered f = putStrLn $ "  " ++ show f

    showPriorityNamed : (Nat -> Maybe String) -> CoveragePriority -> IO ()
    showPriorityNamed lookup p =
      let name = fromMaybe "?" (lookup p.funcIdx)
      in putStrLn $ "  func[" ++ show p.funcIdx ++ "] <" ++ name ++ ">: " ++
                   show p.branchCount ++ " branches (priority " ++ show p.coveragePriority ++ ")"
