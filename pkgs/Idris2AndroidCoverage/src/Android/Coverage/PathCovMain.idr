||| device-pathcov — the Idris2 entry point that computes device path coverage.
|||
||| The bash harness does ONLY raw I/O (collect dumppaths path_ids and on-device hit
||| path_ids, one id per line), then calls this. All coverage math is in Idris2
||| (Android.Coverage.PathCoverage), keeping lazy/etherclaw portable — no python/jq.
|||
|||   device-pathcov <denom-ids-file> <hit-ids-file> [module-prefix]
|||
||| denom-ids-file : one dumppaths path_id per line (denominator, pre-exclusion).
||| hit-ids-file   : one on-device hit path_id per line (numerator).
||| module-prefix  : keep only the app's own functions (e.g. "SpcDaoApp"); "" = all.
||| Prints the parity-ti-shaped report to stdout; exits 0 iff Missing paths: 0.
module Android.Coverage.PathCovMain

import Android.Coverage.PathCoverage
import Data.String
import System
import System.File

%default covering

nonEmptyLines : String -> List String
nonEmptyLines s = filter (/= "") (map trim (lines s))

readIds : String -> IO (List String)
readIds path = do
  Right content <- readFile path
    | Left _ => pure []
  pure (nonEmptyLines content)

main : IO ()
main = do
  args <- getArgs
  case args of
    (_ :: denomF :: hitF :: rest) => do
      let modPrefix = case rest of (p :: _) => p; [] => ""
      denom <- readIds denomF
      hits  <- readIds hitF
      let cov = pathCoverage denom hits modPrefix
      putStr (report cov)
      if cov.missing == [] then pure () else exitFailure
    _ => do
      putStrLn "Usage: device-pathcov <denom-ids-file> <hit-ids-file> [module-prefix]"
      exitFailure
