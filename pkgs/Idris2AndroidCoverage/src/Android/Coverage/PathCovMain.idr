||| device-pathcov — the Idris2 entry point that computes device path coverage.
|||
||| The bash harness does ONLY raw I/O (collect dumppaths path_ids and on-device hit
||| path_ids, one id per line), then calls this. All coverage math is in Idris2
||| (Android.Coverage.PathCoverage), keeping lazy/etherclaw portable — no python/jq.
|||
|||   device-pathcov <denom-ids-file> <hit-ids-file> [module-prefix] [exclusions-file]
|||
||| denom-ids-file : one dumppaths path_id per line (denominator, pre-exclusion).
||| hit-ids-file   : one on-device hit path_id per line (numerator).
||| module-prefix  : keep only the app's own functions (e.g. "SpcDaoApp"); "" = all.
||| Prints the parity-ti-shaped report to stdout; exits 0 iff Missing paths: 0.
module Android.Coverage.PathCovMain

import Android.Coverage.PathCoverage
import Coverage.Core.Exclusions
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

||| Read a project exclusion file (Android.Coverage.PathCoverage.projectPatterns:
||| the dfx line format, a bare line being a function-name prefix).
|||
||| An EMPTY path means "no file": a target with no exclusions measures exactly
||| as it did before this existed. A NAMED file that cannot be read is an error,
||| not []: silently measuring without the file is how android went unexcluded
||| for months (alice #4467 — lazy never passed it, and nothing said so).
readProjectPatterns : String -> IO (Either String (List ExclPattern))
readProjectPatterns "" = pure (Right [])
readProjectPatterns path = do
  Right content <- readFile path
    | Left err => pure (Left ("cannot read exclusions file " ++ path ++ ": " ++ show err))
  pure (Right (projectPatterns content))

main : IO ()
main = do
  args <- getArgs
  case args of
    (_ :: denomF :: hitF :: rest) => do
      let modPrefix = case rest of (p :: _) => p; [] => ""
      denom <- readIds denomF
      hits  <- readIds hitF
      -- Project-supplied exclusions, if the caller named a file. Same role as
      -- GlobalRegistry/coverage-exclusions.txt on the dfx side; absent file →
      -- empty list → byte-identical to the previous behaviour.
      let exclF = case rest of
                    (_ :: f :: _) => f
                    _             => ""
      Right extra <- readProjectPatterns exclF
        | Left err => do putStrLn ("device-pathcov: " ++ err); exitFailure
      let cov = pathCoverageWith extra denom hits modPrefix
      putStr (report cov)
      unless (exclF == "") $
        putStrLn ("exclusions_file: " ++ exclF ++ " (" ++ show (length extra) ++ " pattern(s))")
      -- Exit on ADMISSIBILITY, not on completeness. This producer emits raw
      -- evidence; whether the coverage clears a bar is the consumer's call
      -- against its family threshold (Android = 95.0). Exiting nonzero for
      -- "not 100%" made every real device run a failure and left no exit code
      -- free to mean "this run measured nothing" — the one condition the
      -- caller genuinely cannot recover from, and the one a false-zero
      -- numerator produces. See `claimAdmissible`.
      if claimAdmissible cov then pure () else exitFailure
    _ => do
      putStrLn "Usage: device-pathcov <denom-ids-file> <hit-ids-file> [module-prefix] [exclusions-file]"
      exitFailure
