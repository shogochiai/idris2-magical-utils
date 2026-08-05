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

||| Read a project exclusion file. Format mirrors the dfx side's
||| coverage-exclusions.txt: one entry per line, `# ...` and blank lines ignored,
||| an optional `# reason` tail stripped. Kept deliberately simple — a plain
||| function-name fragment, because the thing being excluded here is "this
||| function cannot run on a device without mutating a live canister", which is
||| a property of the function, not of a path id.
|||
||| A missing or unreadable file yields [] rather than an error: a target with no
||| exclusions must measure exactly as it did before this existed.
readExclusionFragments : String -> IO (List String)
readExclusionFragments path = do
  Right content <- readFile path
    | Left _ => pure []
  pure $ mapMaybe fragmentOf (lines content)
  where
    fragmentOf : String -> Maybe String
    fragmentOf raw =
      let noComment = pack (fst (break (== '#') (unpack raw)))
          frag      = trim noComment
      in if frag == "" then Nothing else Just frag

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
      extra <- case rest of
                 (_ :: exclF :: _) => readExclusionFragments exclF
                 _                 => pure []
      let cov = pathCoverageWith extra denom hits modPrefix
      putStr (report cov)
      if cov.missing == [] then pure () else exitFailure
    _ => do
      putStrLn "Usage: device-pathcov <denom-ids-file> <hit-ids-file> [module-prefix] [exclusions-file]"
      exitFailure
