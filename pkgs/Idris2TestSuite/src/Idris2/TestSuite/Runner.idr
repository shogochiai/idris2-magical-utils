||| The standard test-suite entry point: read the chunk window from the
||| environment and run that slice. The coverage tool builds the test exe ONCE and
||| runs it N times with successive IDRIS2COV_TEST_OFFSET / IDRIS2COV_TEST_LIMIT
||| windows — each a fresh, memory-bounded process (intra-module chunking).
|||
||| A module's generated/handwritten `main`/`runAllTests` calls `runTestSuiteMain
||| allTests`: with no env set it runs the whole suite (unchanged behaviour for
||| small suites and direct runs); with the env set it runs only that window.
module Idris2.TestSuite.Runner

import Idris2.TestSuite
import System
import System.File
import Data.String
import Data.Maybe

%default total

||| Parse a Nat from an env var; Nothing if unset/blank/unparsable.
envNat : String -> IO (Maybe Nat)
envNat name = do
  mv <- getEnv name
  pure $ case mv of
    Nothing => Nothing
    Just s  => case parsePositive {a = Nat} (trim s) of
                 Just n  => Just n
                 Nothing => Nothing

||| Run the suite slice selected by IDRIS2COV_TEST_OFFSET / _LIMIT (both optional),
||| print the canonical `Results: P passed, F failed` line the coverage producers
||| parse, and exit non-zero on any failure. This IS the universal test main.
export covering
runTestSuiteMain : TestSuite -> IO ()
runTestSuiteMain suite = do
  offset <- envNat "IDRIS2COV_TEST_OFFSET"
  limit  <- envNat "IDRIS2COV_TEST_LIMIT"
  let selected = sliceSuite (fromMaybe 0 offset) limit suite
  (passed, failed) <- runSuite selected
  putStrLn ""
  putStrLn $ "Results: " ++ show passed ++ " passed, " ++ show failed ++ " failed"
  if failed > 0 then exitFailure else exitSuccess
