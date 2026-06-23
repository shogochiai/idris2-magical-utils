||| Contract tests for the coverage family registry (the totality anchor).
module Coverage.Plan.Tests

import Data.List

import Idris2.CoverageFamily
import Coverage.Core.Backend
import Coverage.Plan.Registry

%default total

||| NO family is silently coverage-less: every family resolves to a CoverageStatus,
||| and every DeclaredUnimplemented carries a NON-EMPTY rationale. This is the
||| behavioral backstop to the type-level guarantee — even if a future agent sneaks a
||| wildcard into coverageStatusFor to dodge the totality error, this test over
||| `allCoverageFamilies` forces a real status with a real reason. There is no
||| representable "silent Nothing".
test_no_silent_gap : Bool
test_no_silent_gap = all ok allCoverageFamilies
  where
    ok : CoverageFamily -> Bool
    ok cf = case coverageStatusFor cf of
              Implemented _           => True
              DeclaredUnimplemented r => r.rationale /= ""   -- empty rationale = silent gap = FAIL

||| The five mechanised families are Implemented; Humanoid is the one declared
||| unimplemented (its hit-stream is not yet specified) — a KNOWN, reported gap.
test_humanoid_declared : Bool
test_humanoid_declared =
  not (isImplemented (coverageStatusFor Humanoid))
  && isImplemented (coverageStatusFor WebMVU)
  && isImplemented (coverageStatusFor EvmHash)
  && isImplemented (coverageStatusFor DfxWasm)
  && isImplemented (coverageStatusFor AndroidDevice)
  && isImplemented (coverageStatusFor CoreLib)

main : IO ()
main = do
  let checks = [ ("no-silent-gap", test_no_silent_gap)
               , ("humanoid-declared-unimplemented", test_humanoid_declared) ]
  _ <- traverse (\(n, ok) => putStrLn ((if ok then "  PASS " else "  FAIL ") ++ n)) checks
  putStrLn ("Passed " ++ show (length (filter snd checks)) ++ "/" ++ show (length checks))
