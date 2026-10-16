||| Contract tests for the canonical CoverageFamily (the coverage axis B).
module Idris2.CoverageFamily.Tests

import Idris2.CoverageFamily

%default total

-- coverageFamilyTag / coverageFamilyFromString round-trip over every family (the
-- tag is the lossless wire form on the coverage axis).
tagRoundTrips : Bool
tagRoundTrips =
  all (\cf => coverageFamilyFromString (coverageFamilyTag cf) == cf) allCoverageFamilies

-- every family yields a non-empty tag (a coverage family can never be the empty
-- string — a guard the no-silent-gap report relies on).
allTagsNonEmpty : Bool
allTagsNonEmpty = all (\cf => coverageFamilyTag cf /= "") allCoverageFamilies

-- the distinct tags are stable and not accidentally collapsed: web vs android-device
-- (the two android measurement mechanisms), humanoid and esp32-device are all separate.
distinctMechanisms : Bool
distinctMechanisms =
  coverageFamilyTag WebMVU == "web"
  && coverageFamilyTag AndroidDevice == "android-device"
  && coverageFamilyTag Humanoid == "humanoid"
  && coverageFamilyTag Esp32Device == "esp32-device"
  && WebMVU /= AndroidDevice
  && Esp32Device /= CoreLib

main : IO ()
main = do
  let checks = [ ("coverage-tag-round-trips", tagRoundTrips)
               , ("all-tags-non-empty", allTagsNonEmpty)
               , ("distinct-mechanisms", distinctMechanisms) ]
  _ <- traverse (\(n, ok) => putStrLn ((if ok then "  PASS " else "  FAIL ") ++ n)) checks
  putStrLn ("Passed " ++ show (length (filter snd checks)) ++ "/" ++ show (length checks))
