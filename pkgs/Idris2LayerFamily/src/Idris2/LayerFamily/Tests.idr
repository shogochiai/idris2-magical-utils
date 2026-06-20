||| Contract tests for the canonical LayerFamily.
module Idris2.LayerFamily.Tests

import Idris2.LayerFamily

%default total

-- familyTag round-trips through layerFamilyFromString (it is the lossless wire form);
-- familyName does NOT (it is lossy — "web" could be Web or Android, by design).
tagRoundTrips : Bool
tagRoundTrips =
  all (\f => layerFamilyFromString (familyTag f) == f)
      [EVM, ICP Nothing, CLI, Web, Android, Humanoid Nothing, Core]

-- the non-1:1 coverage maps are preserved (android→web, cli→core), and each family's
-- OWN release tag is distinct (android≠web, cli≠core).
coverageMapsPreserved : Bool
coverageMapsPreserved =
  familyName Android == "web"      -- coverage parity with web
  && familyName CLI == "core"
  && familyTag Android == "android"  -- but its release tag is its own
  && familyTag CLI == "cli"
  && familyName (Humanoid Nothing) == "humanoid"

-- the embodiment extension axis: Humanoid carries its own layer-specific payload, and
-- the payload distinguishes nothing in family equality (families compare by constructor).
humanoidExtension : Bool
humanoidExtension =
  Humanoid (Just (MkHumanoidConfig "unit-7" "atlas")) == Humanoid Nothing  -- ctor-equal
  && elem (Humanoid Nothing) allLayerFamilies
  && familyTag (Humanoid Nothing) == "humanoid"

main : IO ()
main = do
  let checks = [ ("tag-round-trips", tagRoundTrips)
               , ("coverage-maps-preserved", coverageMapsPreserved)
               , ("humanoid-extension", humanoidExtension)
               ]
  _ <- traverse (\(n, ok) => putStrLn ((if ok then "  PASS " else "  FAIL ") ++ n)) checks
  let passed = length (filter snd checks)
  putStrLn ("Passed " ++ show passed ++ "/" ++ show (length checks))
