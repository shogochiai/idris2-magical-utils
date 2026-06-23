||| Contract tests for the canonical DeliveryKind.
module Idris2.DeliveryKind.Tests

import Idris2.DeliveryKind

%default total

-- deliveryTag round-trips (lossless wire form); coverageFamilyOf does NOT (lossy).
tagRoundTrips : Bool
tagRoundTrips =
  all (\d => deliveryKindFromString (deliveryTag d) == d) allDeliveryKinds

-- coverage maps preserved (android→web, cli→core), delivery tags stay distinct.
-- coverageFamilyOf now lands on the typed CoverageFamily; coverageFamilyTagOf is the
-- string projection that the old callers used.
coverageMapsPreserved : Bool
coverageMapsPreserved =
  coverageFamilyOf Android == WebMVU
  && coverageFamilyOf CLI == CoreLib
  && coverageFamilyTagOf Android == "web"
  && coverageFamilyTagOf CLI == "core"
  && deliveryTag Android == "android"
  && deliveryTag CLI == "cli"
  && coverageFamilyOf (Humanoid Nothing) == Humanoid

-- the embodiment extension axis.
humanoidExtension : Bool
humanoidExtension =
  Humanoid (Just (MkHumanoidConfig "unit-7" "atlas")) == Humanoid Nothing
  && elem (Humanoid Nothing) allDeliveryKinds
  && deliveryTag (Humanoid Nothing) == "humanoid"

main : IO ()
main = do
  let checks = [ ("tag-round-trips", tagRoundTrips)
               , ("coverage-maps-preserved", coverageMapsPreserved)
               , ("humanoid-extension", humanoidExtension) ]
  _ <- traverse (\(n, ok) => putStrLn ((if ok then "  PASS " else "  FAIL ") ++ n)) checks
  putStrLn ("Passed " ++ show (length (filter snd checks)) ++ "/" ++ show (length checks))
