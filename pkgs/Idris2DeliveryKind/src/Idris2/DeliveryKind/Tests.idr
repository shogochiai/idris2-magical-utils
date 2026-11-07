||| Contract tests for the canonical DeliveryKind.
module Idris2.DeliveryKind.Tests

import Idris2.DeliveryKind

%default total

-- deliveryTag round-trips (lossless wire form); coverageFamilyOf does NOT (lossy).
tagRoundTrips : Bool
tagRoundTrips =
  all (\d => deliveryKindFromString (deliveryTag d) == d) allDeliveryKinds

-- coverage maps: android→android-device (real-device ONLY, no node surrogate),
-- iOS→web, cli→core; delivery tags stay distinct. coverageFamilyOf lands on the typed
-- CoverageFamily; coverageFamilyTagOf is the string projection the old callers used.
coverageMapsPreserved : Bool
coverageMapsPreserved =
  coverageFamilyOf Android == AndroidDevice
  && coverageFamilyOf IOS == WebMVU
  && coverageFamilyOf CLI == CoreLib
  && coverageFamilyTagOf Android == "android-device"
  && coverageFamilyTagOf CLI == "core"
  && deliveryTag Android == "android"
  && deliveryTag CLI == "cli"
  && coverageFamilyOf (Humanoid Nothing) == Humanoid
  && coverageFamilyOf (Esp32 Nothing) == Esp32Device

-- the embodiment extension axis.
humanoidExtension : Bool
humanoidExtension =
  Humanoid (Just (MkHumanoidConfig "unit-7" "atlas")) == Humanoid Nothing
  && elem (Humanoid Nothing) allDeliveryKinds
  && deliveryTag (Humanoid Nothing) == "humanoid"

-- the device extension axis: the board rides in the payload, so two boards are the
-- same DeliveryKind, and the wire tag round-trips.
esp32Extension : Bool
esp32Extension =
  Esp32 (Just (MkEsp32Config "esp32-s3-box-3" "esp32s3" "/dev/cu.usbmodem1101")) == Esp32 Nothing
  && elem (Esp32 Nothing) allDeliveryKinds
  && deliveryTag (Esp32 Nothing) == "esp32-s3-box"
  && deliveryKindFromString "esp32-s3-box" == Esp32 Nothing

main : IO ()
main = do
  let checks = [ ("tag-round-trips", tagRoundTrips)
               , ("coverage-maps-preserved", coverageMapsPreserved)
               , ("humanoid-extension", humanoidExtension)
               , ("esp32-extension", esp32Extension) ]
  _ <- traverse (\(n, ok) => putStrLn ((if ok then "  PASS " else "  FAIL ") ++ n)) checks
  putStrLn ("Passed " ++ show (length (filter snd checks)) ++ "/" ++ show (length checks))
