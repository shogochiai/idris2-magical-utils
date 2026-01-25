||| VLQ Encoder/Decoder Tests
module WasmBuilder.SourceMap.VLQTests

import Data.List
import Data.String
import WasmBuilder.SourceMap.VLQ

%default covering

-- =============================================================================
-- Test Definitions
-- =============================================================================

public export
record VLQTestDef where
  constructor MkVLQTestDef
  specId : String
  description : String
  run : () -> Bool

vlqTest : String -> String -> (() -> Bool) -> VLQTestDef
vlqTest sid desc fn = MkVLQTestDef sid desc fn

-- =============================================================================
-- Base64 Tests
-- =============================================================================

-- Base64 encode 0 -> 'A'
test_base64_0 : () -> Bool
test_base64_0 () = base64Encode 0 == 'A'

-- Base64 encode 25 -> 'Z'
test_base64_25 : () -> Bool
test_base64_25 () = base64Encode 25 == 'Z'

-- Base64 encode 26 -> 'a'
test_base64_26 : () -> Bool
test_base64_26 () = base64Encode 26 == 'a'

-- Base64 decode 'A' -> 0
test_base64_decode_A : () -> Bool
test_base64_decode_A () = base64Decode 'A' == Just 0

-- Base64 decode invalid -> Nothing
test_base64_decode_invalid : () -> Bool
test_base64_decode_invalid () = base64Decode '!' == Nothing

-- =============================================================================
-- VLQ Sign Conversion Tests
-- =============================================================================

-- Positive number: 1 -> 2 (LSB=0)
test_toVLQ_positive : () -> Bool
test_toVLQ_positive () = toVLQSigned 1 == 2

-- Negative number: -1 -> 3 (LSB=1)
test_toVLQ_negative : () -> Bool
test_toVLQ_negative () = toVLQSigned (-1) == 3

-- Zero: 0 -> 0
test_toVLQ_zero : () -> Bool
test_toVLQ_zero () = toVLQSigned 0 == 0

-- Roundtrip positive
test_vlq_roundtrip_positive : () -> Bool
test_vlq_roundtrip_positive () = fromVLQSigned (toVLQSigned 42) == 42

-- Roundtrip negative
test_vlq_roundtrip_negative : () -> Bool
test_vlq_roundtrip_negative () = fromVLQSigned (toVLQSigned (-42)) == -42

-- =============================================================================
-- VLQ Encoding Tests
-- =============================================================================

-- Encode 0 -> "A"
test_encode_0 : () -> Bool
test_encode_0 () = encodeVLQ 0 == "A"

-- Encode small positive
test_encode_small : () -> Bool
test_encode_small () = encodeVLQ 1 == "C"  -- 1*2=2, base64(2)='C'

-- Encode small negative
test_encode_small_neg : () -> Bool
test_encode_small_neg () = encodeVLQ (-1) == "D"  -- 1*2+1=3, base64(3)='D'

-- =============================================================================
-- VLQ Decoding Tests
-- =============================================================================

-- Decode "A" -> 0
test_decode_0 : () -> Bool
test_decode_0 () =
  case decodeVLQ "A" of
    Just (0, "") => True
    _ => False

-- Decode with remaining
test_decode_remaining : () -> Bool
test_decode_remaining () =
  case decodeVLQ "CABC" of
    Just (_, "ABC") => True
    _ => False

-- Roundtrip encode/decode
test_roundtrip_encode_decode : () -> Bool
test_roundtrip_encode_decode () =
  case decodeVLQ (encodeVLQ 42) of
    Just (42, "") => True
    _ => False

-- Roundtrip negative
test_roundtrip_negative : () -> Bool
test_roundtrip_negative () =
  case decodeVLQ (encodeVLQ (-15)) of
    Just (-15, "") => True
    _ => False

-- =============================================================================
-- Segment Tests
-- =============================================================================

-- Single segment encoding
test_single_segment : () -> Bool
test_single_segment () =
  let seg = MkSegment 0 0 0 0 (-1)
      (encoded, _, _, _, _) = encodeSegments 0 0 0 0 [seg]
  in strLength encoded > 0

-- =============================================================================
-- Test Runner
-- =============================================================================

||| All VLQ tests
export
allVLQTests : List VLQTestDef
allVLQTests =
  [ vlqTest "REQ_VLQ_BASE64_001" "Base64 encode 0" test_base64_0
  , vlqTest "REQ_VLQ_BASE64_002" "Base64 encode 25" test_base64_25
  , vlqTest "REQ_VLQ_BASE64_003" "Base64 encode 26" test_base64_26
  , vlqTest "REQ_VLQ_BASE64_004" "Base64 decode A" test_base64_decode_A
  , vlqTest "REQ_VLQ_BASE64_005" "Base64 decode invalid" test_base64_decode_invalid
  , vlqTest "REQ_VLQ_SIGN_001" "toVLQSigned positive" test_toVLQ_positive
  , vlqTest "REQ_VLQ_SIGN_002" "toVLQSigned negative" test_toVLQ_negative
  , vlqTest "REQ_VLQ_SIGN_003" "toVLQSigned zero" test_toVLQ_zero
  , vlqTest "REQ_VLQ_SIGN_004" "Roundtrip positive" test_vlq_roundtrip_positive
  , vlqTest "REQ_VLQ_SIGN_005" "Roundtrip negative" test_vlq_roundtrip_negative
  , vlqTest "REQ_VLQ_ENC_001" "Encode 0" test_encode_0
  , vlqTest "REQ_VLQ_ENC_002" "Encode small positive" test_encode_small
  , vlqTest "REQ_VLQ_ENC_003" "Encode small negative" test_encode_small_neg
  , vlqTest "REQ_VLQ_DEC_001" "Decode 0" test_decode_0
  , vlqTest "REQ_VLQ_DEC_002" "Decode with remaining" test_decode_remaining
  , vlqTest "REQ_VLQ_DEC_003" "Roundtrip encode/decode" test_roundtrip_encode_decode
  , vlqTest "REQ_VLQ_DEC_004" "Roundtrip negative" test_roundtrip_negative
  , vlqTest "REQ_VLQ_SEG_001" "Single segment encoding" test_single_segment
  ]

||| Run all VLQ tests
export
runVLQTests : (Nat, Nat)
runVLQTests =
  let results = map (\t => t.run ()) allVLQTests
      passed = length $ filter id results
      failed = length $ filter not results
  in (passed, failed)
