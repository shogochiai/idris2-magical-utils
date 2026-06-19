||| Tests for Android.Coverage.DeviceProbe — proving the device-coverage contract
||| makes the "acceptance hack" class of bugs unrepresentable.
module Android.Coverage.Tests.AllTests

import Android.Coverage.DeviceProbe
import RN.Node
import Data.List
import Data.String
import System

%default total

-- A tiny app Msg, like a real MVU app's.
data Msg = VoteYes | VoteNo | SelectNft

-- Helpers to build Nodes with a testID, mirroring how a real View.idr is written.
tid : String -> Attr Msg
tid t = Prop "testID" t

card : String -> List (Node Msg) -> Node Msg
card t kids = El "View" [ tid t ] kids

label : String -> Node Msg
label s = El "Text" [] [ Str s ]

btn : String -> Msg -> Node Msg
btn t m = El "Pressable" [ tid t, Handler "onPress" m ] [ label "tap" ]

-- =============================================================================
-- A REAL View: declares the spec's screen surface (the four cards + a vote button).
-- =============================================================================
realView : Node Msg
realView =
  card "spcdao-root"
    [ card "membership-card" [ label "membership NFT status" ]
    , card "thread-card"
        [ label "instance 10 thread"
        , btn "vote-yes" VoteYes ]
    , card "asset-card" [ label "registered assets" ]
    ]

-- A scenario that drives EVERY declared element → GapZero.
fullScenario : DeviceScenario Msg
fullScenario = MkScenario "full"
  [ Visible "spcdao-root"
  , Visible "membership-card"
  , TextContains "thread-card" "instance 10"
  , Tappable "vote-yes" VoteYes
  , Visible "asset-card"
  ]

-- A scenario that SKIPS asset-card → a real coverage gap.
partialScenario : DeviceScenario Msg
partialScenario = MkScenario "partial"
  [ Visible "spcdao-root"
  , Visible "membership-card"
  , Visible "thread-card"
  , Tappable "vote-yes" VoteYes
  ]

-- =============================================================================
-- A HOLLOW View: the proof-TextView host. It renders ONE text node with NO testIDs
-- — exactly what uiautomator finds on the hack screen. Its observable surface is
-- empty, so it can satisfy NO scenario that asserts the spec elements.
-- =============================================================================
hollowView : Node Msg
hollowView = El "TextView" [] [ Str "Membership NFT status: from Idris MVU bundle" ]

-- =============================================================================
-- Tests
-- =============================================================================

||| REQ_ANDCOV_001: the real View's declared surface is exactly its four testID'd
||| cards plus the vote button (5 elements).
test_REQ_ANDCOV_001_view_surface_extracted : Bool
test_REQ_ANDCOV_001_view_surface_extracted =
  sort (nub (viewTestIDs realView))
    == sort ["spcdao-root","membership-card","thread-card","vote-yes","asset-card"]

||| REQ_ANDCOV_002: a scenario that drives every declared element is GapZero.
test_REQ_ANDCOV_002_full_scenario_gapzero : Bool
test_REQ_ANDCOV_002_full_scenario_gapzero =
  deviceGapZero realView fullScenario

||| REQ_ANDCOV_003: a scenario that skips a declared element is NOT GapZero, and the
||| skipped element is named in the missing list (the screen "Missing paths").
test_REQ_ANDCOV_003_partial_scenario_reports_gap : Bool
test_REQ_ANDCOV_003_partial_scenario_reports_gap =
  not (deviceGapZero realView partialScenario)
    && uncoveredTestIDs realView partialScenario == ["asset-card"]

||| REQ_ANDCOV_004: the proof-TextView host declares ZERO observable testIDs — so a
||| device driver finds none of the spec elements. This is the structural reason the
||| hack cannot pass: the runner asserts each scenario testID is present on-device,
||| and a hollow screen exposes none. (Pure side: its surface is empty.)
test_REQ_ANDCOV_004_hollow_host_has_no_surface : Bool
test_REQ_ANDCOV_004_hollow_host_has_no_surface =
  viewTestIDs hollowView == []

||| REQ_ANDCOV_005: coverage report shape mirrors parity-ti — total/covered/missing.
test_REQ_ANDCOV_005_coverage_report : Bool
test_REQ_ANDCOV_005_coverage_report =
  let c = deviceCoverage realView partialScenario
  in totalElements c == 5
     && coveredElements c == 4
     && missing c == ["asset-card"]

-- Runner -----------------------------------------------------------------------

allTests : List (String, Bool)
allTests =
  [ ("REQ_ANDCOV_001_view_surface_extracted",      test_REQ_ANDCOV_001_view_surface_extracted)
  , ("REQ_ANDCOV_002_full_scenario_gapzero",       test_REQ_ANDCOV_002_full_scenario_gapzero)
  , ("REQ_ANDCOV_003_partial_scenario_reports_gap",test_REQ_ANDCOV_003_partial_scenario_reports_gap)
  , ("REQ_ANDCOV_004_hollow_host_has_no_surface",  test_REQ_ANDCOV_004_hollow_host_has_no_surface)
  , ("REQ_ANDCOV_005_coverage_report",             test_REQ_ANDCOV_005_coverage_report)
  ]

covering
main : IO ()
main = do
  let results = allTests
  let passed = length (filter snd results)
  for_ results $ \(name, ok) =>
    putStrLn $ (if ok then "  PASS " else "  FAIL ") ++ name
  putStrLn $ "Passed " ++ show passed ++ "/" ++ show (length results)
  if passed == length results then pure () else exitFailure
