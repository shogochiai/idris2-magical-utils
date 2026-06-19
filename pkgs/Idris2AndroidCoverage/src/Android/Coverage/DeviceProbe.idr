||| Device-driven UI coverage for family=android MVU apps — the root cure for the
||| "acceptance hack" class of bugs.
|||
||| THE PROBLEM. android path-coverage today only measures the PURE Model/Msg/Update
||| layer (Msg-branch coverage). The View, the RN host (MainActivity), and the real
||| device are OUTSIDE coverage. So an agent can ship a real View.idr + bundle, yet
||| host it in a plain Activity that string-checks the bundle and paints a proof
||| TextView — `unzip|grep idris-bundle.js` and `adb install` pass while NONE of the
||| spec screen renders. Every patch (no hard-coded JSX → no placeholder bundle →
||| no proof TextView) is the same hole one layer deeper: nothing checks that the
||| screen the View DECLARES actually appears on a real device.
|||
||| THE CURE. Tie the View's declared screen surface to a device-driver scenario, and
||| require GapZero between them — exactly like Msg-branch parity, but for the screen:
|||   * A View is an RN.Node tree. Its `testID` props ARE its observable surface.
||| `viewTestIDs` extracts every testID the View declares.
|||   * A DeviceScenario is a list of Expect steps a driver runs on the real device
|||     (uiautomator dump → assert a testID is present / tap it / read its text).
|||   * `deviceGapZero` holds iff EVERY testID the View declares is exercised by the
|||     scenario AND (the runner enforces) every one was actually found on-device.
|||
||| A proof-TextView host has ZERO testIDs on screen, so it fails the on-device half;
||| a View that declares membership-card/thread-card/asset-card but whose scenario
||| skips one fails the GapZero half. The hack is unexpressible: to pass, the real
||| View must run on a real RN host and every declared element must appear and be
||| driven. The runner (run-device-e2e.sh) feeds the device dump back; this module is
||| the pure, testable contract it checks against.
module Android.Coverage.DeviceProbe

import RN.Node
import Data.List
import Data.Maybe

%default total

-- =============================================================================
-- The View's observable surface: the testIDs it declares
-- =============================================================================

||| The value of a `testID` prop on an attribute, if any. RN maps testID to the
||| Android resource-id / content-desc that `uiautomator dump` exposes, so this is
||| exactly what a device driver can observe.
attrTestID : Attr e -> Maybe String
attrTestID (Prop "testID" v) = Just v
attrTestID _                 = Nothing

-- viewTestIDs: every testID declared anywhere in a Node tree (the View's full
-- observable surface). A real screen exposes these to uiautomator; a proof TextView
-- exposes none of them. Mutual recursion with an explicit list helper so the totality
-- checker sees the structural descent (concatMap over `kids` hides it; viewTestIDsList
-- recurses on the spine and calls viewTestIDs on each strictly-smaller child).
mutual
  export
  viewTestIDs : Node e -> List String
  viewTestIDs (El _ attrs kids) =
    mapMaybe attrTestID attrs ++ viewTestIDsList kids
  viewTestIDs (Str _) = []   -- a text leaf carries no testID
  viewTestIDs Empty   = []   -- nothing rendered, nothing to observe

  viewTestIDsList : List (Node e) -> List String
  viewTestIDsList []        = []
  viewTestIDsList (n :: ns) = viewTestIDs n ++ viewTestIDsList ns

-- =============================================================================
-- The device-driver scenario: what the driver asserts on the real device
-- =============================================================================

||| One step a device driver performs against the running app. Parameterised by the
||| app's Msg type so a Tap is tied to the Msg it must dispatch — the View's Handler
||| and the scenario's Tap share the app's event space, so they cannot silently drift.
public export
data Expect : (e : Type) -> Type where
  ||| The element with this testID must be PRESENT in the on-device UI dump.
  Visible      : (testID : String) -> Expect e
  ||| The element must be present AND tapping it dispatches this Msg (the runner taps
  ||| it; the app's Update covers the Msg branch — closing the loop to Msg-parity).
  Tappable     : (testID : String) -> (onTap : e) -> Expect e
  ||| The element's rendered text must contain this substring (real data, not a label).
  TextContains : (testID : String) -> (needle : String) -> Expect e

||| The testID a step targets.
export
expectTestID : Expect e -> String
expectTestID (Visible t)        = t
expectTestID (Tappable t _)     = t
expectTestID (TextContains t _) = t

||| A device scenario: the ordered steps a driver runs on the real device. This is
||| the E2E definition the runner executes via `adb shell uiautomator dump` + input.
public export
record DeviceScenario (e : Type) where
  constructor MkScenario
  ||| Human label (shown in the coverage report).
  label : String
  ||| The steps. Their testIDs are the screen surface the scenario exercises.
  steps : List (Expect e)

||| Every testID a scenario exercises.
export
scenarioTestIDs : DeviceScenario e -> List String
scenarioTestIDs s = map expectTestID s.steps

-- =============================================================================
-- GapZero: the View's declared surface must be fully exercised by the scenario
-- =============================================================================

||| testIDs the View declares that NO scenario step exercises — the device-coverage
||| gap. Empty list ⟺ GapZero. This is the screen analogue of "Missing paths".
export
uncoveredTestIDs : Node e -> DeviceScenario e -> List String
uncoveredTestIDs view scenario =
  let declared = nub (viewTestIDs view)
      driven   = scenarioTestIDs scenario
  in filter (\t => not (elem t driven)) declared

||| GapZero for device coverage: every testID the View declares is exercised by the
||| scenario. The RUNNER additionally requires each was found on the real device, so
||| passing this AND the runner's on-device check is impossible for a proof-TextView
||| host (no testIDs on screen) or a View that declares an element it never drives.
export
deviceGapZero : Node e -> DeviceScenario e -> Bool
deviceGapZero view scenario = isNil (uncoveredTestIDs view scenario)
  where
    isNil : List a -> Bool
    isNil [] = True
    isNil _  = False

||| A coverage report mirroring the parity-ti "Missing paths" shape, so the android
||| step4/parity layer can consume it uniformly. `missing` empty ⟺ admissible.
public export
record DeviceCoverage where
  constructor MkDeviceCoverage
  totalElements   : Nat   -- distinct testIDs the View declares
  coveredElements : Nat   -- of those, exercised by the scenario
  missing         : List String

export
deviceCoverage : Node e -> DeviceScenario e -> DeviceCoverage
deviceCoverage view scenario =
  let declared = nub (viewTestIDs view)
      missing  = uncoveredTestIDs view scenario
  in MkDeviceCoverage
       (length declared)
       (length declared `minus` length missing)
       missing
