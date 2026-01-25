||| ERC-7546 Upgrade Detection API
|||
||| Detects dictionary changes by comparing snapshots.
||| Used by lazy evm-lifecycle ask for pending upgrade detection.
|||
||| Reference: https://eips.ethereum.org/EIPS/eip-7546
module Subcontract.Standards.ERC7546.UpgradeDetection

import public EVM.Primitives
import Subcontract.Standards.ERC7546.Slots
import Subcontract.Standards.ERC7546.Analysis

import Data.List
import Data.Maybe
import Data.String

-- =============================================================================
-- Change Detection Types
-- =============================================================================

||| Represents a change in implementation
public export
data ChangeType
  = Added       -- New selector added
  | Removed     -- Selector removed (impl set to 0)
  | Changed     -- Implementation changed to different address
  | Unchanged   -- No change

public export
Eq ChangeType where
  Added == Added = True
  Removed == Removed = True
  Changed == Changed = True
  Unchanged == Unchanged = True
  _ == _ = False

public export
Show ChangeType where
  show Added = "ADDED"
  show Removed = "REMOVED"
  show Changed = "CHANGED"
  show Unchanged = "UNCHANGED"

||| A detected change in a selector's implementation
public export
record ImplementationChange where
  constructor MkImplChange
  ||| Function selector
  selector : Integer
  ||| Type of change
  changeType : ChangeType
  ||| Old implementation address (0 if Added)
  oldImpl : Integer
  ||| New implementation address (0 if Removed)
  newImpl : Integer
  ||| Whether new implementation has code
  newHasCode : Bool

public export
Show ImplementationChange where
  show c = unlines
    [ "Change [" ++ show c.changeType ++ "] 0x" ++ show c.selector
    , "  From: 0x" ++ show c.oldImpl
    , "  To:   0x" ++ show c.newImpl ++ (if c.newHasCode then "" else " [NO CODE!]")
    ]

||| Result of upgrade detection
public export
record UpgradeDetectionResult where
  constructor MkUpgradeDetection
  ||| Baseline snapshot
  baseline : DictionarySnapshot
  ||| Current snapshot
  current : DictionarySnapshot
  ||| All detected changes
  changes : List ImplementationChange
  ||| Number of unchanged selectors
  unchangedCount : Nat
  ||| Whether any upgrade is pending/detected
  hasPendingUpgrade : Bool
  ||| Whether any change is dangerous (zombie reference)
  hasDangerousChange : Bool

public export
Show UpgradeDetectionResult where
  show r = unlines
    [ "=== Upgrade Detection Result ==="
    , "Baseline: block " ++ show r.baseline.blockNum
    , "Current:  block " ++ show r.current.blockNum
    , "Changes:  " ++ show (length r.changes)
    , "Unchanged: " ++ show r.unchangedCount
    , "Pending:  " ++ (if r.hasPendingUpgrade then "YES" else "NO")
    , "Dangerous: " ++ (if r.hasDangerousChange then "YES" else "NO")
    ]

-- =============================================================================
-- Detection Functions
-- =============================================================================

||| Compare a single selector between two snapshots
detectSelectorChange : Integer -> DictionarySnapshot -> DictionarySnapshot -> IO ImplementationChange
detectSelectorChange sel baseline current = do
  let baseImpl = fromMaybe 0 $ lookupInSnapshot sel baseline
  let currImpl = fromMaybe 0 $ lookupInSnapshot sel current

  -- Determine change type
  let chgType =
        if baseImpl == 0 && currImpl /= 0 then Added
        else if baseImpl /= 0 && currImpl == 0 then Removed
        else if baseImpl /= currImpl then Changed
        else Unchanged

  -- Check if new impl has code (only if not removed)
  hasCode <- if currImpl == 0 then pure True else addressHasCode currImpl

  pure $ MkImplChange
    { selector = sel
    , changeType = chgType
    , oldImpl = baseImpl
    , newImpl = currImpl
    , newHasCode = hasCode
    }
  where
    lookupInSnapshot : Integer -> DictionarySnapshot -> Maybe Integer
    lookupInSnapshot s snap =
      case filter (\e => e.selector == s) snap.implementations of
        (e :: _) => Just e.implAddr
        [] => Nothing

||| Detect all changes between baseline and current snapshots
export
detectChanges : DictionarySnapshot -> DictionarySnapshot -> IO (List ImplementationChange)
detectChanges baseline current = do
  -- Get all selectors from both snapshots
  let baseSelectors = map selector baseline.implementations
  let currSelectors = map selector current.implementations
  let allSelectors = nub (baseSelectors ++ currSelectors)

  -- Detect changes for each selector
  traverse (detectChange' baseline current) allSelectors
  where
    detectChange' : DictionarySnapshot -> DictionarySnapshot -> Integer -> IO ImplementationChange
    detectChange' b c s = detectSelectorChange s b c

||| Filter to only actual changes (not Unchanged)
filterActualChanges : List ImplementationChange -> List ImplementationChange
filterActualChanges = filter (\c => c.changeType /= Unchanged)

||| Perform full upgrade detection between two snapshots
export
detectUpgrades : DictionarySnapshot -> DictionarySnapshot -> IO UpgradeDetectionResult
detectUpgrades baseline current = do
  allChanges <- detectChanges baseline current
  let actualChanges = filterActualChanges allChanges
  let unchangedCnt = length allChanges `minus` length actualChanges
  let hasDangerous = any (\c => not c.newHasCode && c.changeType /= Removed) actualChanges

  pure $ MkUpgradeDetection
    { baseline = baseline
    , current = current
    , changes = actualChanges
    , unchangedCount = unchangedCnt
    , hasPendingUpgrade = not (null actualChanges)
    , hasDangerousChange = hasDangerous
    }

-- =============================================================================
-- Live Detection Functions
-- =============================================================================

||| Take current snapshot and compare with baseline
export
detectPendingUpgrades : Integer -> DictionarySnapshot -> IO UpgradeDetectionResult
detectPendingUpgrades dictAddr baseline = do
  let selectors = map selector baseline.implementations
  current <- takeSnapshot dictAddr selectors
  detectUpgrades baseline current

||| Check if a specific selector's implementation changed
export
selectorChanged : Integer -> Integer -> Integer -> IO Bool
selectorChanged dictAddr sel expectedImpl = do
  currentImpl <- queryImplementation dictAddr sel
  pure (currentImpl /= expectedImpl)

||| Verify an upgrade was successfully applied
export
verifyUpgradeApplied : Integer -> Integer -> Integer -> IO Bool
verifyUpgradeApplied dictAddr sel expectedNewImpl = do
  currentImpl <- queryImplementation dictAddr sel
  if currentImpl == expectedNewImpl
    then do
      -- Also verify the implementation has code
      hasCode <- addressHasCode currentImpl
      pure hasCode
    else pure False

-- =============================================================================
-- Danger Detection
-- =============================================================================

||| Find all zombie references (implementations without code)
export
findZombieReferences : DictionarySnapshot -> IO (List ImplEntry)
findZombieReferences snapshot = do
  zombies <- traverse checkZombie snapshot.implementations
  pure $ catMaybes zombies
  where
    checkZombie : ImplEntry -> IO (Maybe ImplEntry)
    checkZombie entry = do
      if entry.implAddr == 0
        then pure Nothing
        else do
          hasCode <- addressHasCode entry.implAddr
          pure $ if hasCode then Nothing else Just entry

||| Check if owner changed between snapshots
export
ownerChanged : DictionarySnapshot -> DictionarySnapshot -> Bool
ownerChanged baseline current = baseline.ownerAddr /= current.ownerAddr
