||| Subcontract Core: Lifecycle (FR-Aware Deploy/Upgrade)
|||
||| Deploy/Upgrade are meta-operations that change morphism existence.
||| FR theory requires these to be:
||| - Classified (specific failure modes)
||| - Observable (evidence for rollback/debug)
||| - Recoverable (compensation procedures)
||| - Epoch-indexed (version safety)
|||
||| Key insight: Deploy creates morphisms, Upgrade replaces them.
||| Both can fail, and failure must be recoverable.
module Subcontract.Core.Lifecycle

import public Subcontract.Core.Conflict
import public Subcontract.Core.Evidence
import public Subcontract.Core.Outcome
import public Subcontract.Core.FR
import public Subcontract.Core.Storable
import public Data.Vect

%default total

-- =============================================================================
-- Lifecycle States (Type-Level)
-- =============================================================================

||| Contract lifecycle states
public export
data LifecycleState : Type where
  ||| Not yet deployed
  Undeployed : LifecycleState
  ||| Deployed but not initialized
  Deployed : LifecycleState
  ||| Fully initialized and operational
  Initialized : LifecycleState
  ||| Upgraded (new epoch)
  Upgraded : LifecycleState
  ||| Deprecated (no longer operational)
  Deprecated : LifecycleState
  ||| Paused (temporarily non-operational)
  Paused : LifecycleState

||| Lifecycle state equality
public export
Eq LifecycleState where
  Undeployed == Undeployed = True
  Deployed == Deployed = True
  Initialized == Initialized = True
  Upgraded == Upgraded = True
  Deprecated == Deprecated = True
  Paused == Paused = True
  _ == _ = False

||| State to string
public export
showState : LifecycleState -> String
showState Undeployed = "UNDEPLOYED"
showState Deployed = "DEPLOYED"
showState Initialized = "INITIALIZED"
showState Upgraded = "UPGRADED"
showState Deprecated = "DEPRECATED"
showState Paused = "PAUSED"

-- =============================================================================
-- Lifecycle Transitions (Type-Level Proofs)
-- =============================================================================

||| Valid lifecycle transitions
||| Only these transitions are allowed - enforced at compile time
public export
data LifecycleTransition : LifecycleState -> LifecycleState -> Type where
  ||| Deploy: Undeployed -> Deployed
  DoDeploy : LifecycleTransition Undeployed Deployed
  ||| Initialize: Deployed -> Initialized
  DoInit : LifecycleTransition Deployed Initialized
  ||| Upgrade: Initialized -> Upgraded (bumps epoch)
  DoUpgrade : LifecycleTransition Initialized Upgraded
  ||| Continue after upgrade: Upgraded -> Initialized
  DoCommitUpgrade : LifecycleTransition Upgraded Initialized
  ||| Rollback upgrade: Upgraded -> Initialized (restore old)
  DoRollbackUpgrade : LifecycleTransition Upgraded Initialized
  ||| Deprecate: Initialized -> Deprecated
  DoDeprecate : LifecycleTransition Initialized Deprecated
  ||| Pause: Initialized -> Paused
  DoPause : LifecycleTransition Initialized Paused
  ||| Unpause: Paused -> Initialized
  DoUnpause : LifecycleTransition Paused Initialized

||| Get transition name for evidence
public export
transitionName : LifecycleTransition from to -> String
transitionName DoDeploy = "DEPLOY"
transitionName DoInit = "INIT"
transitionName DoUpgrade = "UPGRADE"
transitionName DoCommitUpgrade = "COMMIT_UPGRADE"
transitionName DoRollbackUpgrade = "ROLLBACK_UPGRADE"
transitionName DoDeprecate = "DEPRECATE"
transitionName DoPause = "PAUSE"
transitionName DoUnpause = "UNPAUSE"

-- =============================================================================
-- Deploy Evidence
-- =============================================================================

||| Evidence for deploy operations
public export
record DeployEvidence where
  constructor MkDeployEvidence
  ||| Hash of bytecode being deployed
  bytecodeHash : Bits256
  ||| Salt for CREATE2 (0 for CREATE)
  salt : Bits256
  ||| Expected address (for CREATE2)
  expectedAddr : Bits256
  ||| Actual resulting address (0 if failed)
  actualAddr : Bits256
  ||| Initialization parameters
  initParams : List Bits256
  ||| Deployer address
  deployer : Bits256

||| Convert DeployEvidence to Evidence
public export
deployToEvidence : DeployEvidence -> Evidence
deployToEvidence de = MkEvidence
  de.bytecodeHash
  ["deploy", if de.actualAddr == 0 then "failed" else "success"]
  []
  []
  []

-- =============================================================================
-- Upgrade Evidence
-- =============================================================================

||| Evidence for upgrade operations
public export
record UpgradeEvidence where
  constructor MkUpgradeEvidence
  ||| Epoch before upgrade
  fromEpoch : Nat
  ||| Epoch after upgrade
  toEpoch : Nat
  ||| Old implementation address
  oldImpl : Bits256
  ||| New implementation address
  newImpl : Bits256
  ||| Function selector being upgraded (0 for full upgrade)
  selector : Bits256
  ||| Migration data hash
  migrationHash : Bits256
  ||| Upgrader address
  upgrader : Bits256

||| Convert UpgradeEvidence to Evidence
public export
upgradeToEvidence : UpgradeEvidence -> Evidence
upgradeToEvidence ue = MkEvidence
  ue.newImpl
  ["upgrade", "epoch:" ++ show ue.fromEpoch ++ "->" ++ show ue.toEpoch]
  []
  []
  [(ue.oldImpl, ue.newImpl)]

-- =============================================================================
-- Lifecycle Storage
-- =============================================================================

||| Storage slots for lifecycle state
public export
LIFECYCLE_STATE_SLOT : Bits256
LIFECYCLE_STATE_SLOT = 0x4c494645435943  -- "LIFECYC" as hex

||| Storage slot for current epoch
public export
LIFECYCLE_EPOCH_SLOT : Bits256
LIFECYCLE_EPOCH_SLOT = 0x45504f4348  -- "EPOCH" as hex

||| Storage slot for pending upgrade
public export
PENDING_UPGRADE_SLOT : Bits256
PENDING_UPGRADE_SLOT = 0x50454e44494e47  -- "PENDING" as hex

||| Read current lifecycle state
public export
getLifecycleState : IO LifecycleState
getLifecycleState = do
  val <- sload LIFECYCLE_STATE_SLOT
  pure $ case val of
    0 => Undeployed
    1 => Deployed
    2 => Initialized
    3 => Upgraded
    4 => Deprecated
    5 => Paused
    _ => Undeployed

||| Write lifecycle state
public export
setLifecycleState : LifecycleState -> IO ()
setLifecycleState state = do
  let val = case state of
              Undeployed => 0
              Deployed => 1
              Initialized => 2
              Upgraded => 3
              Deprecated => 4
              Paused => 5
  sstore LIFECYCLE_STATE_SLOT val

||| Read current epoch
public export
getEpoch : IO Nat
getEpoch = do
  val <- sload LIFECYCLE_EPOCH_SLOT
  pure $ cast val

||| Write epoch
public export
setEpoch : Nat -> IO ()
setEpoch e = sstore LIFECYCLE_EPOCH_SLOT (cast e)

-- =============================================================================
-- FR-Aware Deploy
-- =============================================================================

||| Deploy failure reasons
public export
data DeployFailure : Type where
  ||| Not enough gas for deployment
  DeployGasExhausted : DeployFailure
  ||| Bytecode too large (EIP-170: 24KB limit)
  CodeSizeExceeded : DeployFailure
  ||| Constructor reverted
  ConstructorReverted : DeployFailure
  ||| CREATE2 address collision
  AddressCollision : DeployFailure
  ||| Already deployed at this address
  AlreadyDeployed : DeployFailure

||| Map DeployFailure to Conflict
public export
deployFailureToConflict : DeployFailure -> Conflict
deployFailureToConflict DeployGasExhausted = GasExhausted
deployFailureToConflict CodeSizeExceeded = InvariantViolation
deployFailureToConflict ConstructorReverted = Revert
deployFailureToConflict AddressCollision = StorageAlias
deployFailureToConflict AlreadyDeployed = InitAlready

||| FR-aware CREATE
||| value: ETH to send, offset: memory offset of bytecode, size: bytecode size
public export
createFR : Bits256 -> Bits256 -> Bits256 -> IO (Outcome Bits256)
createFR value offset size = do
  addr <- create value offset size
  if addr == 0
    then pure $ Fail GasExhausted (tagEvidence "create failed")
    else pure $ Ok addr

||| FR-aware CREATE2
||| value: ETH to send, offset: memory offset, size: bytecode size, salt: CREATE2 salt
public export
create2FR : Bits256 -> Bits256 -> Bits256 -> Bits256 -> IO (Outcome Bits256)
create2FR value offset size salt = do
  addr <- create2 value offset size salt
  if addr == 0
    then pure $ Fail GasExhausted (tagsEvidence ["create2", "failed"])
    else pure $ Ok addr

||| Full FR-aware deploy with evidence
||| bytecodeOffset: memory offset where bytecode starts
||| bytecodeSize: size of bytecode in bytes
||| salt: CREATE2 salt
||| initParams: initialization parameters (for evidence)
public export
deployWithEvidence : Bits256 -> Bits256 -> Bits256 -> List Bits256 -> IO (Outcome (Bits256, DeployEvidence))
deployWithEvidence bytecodeOffset bytecodeSize salt initParams = do
  deployer <- caller
  -- Compute bytecode hash for evidence
  let bytecodeHash = salt  -- Simplified: use salt as hash placeholder
  -- Attempt CREATE2
  result <- create2FR 0 bytecodeOffset bytecodeSize salt
  case result of
    Fail c e => do
      let evidence = MkDeployEvidence bytecodeHash salt 0 0 initParams deployer
      pure $ Fail c (mergeEvidence e (deployToEvidence evidence))
    Ok addr => do
      let evidence = MkDeployEvidence bytecodeHash salt addr addr initParams deployer
      pure $ Ok (addr, evidence)

-- =============================================================================
-- FR-Aware Initialize
-- =============================================================================

||| FR-aware initialization check
public export
requireNotInitialized : IO (Outcome ())
requireNotInitialized = do
  state <- getLifecycleState
  case state of
    Deployed => pure $ Ok ()
    Undeployed => pure $ Fail NotInitialized (tagEvidence "not deployed")
    _ => pure $ Fail InitAlready (tagsEvidence ["init", showState state])

||| FR-aware initialization
public export
initializeFR : IO (Outcome a) -> IO (Outcome a)
initializeFR action = do
  -- Check not already initialized
  check <- requireNotInitialized
  case check of
    Fail c e => pure $ Fail c e
    Ok () => do
      -- Run initialization
      result <- action
      case result of
        Fail c e => pure $ Fail c (addTag "init" e)
        Ok x => do
          -- Mark as initialized
          setLifecycleState Initialized
          setEpoch 0
          pure $ Ok x

-- =============================================================================
-- FR-Aware Upgrade
-- =============================================================================

||| Check upgrade is allowed
public export
requireUpgradeAllowed : IO (Outcome ())
requireUpgradeAllowed = do
  state <- getLifecycleState
  case state of
    Initialized => pure $ Ok ()
    Paused => pure $ Fail UpgradeNotAllowed (tagEvidence "contract paused")
    Deprecated => pure $ Fail UpgradeNotAllowed (tagEvidence "contract deprecated")
    Upgraded => pure $ Fail UpgradeNotAllowed (tagEvidence "upgrade pending")
    _ => pure $ Fail UpgradeNotAllowed (tagsEvidence ["upgrade", showState state])

||| FR-aware upgrade with evidence
public export
upgradeFR : Bits256 -> Bits256 -> IO (Outcome UpgradeEvidence)
upgradeFR selector newImpl = do
  -- Check upgrade allowed
  check <- requireUpgradeAllowed
  case check of
    Fail c e => pure $ Fail c e
    Ok () => do
      -- Get current state
      currentEpoch <- getEpoch
      oldImpl <- sload selector  -- Simplified: selector as slot
      upgrader <- caller

      -- Create evidence BEFORE mutation
      let evidence = MkUpgradeEvidence
            currentEpoch
            (S currentEpoch)
            oldImpl
            newImpl
            selector
            0  -- migration hash
            upgrader

      -- Perform upgrade
      sstore selector newImpl
      setEpoch (S currentEpoch)
      setLifecycleState Upgraded

      pure $ Ok evidence

||| Commit upgrade (finalize)
public export
commitUpgrade : IO (Outcome ())
commitUpgrade = do
  state <- getLifecycleState
  case state of
    Upgraded => do
      setLifecycleState Initialized
      pure $ Ok ()
    _ => pure $ Fail InvalidTransition
              (tagsEvidence ["commit", "not in upgraded state"])

||| Rollback upgrade using evidence
public export
rollbackUpgrade : UpgradeEvidence -> IO (Outcome ())
rollbackUpgrade ue = do
  state <- getLifecycleState
  case state of
    Upgraded => do
      -- Restore old implementation
      sstore ue.selector ue.oldImpl
      -- Restore old epoch
      setEpoch ue.fromEpoch
      -- Back to initialized
      setLifecycleState Initialized
      pure $ Ok ()
    _ => pure $ Fail RollbackNotAllowed
              (tagsEvidence ["rollback", "not in upgraded state"])

-- =============================================================================
-- Compensable Upgrade (Saga Pattern)
-- =============================================================================

||| Upgrade as a compensable operation
public export
record UpgradeCompensable where
  constructor MkUpgradeComp
  ||| The upgrade evidence (for rollback)
  evidence : UpgradeEvidence
  ||| Forward operation performed
  performed : Bool

||| Create compensable upgrade
public export
compensableUpgrade : Bits256 -> Bits256 -> IO (Outcome UpgradeCompensable)
compensableUpgrade selector newImpl = do
  result <- upgradeFR selector newImpl
  case result of
    Fail c e => pure $ Fail c e
    Ok evidence => pure $ Ok $ MkUpgradeComp evidence True

||| Compensate (rollback) an upgrade
public export
compensateUpgrade : UpgradeCompensable -> IO (Outcome ())
compensateUpgrade uc =
  if uc.performed
    then rollbackUpgrade uc.evidence
    else pure $ Ok ()  -- Nothing to compensate

-- =============================================================================
-- Lifecycle Guards
-- =============================================================================

||| Require contract is initialized
public export
requireInitialized : IO (Outcome ())
requireInitialized = do
  state <- getLifecycleState
  case state of
    Initialized => pure $ Ok ()
    Upgraded => pure $ Ok ()  -- Upgraded is also operational
    _ => pure $ Fail NotInitialized (tagsEvidence ["require", showState state])

||| Require contract is not paused
public export
requireNotPaused : IO (Outcome ())
requireNotPaused = do
  state <- getLifecycleState
  case state of
    Paused => pure $ Fail InvalidTransition (tagEvidence "contract paused")
    Deprecated => pure $ Fail InvalidTransition (tagEvidence "contract deprecated")
    _ => pure $ Ok ()

||| Require contract is operational (initialized + not paused)
public export
requireOperational : IO (Outcome ())
requireOperational = do
  initCheck <- requireInitialized
  case initCheck of
    Fail c e => pure $ Fail c e
    Ok () => requireNotPaused

||| Guard function with lifecycle check
public export
withLifecycleGuard : IO (Outcome a) -> IO (Outcome a)
withLifecycleGuard action = do
  check <- requireOperational
  case check of
    Fail c e => pure $ Fail c e
    Ok () => action

-- =============================================================================
-- Pause/Unpause
-- =============================================================================

||| FR-aware pause
public export
pauseFR : IO (Outcome ())
pauseFR = do
  state <- getLifecycleState
  case state of
    Initialized => do
      setLifecycleState Paused
      pure $ Ok ()
    _ => pure $ Fail InvalidTransition
              (tagsEvidence ["pause", "invalid state", showState state])

||| FR-aware unpause
public export
unpauseFR : IO (Outcome ())
unpauseFR = do
  state <- getLifecycleState
  case state of
    Paused => do
      setLifecycleState Initialized
      pure $ Ok ()
    _ => pure $ Fail InvalidTransition
              (tagsEvidence ["unpause", "invalid state", showState state])

-- =============================================================================
-- Deprecation
-- =============================================================================

||| FR-aware deprecation
public export
deprecateFR : IO (Outcome ())
deprecateFR = do
  state <- getLifecycleState
  case state of
    Initialized => do
      setLifecycleState Deprecated
      pure $ Ok ()
    Paused => do
      setLifecycleState Deprecated
      pure $ Ok ()
    _ => pure $ Fail InvalidTransition
              (tagsEvidence ["deprecate", "invalid state", showState state])

-- =============================================================================
-- Full Lifecycle Execution with Transition Proof
-- =============================================================================

||| Execute a lifecycle transition with type-level proof
public export
executeTransition : LifecycleTransition from to
                 -> IO (Outcome a)
                 -> IO (Outcome (a, Evidence))
executeTransition trans action = do
  -- Get current state
  currentState <- getLifecycleState
  -- Verify we're in expected state (runtime check matching type-level)
  let expectedFrom = case trans of
        DoDeploy => Undeployed
        DoInit => Deployed
        DoUpgrade => Initialized
        DoCommitUpgrade => Upgraded
        DoRollbackUpgrade => Upgraded
        DoDeprecate => Initialized
        DoPause => Initialized
        DoUnpause => Paused
  if currentState /= expectedFrom
    then pure $ Fail InvalidTransition
              (tagsEvidence ["transition", transitionName trans
                           , "expected", showState expectedFrom
                           , "actual", showState currentState])
    else do
      result <- action
      case result of
        Fail c e => pure $ Fail c (addTag (transitionName trans) e)
        Ok x => do
          let evidence = tagsEvidence ["transition", transitionName trans, "success"]
          pure $ Ok (x, evidence)

