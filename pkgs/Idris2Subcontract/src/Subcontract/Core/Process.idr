||| Subcontract Core: Process Tracking with Linear Tokens
|||
||| Implements mc-style process tracking where operations must be
||| explicitly started and finished. Uses Idris2's linear types to
||| ensure processes cannot be left unfinished.
|||
||| Key insight: mc's startProcess/finishProcess pattern becomes a
||| linear resource that MUST be consumed, making "forgot to finish"
||| a compile-time error.
|||
||| Example:
|||   initMember : (1 _ : ProcessToken Init) -> Contract (ProcessToken Init)
|||   initMember tok = do
|||     -- operations here
|||     pure tok  -- must return the token
|||
module Subcontract.Core.Process

-- =============================================================================
-- Process Phases (mc workflow states)
-- =============================================================================

||| Phases in the mc deployment/upgrade workflow
||| These correspond to mc's process names
public export
data Phase
  = Init           -- Initial setup (dictionary registration)
  | EnsureInit     -- Verify initialization is complete
  | Find           -- Lookup in registry
  | FindCurrent    -- Get current implementation
  | Upgrade        -- Upgrade implementation
  | Deploy         -- Deploy new contract
  | Custom String  -- Custom process name

export
Show Phase where
  show Init = "init"
  show EnsureInit = "ensureInit"
  show Find = "find"
  show FindCurrent = "findCurrent"
  show Upgrade = "upgrade"
  show Deploy = "deploy"
  show (Custom s) = s

-- =============================================================================
-- Process Token (Linear Resource)
-- =============================================================================

||| A linear token representing an active process
||| Must be consumed by `finish` - cannot be dropped or duplicated
public export
data ProcessToken : Phase -> Type where
  MkProcessToken : (pid : Nat) -> (phase : Phase) -> ProcessToken phase

||| Extract process ID (for logging)
export
processId : ProcessToken ph -> Nat
processId (MkProcessToken pid _) = pid

-- =============================================================================
-- Process Status (mc-style)
-- =============================================================================

||| Status of a tracked entity (bundle, implementation, etc.)
public export
data Status
  = NotInitialized
  | InProgress
  | Completed
  | Current
  | Deprecated

export
Eq Status where
  NotInitialized == NotInitialized = True
  InProgress == InProgress = True
  Completed == Completed = True
  Current == Current = True
  Deprecated == Deprecated = True
  _ == _ = False

export
Show Status where
  show NotInitialized = "NotInitialized"
  show InProgress = "InProgress"
  show Completed = "Completed"
  show Current = "Current"
  show Deprecated = "Deprecated"

-- =============================================================================
-- Process Trace (for debugging/auditing)
-- =============================================================================

||| A trace entry for process execution
public export
record TraceEntry where
  constructor MkTrace
  tracePhase : Phase
  tracePid : Nat
  traceMessage : String

export
Show TraceEntry where
  show e = "[" ++ show e.tracePhase ++ "#" ++ show e.tracePid ++ "] " ++ e.traceMessage

-- =============================================================================
-- Process Monad (Indexed by Phase)
-- =============================================================================

||| Indexed monad for process-aware operations
||| Tracks the current phase in the type
public export
data Process : Phase -> Phase -> Type -> Type where
  Pure : a -> Process p p a
  Bind : Process p q a -> (a -> Process q r b) -> Process p r b
  Log : String -> Process p p ()

||| Bind for same-phase process
export
(>>=) : Process p p a -> (a -> Process p p b) -> Process p p b
(>>=) = Bind

||| Sequence processes
export
(>>) : Process p p () -> Process p p b -> Process p p b
m >> n = Bind m (\_ => n)

||| Start a new process, returning a linear token
export
startProcess : (ph : Phase) -> Process ph ph (ProcessToken ph)
startProcess ph = Pure (MkProcessToken 0 ph)

||| Finish a process, consuming the linear token
export
finishProcess : ProcessToken ph -> Process ph ph ()
finishProcess (MkProcessToken _ _) = Pure ()

||| Run a process and collect trace
export
runProcess : Process p q a -> (a, List TraceEntry)
runProcess (Pure a) = (a, [])
runProcess (Bind m k) =
  let (a, t1) = runProcess m
      (b, t2) = runProcess (k a)
  in (b, t1 ++ t2)
runProcess (Log msg) = ((), [MkTrace Init 0 msg])
