||| Evidence Tracking for FR Monad
|||
||| Based on the FR Monad paper: "Recovery-Preserving Kleisli Semantics for World-Computer Virtual Machines"
|||
||| E is the set of evidence, forming a monoid (E, ⊕, e₀).
||| Key principle: force observability - every FR result must carry evidence.
|||
||| Section 4: Evidence accumulates via ⊕ under Kleisli composition.
||| This enables:
||| - Post-mortem debugging
||| - Replay/audit trails
||| - Performance analysis
||| - Cycles cost attribution
module FRMonad.Evidence

import Data.List

%default total

-- Local unlines implementation
unlines : List String -> String
unlines []        = ""
unlines [x]       = x
unlines (x :: xs) = x ++ "\n" ++ unlines xs

-- =============================================================================
-- Phase: Execution context boundaries (Section 3.1: Boundaries)
-- =============================================================================

||| ICP canister lifecycle phases
||| Each phase represents a distinct execution boundary with different capabilities
public export
data Phase
  = Init           -- Canister initialization
  | PreUpgrade     -- Before upgrade (save state)
  | PostUpgrade    -- After upgrade (restore state)
  | Update         -- State-modifying call
  | Query          -- Read-only call
  | Heartbeat      -- Periodic execution
  | Inspect        -- Message inspection
  | Timer          -- Timer callback
  | HttpRequest    -- HTTP outcall context
  | Composite      -- Composite query

public export
Show Phase where
  show Init        = "Init"
  show PreUpgrade  = "PreUpgrade"
  show PostUpgrade = "PostUpgrade"
  show Update      = "Update"
  show Query       = "Query"
  show Heartbeat   = "Heartbeat"
  show Inspect     = "Inspect"
  show Timer       = "Timer"
  show HttpRequest = "HttpRequest"
  show Composite   = "Composite"

public export
Eq Phase where
  Init        == Init        = True
  PreUpgrade  == PreUpgrade  = True
  PostUpgrade == PostUpgrade = True
  Update      == Update      = True
  Query       == Query       = True
  Heartbeat   == Heartbeat   = True
  Inspect     == Inspect     = True
  Timer       == Timer       = True
  HttpRequest == HttpRequest = True
  Composite   == Composite   = True
  _ == _ = False

-- =============================================================================
-- Call Record: Inter-canister call tracking
-- =============================================================================

||| Record of an inter-canister call
public export
record CallRecord where
  constructor MkCallRecord
  targetCanister : String       -- Target canister principal
  methodName     : String       -- Method called
  cyclesSent     : Nat          -- Cycles attached
  success        : Bool         -- Did call succeed?
  responseSize   : Nat          -- Response payload size

public export
Show CallRecord where
  show c = c.targetCanister ++ "." ++ c.methodName ++
           " (" ++ show c.cyclesSent ++ " cycles)" ++
           if c.success then " OK" else " FAILED"

||| Empty call record
public export
emptyCall : CallRecord
emptyCall = MkCallRecord "" "" 0 True 0

-- =============================================================================
-- Storage Operation: Stable memory tracking
-- =============================================================================

||| Record of stable memory operations
public export
record StorageOp where
  constructor MkStorageOp
  operation : String    -- "read" | "write" | "grow"
  offset    : Nat       -- Memory offset
  size      : Nat       -- Bytes read/written

public export
Show StorageOp where
  show s = s.operation ++ " @" ++ show s.offset ++ " (" ++ show s.size ++ " bytes)"

-- =============================================================================
-- HTTP Outcall Record: HTTP call tracking
-- =============================================================================

||| Record of an HTTP outcall
public export
record HttpRecord where
  constructor MkHttpRecord
  url         : String    -- Target URL
  method      : String    -- HTTP method
  statusCode  : Int       -- Response status code
  cyclesCost  : Nat       -- Cycles consumed
  latencyMs   : Nat       -- Latency in milliseconds

public export
Show HttpRecord where
  show h = h.method ++ " " ++ h.url ++ " -> " ++ show h.statusCode ++
           " (" ++ show h.cyclesCost ++ " cycles, " ++ show h.latencyMs ++ "ms)"

-- =============================================================================
-- Evidence: The set E with monoid structure (E, ⊕, e₀)
-- =============================================================================

||| Evidence record - captures comprehensive context for diagnosis
||| Every FR result must carry evidence regardless of success/failure
public export
record Evidence where
  constructor MkEvidence
  -- Execution context
  phase         : Phase           -- Execution phase
  label         : String          -- Operation identifier
  detail        : String          -- Additional context
  timestamp     : Nat             -- IC time (nanoseconds)
  caller        : String          -- Caller principal
  -- Resource tracking
  cyclesStart   : Nat             -- Cycles at operation start
  cyclesEnd     : Nat             -- Cycles at operation end
  instructionsUsed : Nat          -- Instructions consumed
  -- Operation tracking
  tags          : List String     -- Diagnostic tags
  calls         : List CallRecord -- Inter-canister calls made
  storageOps    : List StorageOp  -- Stable memory operations
  httpCalls     : List HttpRecord -- HTTP outcalls made
  -- Chain
  parentId      : String          -- Parent operation ID (for tracing)

public export
Show Evidence where
  show e = "[" ++ show e.phase ++ "] " ++ e.label ++ ": " ++ e.detail ++
           " (cycles: " ++ show (minus e.cyclesStart e.cyclesEnd) ++ ")"

-- =============================================================================
-- Monoid identity: e₀ (neutral element)
-- =============================================================================

||| Empty evidence - the monoid identity e₀
||| pure computations introduce no recovery obligations (Unit law)
public export
emptyEvidence : Evidence
emptyEvidence = MkEvidence
  Query "" "" 0 ""
  0 0 0
  [] [] [] []
  ""

-- =============================================================================
-- Evidence constructors
-- =============================================================================

||| Create basic evidence
public export
mkEvidence : Phase -> String -> String -> Evidence
mkEvidence phase label detail = MkEvidence
  phase label detail 0 ""
  0 0 0
  [] [] [] []
  ""

||| Create evidence with caller
public export
mkEvidenceWithCaller : Phase -> String -> String -> String -> Evidence
mkEvidenceWithCaller phase label detail caller = MkEvidence
  phase label detail 0 caller
  0 0 0
  [] [] [] []
  ""

||| Create evidence with timestamp
public export
mkEvidenceWithTime : Phase -> String -> String -> Nat -> Evidence
mkEvidenceWithTime phase label detail timestamp = MkEvidence
  phase label detail timestamp ""
  0 0 0
  [] [] [] []
  ""

-- =============================================================================
-- Evidence modification
-- =============================================================================

||| Add a tag to evidence
public export
addTag : String -> Evidence -> Evidence
addTag tag e = { tags := tag :: e.tags } e

||| Add multiple tags
public export
addTags : List String -> Evidence -> Evidence
addTags ts e = { tags := ts ++ e.tags } e

||| Record a call in evidence
public export
recordCall : CallRecord -> Evidence -> Evidence
recordCall c e = { calls := c :: e.calls } e

||| Record a storage operation
public export
recordStorage : StorageOp -> Evidence -> Evidence
recordStorage s e = { storageOps := s :: e.storageOps } e

||| Record an HTTP outcall
public export
recordHttp : HttpRecord -> Evidence -> Evidence
recordHttp h e = { httpCalls := h :: e.httpCalls } e

||| Set cycles tracking
public export
withCycles : Nat -> Nat -> Evidence -> Evidence
withCycles start end e = { cyclesStart := start, cyclesEnd := end } e

||| Set instructions used
public export
withInstructions : Nat -> Evidence -> Evidence
withInstructions n e = { instructionsUsed := n } e

||| Set parent ID for tracing
public export
withParent : String -> Evidence -> Evidence
withParent pid e = { parentId := pid } e

-- =============================================================================
-- Monoid operation: ⊕ (Evidence combination)
-- Section 4: Evidence accumulates via ⊕ under Kleisli composition
-- =============================================================================

||| Combine evidence (the monoid operation ⊕)
||| Merges tracking data, keeps latest context
||| This is the key operation for recovery-aware Kleisli composition
public export
combineEvidence : Evidence -> Evidence -> Evidence
combineEvidence e1 e2 = MkEvidence
  e2.phase
  (if e1.label == "" then e2.label else e1.label ++ " -> " ++ e2.label)
  (if e1.detail == "" then e2.detail else e1.detail ++ "; " ++ e2.detail)
  e2.timestamp
  (if e2.caller == "" then e1.caller else e2.caller)
  e1.cyclesStart
  e2.cyclesEnd
  (e1.instructionsUsed + e2.instructionsUsed)
  (e1.tags ++ e2.tags)
  (e1.calls ++ e2.calls)
  (e1.storageOps ++ e2.storageOps)
  (e1.httpCalls ++ e2.httpCalls)
  (if e2.parentId == "" then e1.parentId else e2.parentId)

||| Semigroup instance: (<+>) = ⊕
public export
Semigroup Evidence where
  (<+>) = combineEvidence

||| Monoid instance: neutral = e₀
public export
Monoid Evidence where
  neutral = emptyEvidence

-- =============================================================================
-- Evidence queries
-- =============================================================================

||| Total cycles consumed
public export
cyclesConsumed : Evidence -> Nat
cyclesConsumed e = minus e.cyclesStart e.cyclesEnd

||| Total cycles sent in calls
public export
cyclesSentInCalls : Evidence -> Nat
cyclesSentInCalls e = foldl (\acc, c => acc + c.cyclesSent) 0 e.calls

||| Total HTTP cycles cost
public export
httpCyclesCost : Evidence -> Nat
httpCyclesCost e = foldl (\acc, h => acc + h.cyclesCost) 0 e.httpCalls

||| Number of inter-canister calls
public export
callCount : Evidence -> Nat
callCount e = length e.calls

||| Number of failed calls
public export
failedCallCount : Evidence -> Nat
failedCallCount e = length (filter (not . success) e.calls)

||| Number of storage operations
public export
storageOpCount : Evidence -> Nat
storageOpCount e = length e.storageOps

||| Number of HTTP outcalls
public export
httpCallCount : Evidence -> Nat
httpCallCount e = length e.httpCalls

||| Check if any call failed
public export
hasFailedCalls : Evidence -> Bool
hasFailedCalls e = failedCallCount e > 0

||| Check if evidence has specific tag
public export
hasTag : String -> Evidence -> Bool
hasTag tag e = elem tag e.tags

-- =============================================================================
-- Evidence serialization helpers
-- =============================================================================

||| Render evidence as diagnostic string
public export
renderEvidence : Evidence -> String
renderEvidence e = unlines
  [ "=== Evidence ==="
  , "Phase: " ++ show e.phase
  , "Label: " ++ e.label
  , "Detail: " ++ e.detail
  , "Caller: " ++ e.caller
  , "Cycles: " ++ show (cyclesConsumed e)
  , "Instructions: " ++ show e.instructionsUsed
  , "Tags: " ++ show e.tags
  , "Calls: " ++ show (length e.calls)
  , "Storage ops: " ++ show (length e.storageOps)
  , "HTTP calls: " ++ show (length e.httpCalls)
  ]
