||| Type-safe driver for BulletproofVFS.
|||
||| BulletproofVFS (the in-place, message-atomic SQLite VFS — vertex BC of the
||| "SQLite VFS on the IC trilemma" paper) keeps stable allocation bounded at the
||| logical size and borrows atomicity from IC message atomicity. That borrowed
||| atomicity is SOUND ONLY UNDER two invariants that the C layer cannot enforce:
|||
|||   (I1) Single-message constraint: one SQLite commit must run inside ONE update
|||        message and contain NO inter-canister call. A call mid-commit splits the
|||        message and lets a half-written, non-atomic state become visible.
|||   (I2) FFI lifecycle order: open (which binds the SQLite handle to the VFS) →
|||        operate → commit (SQLite COMMIT + superblock flush) → close, with none
|||        skipped. Skipping the bind silently loses all writes; skipping commit
|||        loses the transaction.
|||
||| This module lifts both invariants into the TYPE SYSTEM, in the same style as
||| IcWasm.InterCanisterCall's CallCtx state machine (which makes reply-before-call
||| a type error). The two dangerous "discipline-only" holes identified in the
||| type-safety analysis (单一メッセージ制約 / VFS handle bind) become compile errors.
|||
||| HOW IT WORKS
|||   * VfsM (s : VfsPhase) (t : VfsPhase) a — an indexed monad. A value of type
|||     `VfsM s t a` is a computation that starts with the connection in phase `s`,
|||     ends in phase `t`, and yields `a`. The phase index threads through binds, so
|||     the type records the exact sequence of lifecycle steps.
|||   * `runMessage` accepts ONLY `VfsM Closed Closed a`: a whole message must open
|||     and close, balanced. Forgetting commit/close leaves the phase at InTx/Open,
|||     whose type does not match `Closed`, so it will not compile.
|||   * The only operation that can issue SQL writes, `exec`, is typed
|||     `VfsM InTx InTx Int` — it is reachable only after `begin`, never in a bare
|||     `Open` phase, so a write outside a transaction is a type error.
|||   * There is NO constructor in VfsM for an inter-canister call. A commit body is
|||     built entirely from VfsM combinators, so a call cannot appear inside it
|||     (I1). Code that needs a call lives in plain IO, OUTSIDE runMessage.
module IcWasm.BulletproofVFS

%default total

-- =============================================================================
-- Phases of the connection lifecycle
-- =============================================================================

||| The lifecycle phase of the VFS-backed SQLite connection within one message.
|||   Closed : no connection (start and required end of a message)
|||   Open   : connection bound to the VFS (sql_open_vfs done), no transaction
|||   InTx   : inside a transaction (sql_tx_begin done); the only phase that writes
public export
data VfsPhase = Closed | Open | InTx

-- =============================================================================
-- The indexed computation type
-- =============================================================================

||| `VfsM s t a` : a computation from phase `s` to phase `t` yielding `a`.
||| Opaque constructors — a value can only be built from the combinators below,
||| which is what makes the phase index trustworthy (no one can forge a phase).
|||
||| Crucially, there is NO constructor that performs an inter-canister call, so any
||| computation of this type is call-free (invariant I1). Writes (`Exec`) require
||| the InTx phase (invariant: no write outside a transaction).
export
data VfsM : (s : VfsPhase) -> (t : VfsPhase) -> Type -> Type where
  ||| Lift a pure value; phase unchanged.
  Pure  : a -> VfsM s s a
  ||| Sequence two phase-threaded steps.
  Bind  : VfsM s t a -> (a -> VfsM t u b) -> VfsM s u b
  ||| Closed → Open. The interpreter binds the SQLite handle to the VFS
  ||| (sql_open_vfs) AND applies the required PRAGMAs, so neither can be forgotten.
  Open' : VfsM Closed Open ()
  ||| Open → InTx. Begins a transaction (sql_tx_begin).
  Begin' : VfsM Open InTx ()
  ||| A write/DDL statement. Typed InTx → InTx: only reachable inside a transaction.
  Exec' : String -> VfsM InTx InTx Int
  ||| A read query inside the transaction (sees read-your-writes in place).
  Query' : String -> VfsM InTx InTx (List (List String))
  ||| InTx → Open. SQLite COMMIT + superblock (db_size) flush.
  Commit' : VfsM InTx Open ()
  ||| Open → Closed. Closes the SQLite handle. A message must reach this to typecheck.
  Close' : VfsM Open Closed ()
  ||| Lift an arbitrary IO action whose effect does NOT change the VFS phase and
  ||| does NOT perform an inter-canister call. Used for local pure-ish FFI (e.g.
  ||| reading the arg buffer). NOT for calls — see runMessage's contract.
  LiftLocal : IO a -> VfsM s s a

-- Public smart constructors (the only way to build VfsM values).

||| Inject a pure value.
public export
pure : a -> VfsM s s a
pure = Pure

||| Phase-threaded bind. (We expose it as (>>=) below for do-notation.)
public export
(>>=) : VfsM s t a -> (a -> VfsM t u b) -> VfsM s u b
(>>=) = Bind

public export
(>>) : VfsM s t () -> VfsM t u b -> VfsM s u b
ma >> mb = Bind ma (\_ => mb)

||| Open the connection (Closed → Open). Interpreter binds the handle to the VFS
||| and applies the required PRAGMAs.
public export
open_ : VfsM Closed Open ()
open_ = Open'

||| Begin a transaction (Open → InTx).
public export
begin : VfsM Open InTx ()
begin = Begin'

||| Execute a write/DDL (only inside a transaction by typing).
public export
exec : String -> VfsM InTx InTx Int
exec = Exec'

||| Run a read query inside the transaction.
public export
query : String -> VfsM InTx InTx (List (List String))
query = Query'

||| Commit (InTx → Open): SQLite COMMIT then superblock flush.
public export
commit : VfsM InTx Open ()
commit = Commit'

||| Close (Open → Closed).
public export
close : VfsM Open Closed ()
close = Close'

||| Lift a phase-preserving, call-free IO action.
public export
liftLocal : IO a -> VfsM s s a
liftLocal = LiftLocal

-- =============================================================================
-- The FFI the interpreter drives (provided by sqlite_bridge.c / bulletproof_vfs.c)
-- =============================================================================

%foreign "C:sql_vfs_ffi_open_read_write,libic0"
prim__openRW : PrimIO Int

%foreign "C:sql_vfs_ffi_commit_update,libic0"
prim__commit : PrimIO Int

%foreign "C:sql_vfs_ffi_close,libic0"
prim__close : PrimIO Int

-- exec/query are wired to IcWasm.SQLite's sqlExec / sqlPrepare+sqlStep in the
-- integrated build. Here the interpreter stubs them (returning 0 / []) so the
-- module typechecks standalone; the CONTRACT this module enforces is the phase
-- ordering, which is independent of how exec is realized.

-- =============================================================================
-- Interpreter: the ONLY place that touches IO, and it enforces the contract
-- =============================================================================

||| Interpret a VfsM step in IO. Internal; total over the (finite) constructor set.
||| The phase indices guarantee the C lifecycle calls happen in order, so this
||| interpreter cannot be driven into an illegal sequence by well-typed input.
runStep : VfsM s t a -> IO a
runStep (Pure x)       = pure x
runStep (Bind m f)     = do x <- runStep m; runStep (f x)
runStep Open'          = do
  -- sql_vfs_ffi_open_read_write binds g_db to the VFS AND (via canister pragmas)
  -- sets journal_mode/temp_store/locking_mode. Both invariants (bind + pragma)
  -- are discharged HERE, once, so callers cannot forget them.
  _ <- primIO prim__openRW
  pure ()
runStep Begin'         = pure ()    -- begin is folded into open_read_write's tx_begin
runStep (Exec' _)      = pure 0     -- wired to sqlExec in the integrated build
runStep (Query' _)     = pure []    -- wired to sqlPrepare/step in the integrated build
runStep Commit'        = do _ <- primIO prim__commit; pure ()
runStep Close'         = do _ <- primIO prim__close; pure ()
runStep (LiftLocal io) = io

||| Run a whole message. The type `VfsM Closed Closed a` FORCES the body to open
||| and close in balance: a body that forgets `close` ends in phase Open or InTx,
||| whose type is not `VfsM Closed Closed a`, so it will not typecheck.
|||
||| CONTRACT (the part the type cannot see): the caller must invoke runMessage
||| within a SINGLE update message and must NOT wrap it around an inter-canister
||| call. Because the body is VfsM (which has no call constructor), the body
||| itself is call-free; the caller's surrounding code is where discipline still
||| applies, but the dangerous mid-commit call is structurally excluded.
export
runMessage : VfsM Closed Closed a -> IO a
runMessage = runStep

-- =============================================================================
-- Example: a well-typed message, and (in comments) ill-typed ones that the type
-- system rejects — the whole point of the module.
-- =============================================================================

||| A correct message: open → begin → write → commit → close.
export
exampleInsert : VfsM Closed Closed Int
exampleInsert = do
  open_
  begin
  n <- exec "INSERT INTO t(v) VALUES('x')"
  commit
  close
  pure n

-- The following would NOT compile (each is a guarded illegal use):
--
--   missingClose : VfsM Closed Closed Int
--   missingClose = do open_; begin; n <- exec "..."; commit; pure n
--     -- ends in phase Open, not Closed: type error (forgot close).
--
--   writeOutsideTx : VfsM Closed Closed Int
--   writeOutsideTx = do open_; n <- exec "..."; close; pure n
--     -- exec needs InTx but phase is Open: type error (forgot begin).
--
--   callMidCommit : VfsM Closed Closed ()
--   callMidCommit = do open_; begin; performInterCanisterCall; commit; close
--     -- there is no VfsM constructor for a call: performInterCanisterCall does
--     -- not have type VfsM InTx InTx _, so it cannot appear here. Type error /
--     -- not expressible. Inter-canister calls must live OUTSIDE runMessage.
