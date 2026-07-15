||| PoC — linearity makes "this path consumes a process-spawn output" a TYPE FACT
||| the compiler enforces, not a human declaration. This is the trick-proof basis
||| for EffectBoundary (Coverage.Standardization.Types): a path that produces or
||| consumes `Spawned` IS a ProcessSpawn-boundary path BY TYPE — no call-graph
||| approximation, no observer judgment, no value weight.
|||
||| Verified (idris2 --check):
|||   * the single legal consume typechecks;
|||   * using the linear output TWICE  → "Trying to use linear name in non-linear context";
|||   * using it ZERO times (dropping)  → "There are 0 uses of linear name ... must be used exactly once".
||| So "exactly-once consumption of the external-world resource" is compiler-enforced.
module LinearBoundaryPoC

||| The output captured from a real process spawn (popen2/runProc).
public export
record ProcResult where
  constructor MkProcResult
  exitCode : Int
  stdout   : String

||| A spawn witness: the external output MUST be consumed exactly once. By giving
||| the boundary resource linear multiplicity, "this code path touched the
||| process-spawn hole" becomes a property the TYPE CHECKER verifies — the
||| denominator-exclusion basis the observer cannot move.
public export
data Spawned : Type where
  MkSpawned : (1 _ : ProcResult) -> Spawned

||| The one legal use: extract the result, discharging linearity exactly once.
public export
consume : (1 _ : Spawned) -> ProcResult
consume (MkSpawned r) = r

||| A path that never touches Spawned is PureComputation BY TYPE — it stays in
||| the test-coverage denominator and must be tested.
public export
pureLen : String -> Nat
pureLen = length
