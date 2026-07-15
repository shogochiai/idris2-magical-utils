||| The canonical test-suite type, shared by EVERY Idris2 test module across the
||| EtherClaw repos (the app, the canister, lazy, the coverage tools).
|||
||| WHY a single type: the coverage tool measures Step 4 by building a test exe and
||| running it. A large suite (EtherClaw: ~1100 tests in ONE module) OOMs an 8GB
||| host if run in one process. The fix is INTRA-MODULE chunking — run the suite in
||| offset/limit slices, each a fresh process. That is only possible GENERICALLY if
||| every module exposes its tests as DATA of one common type the runner can slice.
||| Six ad-hoc shapes ((String, IO Bool), TestDef, (String, Bool), …) made a generic
||| slicer impossible; this type is the single shape they all migrate to.
|||
||| EFFECTFUL by design: a TestCase's body is `IO Bool`, the superset — a pure
||| `Bool` test wraps as `pure b` (see `pureCase`). So no test semantics are lost.
|||
||| DEPENDENCY DISCIPLINE: `base` ONLY, so the canister, the app, lazy, and the
||| coverage tools can all depend on it without pulling in anything heavier.
module Idris2.TestSuite

import Data.List
import System.Coverage

%default total

||| One test: a human-readable name and an effectful pass/fail body.
public export
TestCase : Type
TestCase = (String, IO Bool)

||| A test suite is just the list of its cases — sliceable by the coverage runner.
public export
TestSuite : Type
TestSuite = List TestCase

||| Wrap a pure (non-effectful) test into a TestCase.
public export
pureCase : String -> Bool -> TestCase
pureCase name result = (name, pure result)

||| Wrap a pure thunked test (`() -> Bool`, the old TestDef.run shape).
public export
thunkCase : String -> (() -> Bool) -> TestCase
thunkCase name run = (name, pure (run ()))

||| The slice of a suite the runner should execute this process: drop `offset`,
||| then take `limit` (Nothing = take the rest). This is THE intra-module chunk
||| primitive — the coverage tool runs the same exe with successive offsets so each
||| chunk is a separate, memory-bounded process.
public export
sliceSuite : (offset : Nat) -> (limit : Maybe Nat) -> TestSuite -> TestSuite
sliceSuite offset limit suite =
  let dropped = drop offset suite
  in case limit of
       Nothing => dropped
       Just n  => take n dropped

||| Run a suite, returning (passed, failed). Tail-recursive accumulation so a
||| large suite does not retain every result.
public export
runSuite : TestSuite -> IO (Nat, Nat)
runSuite = go 0 0
  where
    go : Nat -> Nat -> TestSuite -> IO (Nat, Nat)
    go p f [] = pure (p, f)
    go p f ((name, body) :: rest) = do
      -- Attribute every path hit during this test to its name (path-coverage
      -- Step 4): under --dumppaths-hits the compiler records `<name>\t<path-id>`,
      -- so the numerator can be grouped per test (hence per SpecId, since test
      -- names are `test_<REQ_ID>_...`). A no-op in a normal (non-instrumented)
      -- build, so this costs nothing off the coverage path.
      enterTest name
      ok <- body
      if ok
        then do putStrLn ("[PASS] " ++ name); go (S p) f rest
        else do putStrLn ("[FAIL] " ++ name); go p (S f) rest
