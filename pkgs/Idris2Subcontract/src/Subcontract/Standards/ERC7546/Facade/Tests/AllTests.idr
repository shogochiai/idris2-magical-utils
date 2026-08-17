||| Facade Tests
|||
||| SPEC-Test Parity for Subcontract.Standards.ERC7546.Facade.
|||
||| Every [[spec]] entry in the co-located Facade/SPEC.toml (prefix FCD) is
||| declared here as a `TestDef` with a real `Runs` body: each test calls
||| `facadeSolidity` with concrete FacadeFn fixtures and asserts on the exact
||| rendered Solidity. The runner sets the coverage label to `test_<specId>`
||| immediately before each `Runs` body so path hits are attributed per SpecId.
module Subcontract.Standards.ERC7546.Facade.Tests.AllTests

import Subcontract.Standards.ERC7546.Facade
import Subcontract.Core.ABI.Sig

import Data.String
import System
import System.Coverage

%default covering

-- =============================================================================
-- Test Infrastructure
-- =============================================================================

||| Body of a test definition: executable assertions or a pending marker.
public export
data TestBody
  = Runs (IO Bool)
  | Pending String

||| A named test definition that either runs assertions or is pending.
||| The `specId` is the exact REQ id from SPEC.toml.
public export
record TestDef where
  constructor MkTest
  specId      : String
  description : String
  body        : TestBody

||| Construct a test definition with a body.
public export
test : String -> String -> TestBody -> TestDef
test sid desc body = MkTest sid desc body

||| Outcome of running a single test definition.
public export
data Verdict
  = VerdictPass
  | VerdictFail
  | VerdictPending

||| Run a test body, producing a verdict.
public export
verdictOf : TestBody -> IO Verdict
verdictOf (Pending _) = pure VerdictPending
verdictOf (Runs io) = do
  ok <- io
  pure (if ok then VerdictPass else VerdictFail)

||| Aggregate verdicts into (passed, failed, pending) counts.
export
tally : List Verdict -> (Nat, Nat, Nat)
tally = go 0 0 0
  where
    go : Nat -> Nat -> Nat -> List Verdict -> (Nat, Nat, Nat)
    go p f q [] = (p, f, q)
    go p f q (VerdictPass :: rest) = go (S p) f q rest
    go p f q (VerdictFail :: rest) = go p (S f) q rest
    go p f q (VerdictPending :: rest) = go p f (S q) rest

||| Short tag for a verdict, used in result lines.
export
showVerdict : Verdict -> String
showVerdict VerdictPass = "PASS"
showVerdict VerdictFail = "FAIL"
showVerdict VerdictPending = "PENDING"

||| Run one test definition, producing its verdict and a result line.
||| The coverage label is set to `test_<specId>` immediately before running
||| a `Runs` body so that path hits are attributed per SpecId.
runOne : TestDef -> IO (Verdict, String)
runOne (MkTest sid desc (Pending reason)) =
  pure (VerdictPending, "[PENDING] " ++ sid ++ " " ++ desc ++ " (" ++ reason ++ ")")
runOne (MkTest sid desc (Runs io)) = do
  enterTest ("test_" ++ sid)
  ok <- io
  let v = if ok then VerdictPass else VerdictFail
  pure (v, "[" ++ showVerdict v ++ "] " ++ sid ++ " " ++ desc)

-- =============================================================================
-- Fixtures
-- =============================================================================

||| A single-value view function with two args and one bool return.
transferView : FacadeFn
transferView = MkFacadeFn (MkSig "transfer" [TAddress, TUint256] [TBool]) View

||| The same transfer signature marked Nonpayable.
transferNonpayable : FacadeFn
transferNonpayable = MkFacadeFn (MkSig "transfer" [TAddress, TUint256] [TBool]) Nonpayable

||| A payable function with a single argument and no returns.
depositPayable : FacadeFn
depositPayable = MkFacadeFn (MkSig "deposit" [TUint256] []) Payable

||| A nonpayable function with no arguments and no returns.
noopNonpayable : FacadeFn
noopNonpayable = MkFacadeFn (MkSig "noop" [] []) Nonpayable

||| A view function with one argument and one uint256 return.
balanceView : FacadeFn
balanceView = MkFacadeFn (MkSig "balanceOf" [TAddress] [TUint256]) View

||| A view function with two returns in a fixed order.
multiRet : FacadeFn
multiRet = MkFacadeFn (MkSig "positions" [TAddress] [TUint256, TAddress]) View

||| The same transfer signature with a different (uint256) return type.
transferUintRet : FacadeFn
transferUintRet = MkFacadeFn (MkSig "transfer" [TAddress, TUint256] [TUint256]) View

||| 32 zero bytes, hex-encoded (EIP-1014 example salt).
create2SaltZeros : String
create2SaltZeros = "0000000000000000000000000000000000000000000000000000000000000000"

||| keccak256 of init code 0x00 (EIP-1014 example 1 init code hash).
create2InitCodeHash00 : String
create2InitCodeHash00 = "bc36789e7a1e281436464229828f817d6612f7b477d66591ff96a9e064bcc98a"

||| EIP-1014 example 1 expected CREATE2 address for a zero factory and salt.
create2KnownAddress : Integer
create2KnownAddress = 0x4D1A2e2bB4F88F0250f26Ffff098B0b30B26BF38

||| Count the rendered `function` keywords in an output string.
fnCount : String -> Nat
fnCount out = length (filter (== "function") (words out))

-- =============================================================================
-- Test Definitions (SPEC-Test Parity)
-- =============================================================================

||| FCD_ENV_001: the rendered output begins with an SPDX license identifier line.
export
test_FCD_ENV_001 : IO Bool
test_FCD_ENV_001 =
  let out = facadeSolidity "EnvFacade" [transferView]
  in pure (isPrefixOf "// SPDX-License-Identifier: MIT" out)

||| FCD_ENV_002: the rendered output declares pragma solidity ^0.8.20.
export
test_FCD_ENV_002 : IO Bool
test_FCD_ENV_002 =
  let out = facadeSolidity "EnvFacade" [transferView]
  in pure (isInfixOf "pragma solidity ^0.8.20;" out)

||| FCD_ENV_003: every rendered function is wrapped in contract <name> { ... }.
export
test_FCD_ENV_003 : IO Bool
test_FCD_ENV_003 =
  let out = facadeSolidity "ExplorerFacade" [transferView]
  in pure (isInfixOf "contract ExplorerFacade {" out && isSuffixOf "}\n" out)

||| FCD_FN_001: exactly one external function per FacadeFn, args types only.
export
test_FCD_FN_001 : IO Bool
test_FCD_FN_001 =
  let out = facadeSolidity "FnFacade" [transferView]
  in pure (isInfixOf "function transfer(address,uint256) external view returns (bool) {}" out
           && fnCount out == 1)

||| FCD_FN_002: a View FacadeFn renders external view.
export
test_FCD_FN_002 : IO Bool
test_FCD_FN_002 =
  let out = facadeSolidity "FnFacade" [balanceView]
  in pure (isInfixOf "function balanceOf(address) external view returns (uint256) {}" out)

||| FCD_FN_003: a Payable FacadeFn renders external payable.
export
test_FCD_FN_003 : IO Bool
test_FCD_FN_003 =
  let out = facadeSolidity "FnFacade" [depositPayable]
  in pure (isInfixOf "function deposit(uint256) external payable {}" out)

||| FCD_FN_004: a Nonpayable FacadeFn renders plain external with no extra keyword.
export
test_FCD_FN_004 : IO Bool
test_FCD_FN_004 =
  let out = facadeSolidity "FnFacade" [transferNonpayable]
  in pure (isInfixOf "function transfer(address,uint256) external returns (bool) {}" out
           && not (isInfixOf "view" out)
           && not (isInfixOf "payable" out))

||| FCD_FN_005: every emitted function has an empty body.
export
test_FCD_FN_005 : IO Bool
test_FCD_FN_005 =
  let out = facadeSolidity "FnFacade" [noopNonpayable, depositPayable, balanceView]
  in pure (isInfixOf "function noop() external {}" out
           && isInfixOf "function deposit(uint256) external payable {}" out
           && isInfixOf "function balanceOf(address) external view returns (uint256) {}" out)

||| FCD_RET_001: an empty rets list emits no returns clause.
export
test_FCD_RET_001 : IO Bool
test_FCD_RET_001 =
  let out = facadeSolidity "RetFacade" [noopNonpayable]
  in pure (isInfixOf "function noop() external {}" out && not (isInfixOf "returns" out))

||| FCD_RET_002: a single ret emits returns with that one Solidity type.
export
test_FCD_RET_002 : IO Bool
test_FCD_RET_002 =
  let out = facadeSolidity "RetFacade" [balanceView]
  in pure (isInfixOf "returns (uint256)" out)

||| FCD_RET_003: multiple rets emit comma-separated Solidity types in order.
export
test_FCD_RET_003 : IO Bool
test_FCD_RET_003 =
  let out = facadeSolidity "RetFacade" [multiRet]
  in pure (isInfixOf "returns (uint256,address)" out)

||| FCD_CANON_001: each FacadeFn renders as one
||| name(argTypes)->(retTypes):mutability line with types and mutability included.
export
test_FCD_CANON_001 : IO Bool
test_FCD_CANON_001 =
  pure (facadeCanonical [transferView] == "transfer(address,uint256)->(bool):view"
        && facadeCanonical [multiRet] == "positions(address)->(uint256,address):view"
        && facadeCanonical [depositPayable] == "deposit(uint256)->():payable"
        && facadeCanonical [noopNonpayable] == "noop()->():nonpayable")

||| FCD_CANON_002: the canonical output is order-insensitive because the lines
||| are sorted ascending before joining.
export
test_FCD_CANON_002 : IO Bool
test_FCD_CANON_002 =
  let set = [transferView, depositPayable, balanceView]
      permuted = [balanceView, transferView, depositPayable]
  in pure (facadeCanonical set == facadeCanonical permuted
           && facadeCanonical set
              == "balanceOf(address)->(uint256):view\n"
                 ++ "deposit(uint256)->():payable\n"
                 ++ "transfer(address,uint256)->(bool):view")

||| FCD_HASH_001: facadeShapeHash is the lowercase-hex keccak256 of facadeCanonical.
export
test_FCD_HASH_001 : IO Bool
test_FCD_HASH_001 =
  let set = [transferView, depositPayable]
  in pure (facadeShapeHash set == keccak256Hex (facadeCanonical set)
           && length (unpack (facadeShapeHash set)) == 64)

||| FCD_HASH_002: a return-type-only difference produces a different shape hash.
export
test_FCD_HASH_002 : IO Bool
test_FCD_HASH_002 =
  pure (facadeShapeHash [transferView] /= facadeShapeHash [transferUintRet])

||| FCD_HASH_003: a mutability-only difference produces a different shape hash.
export
test_FCD_HASH_003 : IO Bool
test_FCD_HASH_003 =
  pure (facadeShapeHash [transferView] /= facadeShapeHash [transferNonpayable])

||| FCD_ADDR_001: facadeCreate2Address matches the EIP-1014 example vector:
||| low 20 bytes of keccak256(0xff ++ 20-byte factory ++ 32-byte salt ++
||| 32-byte initCodeHash).
export
test_FCD_ADDR_001 : IO Bool
test_FCD_ADDR_001 =
  pure (facadeCreate2Address 0 create2SaltZeros create2InitCodeHash00 == create2KnownAddress)

public export
allTests : List TestDef
allTests = [
  -- Contract Envelope
  test "FCD_ENV_001" "Output begins with an SPDX license identifier line" (Runs test_FCD_ENV_001),
  test "FCD_ENV_002" "Output declares pragma solidity ^0.8.20" (Runs test_FCD_ENV_002),
  test "FCD_ENV_003" "Output wraps functions in contract <name> block" (Runs test_FCD_ENV_003),

  -- Function Rendering
  test "FCD_FN_001" "One external function per FacadeFn with types-only args" (Runs test_FCD_FN_001),
  test "FCD_FN_002" "View mutability renders external view" (Runs test_FCD_FN_002),
  test "FCD_FN_003" "Payable mutability renders external payable" (Runs test_FCD_FN_003),
  test "FCD_FN_004" "Nonpayable renders plain external" (Runs test_FCD_FN_004),
  test "FCD_FN_005" "Every emitted function has an empty body" (Runs test_FCD_FN_005),

  -- Return Clause
  test "FCD_RET_001" "Empty rets emit no returns clause" (Runs test_FCD_RET_001),
  test "FCD_RET_002" "Single ret emits one-type returns clause" (Runs test_FCD_RET_002),
  test "FCD_RET_003" "Multiple rets emit comma-separated returns clause" (Runs test_FCD_RET_003),

  -- Content Addressing
  test "FCD_CANON_001" "Canonical line is name(argTypes)->(retTypes):mutability" (Runs test_FCD_CANON_001),
  test "FCD_CANON_002" "Canonical output is order-insensitive via ascending sort" (Runs test_FCD_CANON_002),
  test "FCD_HASH_001" "Shape hash is hex keccak256 of the canonical form" (Runs test_FCD_HASH_001),
  test "FCD_HASH_002" "Return-type-only difference yields a different hash" (Runs test_FCD_HASH_002),
  test "FCD_HASH_003" "Mutability-only difference yields a different hash" (Runs test_FCD_HASH_003),
  test "FCD_ADDR_001" "CREATE2 address matches EIP-1014 vector" (Runs test_FCD_ADDR_001)
]

-- =============================================================================
-- Test Runner
-- =============================================================================

||| Run all tests, counting passed / failed / pending.
|||
||| Prints one `[PASS]` / `[FAIL]` / `[PENDING]` line per row, then a
||| `Results: P passed, F failed, N pending` summary. Exits non-zero only
||| when at least one test failed; pending never fails the run.
export
runAllTests : IO ()
runAllTests = do
  results <- traverse runOne allTests
  let (passed, failed, pending) = tally (map fst results)
  _ <- traverse (putStrLn . snd) results
  if pending > 0
    then putStrLn ("[WARNING] " ++ show pending ++ " pending test(s): declared but not yet verified")
    else pure ()
  putStrLn ""
  putStrLn $ "Results: " ++ show passed ++ " passed, "
          ++ show failed ++ " failed, "
          ++ show pending ++ " pending"
  if failed > 0 then exitFailure else exitSuccess

export
main : IO ()
main = runAllTests
