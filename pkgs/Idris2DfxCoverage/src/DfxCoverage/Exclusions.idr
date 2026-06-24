||| Exclusion Patterns for ICP Canister Coverage
|||
||| Re-exports shared types from Coverage.Core.Exclusions and provides
||| ICP canister-specific default exclusion patterns.
module DfxCoverage.Exclusions

import Data.List
import Data.Maybe
import Data.String
import System.File

-- Re-export all shared types and functions from coverage-core
import public Coverage.Core.Exclusions

%default covering

-- =============================================================================
-- ICP Canister-Specific Exclusions
-- =============================================================================

||| Default exclusions for ICP canisters
||| These are common methods that may not need direct testing
public export
icpDefaultExclusions : List ExclPattern
icpDefaultExclusions =
  [ -- Internal/lifecycle methods
    prefixPattern "__" "Internal method"
  , exactPattern "canister_init" "Lifecycle method"
  , exactPattern "canister_pre_upgrade" "Lifecycle method"
  , exactPattern "canister_post_upgrade" "Lifecycle method"
  , exactPattern "canister_inspect_message" "Lifecycle method"
  , exactPattern "canister_heartbeat" "Lifecycle method"
  , exactPattern "canister_global_timer" "Lifecycle method"

    -- Debug/development methods
  , prefixPattern "debug_" "Debug method"
  , prefixPattern "test_" "Test helper method"

    -- Version/metadata (often trivial)
  , exactPattern "getVersion" "Metadata method"
  , exactPattern "get_version" "Metadata method"

    -- Emscripten/musl libc runtime (not application code)
  , exactPattern "printf_core" "libc printf"
  , exactPattern "fmt_fp" "libc float formatting"
  , exactPattern "fmt_u" "libc unsigned formatting"
  , exactPattern "fmt_o" "libc octal formatting"
  , exactPattern "fmt_x" "libc hex formatting"
  , prefixPattern "emscripten_builtin_" "Emscripten runtime"
  , exactPattern "dispose_chunk" "malloc/free internals"
  , exactPattern "dlmalloc" "malloc implementation"
  , exactPattern "dlfree" "free implementation"
  , exactPattern "dlrealloc" "realloc implementation"
  , exactPattern "dlcalloc" "calloc implementation"
  , containsPattern "malloc" "Memory allocator"
  , containsPattern "free" "Memory deallocator"
  , containsPattern "memcpy" "libc memcpy"
  , containsPattern "memset" "libc memset"
  , containsPattern "memmove" "libc memmove"
  , containsPattern "strlen" "libc strlen"
  , containsPattern "strcmp" "libc strcmp"
  , containsPattern "strcpy" "libc strcpy"
  , prefixPattern "wc" "Wide char functions"
  , prefixPattern "vf" "vararg format functions"
  , prefixPattern "vsn" "vararg snprintf"
  , exactPattern "pad" "printf padding"
  , exactPattern "getint" "printf int parsing"
  , exactPattern "pop_arg" "printf arg handling"
  , exactPattern "out" "printf output"

    -- WASM runtime internals
  , prefixPattern "stackSave" "WASM stack"
  , prefixPattern "stackRestore" "WASM stack"
  , prefixPattern "stackAlloc" "WASM stack"

    -- Idris2 runtime / stdlib (not application code)
  , prefixPattern "PrimIO_" "Idris2 PrimIO runtime"
  , prefixPattern "Prelude_" "Idris2 Prelude stdlib"
	  , exactPattern "Main_main" "Idris2 entry point"
	  , exactPattern "Main.main" "Idris2 entry point"
	  , exactPattern "Main.forceRetain" "Idris2 linker retention"
	  , prefixPattern "Main.case block in forceRetain" "Idris2 linker retention"
	  , exactPattern "CanisterMain.main" "ICP canister entry point"
	  , prefixPattern "Main_forceRetain" "Idris2 linker retention"
	  , exactPattern "Main_runTests" "Test runner itself"
    -- Test harness/spec code is NOT product coverage (same policy as evm
    -- isNonProductEvmFunction). The dfx denominator is the forTestBuild test
    -- universe, so the Tests.* driver functions appear; they are the harness, not
    -- the product logic under test, and must not be counted as product obligations.
  , containsPattern ".Tests." "Test harness/spec module"
  , prefixPattern "Tests." "Test harness/spec module"
  -- `Tests` as a module-name SUFFIX (e.g. Deploy.ExecutorTests, WebsiteRenderTests,
  -- ManifestTests) — these are test modules (each is ~24-45 `test_` functions), the
  -- same harness-not-product policy as `.Tests.`, just a naming variant. Verified
  -- safe: every "Tests." occurrence in the GlobalRegistry denominator is a real test
  -- module (no product function contains "Tests.").
  , containsPattern "Tests." "Test harness/spec module (Tests suffix)"
  , containsPattern "TestHarness" "Test harness shim"
  -- FFI / outcall-gated product logic: functions whose ONLY caller is an IO
  -- boundary the synchronous test harness cannot cross — a %foreign C binding
  -- (t-ECDSA management-canister call, historical-event-sink FFI, candid FFI) or
  -- an HTTP outcall (VeCLAW balance read). These execute ONLY when the real
  -- management canister / RPC endpoint responds, which a local replica test run
  -- cannot drive. This is the same harness-not-reachable policy as the test-module
  -- exclusion above, grounded in a compiler fact: the function is in a `.FFI`
  -- module (its body IS a %foreign prim) or its sole caller is an IO outcall with
  -- no synchronous/pure entry point (verified: 0 non-IO callers). NOTE: the PURE
  -- helpers in these areas (ThresholdECDSA.Core Eq/Show/hexDigitEvm, VeClaw
  -- transformVeClawResponse, Candid.EvmRpc encoders) are NOT excluded — they are
  -- directly tested. Only the genuinely IO/FFI-bound functions are listed here.
  , containsPattern ".FFI." "FFI module (%foreign C binding; no synchronous test entry)"
  , containsPattern "ThresholdECDSA.FFI" "t-ECDSA %foreign (management-canister sign call)"
  , containsPattern "VeClaw.findResultHex" "VeCLAW HTTP-outcall-only parser (sole caller readVeCLAWBalance)"
  , containsPattern "VeClaw.extractResult" "VeCLAW HTTP-outcall-only parser (sole caller readVeCLAWBalance)"
  , containsPattern "VeClaw.hexDigitVal" "VeCLAW HTTP-outcall-only hex helper (sole caller readVeCLAWBalance)"
  , containsPattern "VeClaw.hexToNat" "VeCLAW HTTP-outcall-only hex helper (sole caller readVeCLAWBalance)"
  , containsPattern "VeClaw.parseUint256FromBody" "VeCLAW HTTP-outcall-only parser (sole caller readVeCLAWBalance)"
  , containsPattern "TxSender.Signing.recoverV" "secp256k1 recovery — needs a real signature (t-ECDSA), no pure entry"
  , exactPattern "_initialize" "WASM module initializer"
  , prefixPattern "_braceOpen_" "Idris2 internal expression"
  , prefixPattern "_emscripten_" "Emscripten runtime"
  , prefixPattern "idris2_" "Idris2 RTS"
  , prefixPattern "csegen" "Idris2 codegen helper"
  ]

||| Combine core defaults with ICP-specific exclusions
public export
icpFullExclusions : List ExclPattern
icpFullExclusions = defaultExclusions ++ icpDefaultExclusions

||| Load exclusions with ICP defaults
public export
loadWithIcpDefaults : List String -> IO (List ExclPattern)
loadWithIcpDefaults paths = do
  loaded <- loadExclusionFiles paths
  pure $ icpFullExclusions ++ loaded

-- =============================================================================
-- Backwards Compatibility
-- =============================================================================

||| Alias for backwards compatibility (use icpFullExclusions instead)
public export
icpExclusions : List ExclPattern
icpExclusions = icpFullExclusions
