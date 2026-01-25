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
