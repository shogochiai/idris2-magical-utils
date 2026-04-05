module Execution.Standardization.Model

import Execution.Standardization.Types

%default total

public export
icWasmExecutionProfile : ExecutionProfile
icWasmExecutionProfile =
  MkExecutionProfile
    IcWasmBackend
    "IC WASM Execution Profile"
    Instructions
    True
    True
    [ "Single-message instruction budgets dominate hot-path design."
    , "Stable memory offsets, page counts, bytes, timestamps, and cycles should use machine-integer wrappers."
    ]

public export
evmExecutionProfile : ExecutionProfile
evmExecutionProfile =
  MkExecutionProfile
    EvmBackend
    "EVM Execution Profile"
    Gas
    True
    True
    [ "Gas-heavy paths should avoid Peano naturals and allocation-heavy encodings."
    , "Wei, gas, nonce, block numbers, and storage offsets should use explicit wrappers."
    ]

public export
webExecutionProfile : ExecutionProfile
webExecutionProfile =
  MkExecutionProfile
    WebBackend
    "Web Execution Profile"
    BrowserRuntime
    True
    False
    [ "JS/V8 does not have ICP/EVM-style per-message hard budgets, but instrumentation overhead still matters."
    , "Coverage offsets and byte ranges should use explicit wrappers when mapping runtime observations."
    ]

public export
allExecutionProfiles : List ExecutionProfile
allExecutionProfiles =
  [ icWasmExecutionProfile
  , evmExecutionProfile
  , webExecutionProfile
  ]

public export
executionProfileName : ExecutionProfile -> String
executionProfileName (MkExecutionProfile _ name _ _ _ _) = name

public export
icWasmExecutionProfileName : String
icWasmExecutionProfileName = executionProfileName icWasmExecutionProfile

public export
evmExecutionProfileName : String
evmExecutionProfileName = executionProfileName evmExecutionProfile

public export
webExecutionProfileName : String
webExecutionProfileName = executionProfileName webExecutionProfile
