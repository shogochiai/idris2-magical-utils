module Execution.Standardization.Types

%default total

public export
data BackendKind
  = IcWasmBackend
  | EvmBackend
  | WebBackend

public export
Eq BackendKind where
  IcWasmBackend == IcWasmBackend = True
  EvmBackend == EvmBackend = True
  WebBackend == WebBackend = True
  _ == _ = False

public export
data BudgetDomain
  = Instructions
  | Gas
  | BrowserRuntime

public export
Eq BudgetDomain where
  Instructions == Instructions = True
  Gas == Gas = True
  BrowserRuntime == BrowserRuntime = True
  _ == _ = False

public export
data ValueRole
  = IdentifierRole
  | TimestampRole
  | BalanceRole
  | CyclesRole
  | BytesRole
  | PageCountRole
  | OffsetRole
  | LimitRole
  | CountRole
  | BlockNumberRole
  | NonceRole
  | GasRole
  | CoverageOffsetRole

public export
Eq ValueRole where
  IdentifierRole == IdentifierRole = True
  TimestampRole == TimestampRole = True
  BalanceRole == BalanceRole = True
  CyclesRole == CyclesRole = True
  BytesRole == BytesRole = True
  PageCountRole == PageCountRole = True
  OffsetRole == OffsetRole = True
  LimitRole == LimitRole = True
  CountRole == CountRole = True
  BlockNumberRole == BlockNumberRole = True
  NonceRole == NonceRole = True
  GasRole == GasRole = True
  CoverageOffsetRole == CoverageOffsetRole = True
  _ == _ = False

public export
record ExecutionProfile where
  constructor MkExecutionProfile
  backend : BackendKind
  profileName : String
  primaryBudget : BudgetDomain
  prefersMachineIntegers : Bool
  forbidsLargePeanoNaturalsInHotPaths : Bool
  notes : List String
