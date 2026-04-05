module Coverage.Standardization.Idris2Interface

%default total

public export
data DeliveryPhase
  = DownstreamNow
  | RequiresIdris2Upstream

public export
record CompilerCapability where
  constructor MkCompilerCapability
  capabilityId : String
  title : String
  rationale : String
  deliveryPhase : DeliveryPhase

public export
minimumCapabilities : List CompilerCapability
minimumCapabilities =
  [ MkCompilerCapability
      "capability.provenance-tags"
      "Provenance-tagged case tree nodes"
      "Coverage tooling must distinguish user clauses, impossible clauses, partial completions, optimizer artifacts, and no-clause bodies without guessing from surface syntax."
      RequiresIdris2Upstream
  , MkCompilerCapability
      "capability.stable-obligation-ids"
      "Stable obligation identifiers"
      "Runtime hits must map to pre-optimization obligations rather than backend-generated nodes to prevent numerator leakage and over-100-percent coverage."
      RequiresIdris2Upstream
  , MkCompilerCapability
      "capability.machine-readable-dump"
      "Machine-readable elaboration output"
      "A structured output format is needed so downstream tools are not forced to parse human-oriented dumpcases text."
      RequiresIdris2Upstream
  , MkCompilerCapability
      "capability.unknown-classification"
      "Explicit unknown bucket"
      "If the compiler cannot justify a stronger classification, the downstream standard must preserve that uncertainty instead of silently excluding cases."
      DownstreamNow
  , MkCompilerCapability
      "capability.reproducibility-metadata"
      "Compiler and backend metadata in reports"
      "Coverage values need compiler version, backend, and profiling mode attached to be comparable across projects and papers."
      DownstreamNow
  ]
