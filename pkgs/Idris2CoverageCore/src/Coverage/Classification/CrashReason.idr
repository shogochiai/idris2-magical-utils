||| CRASH Reason Classification
|||
||| Classifies CRASH messages in --dumpcases output based on
||| Idris2 community feedback (dunham, Thomas):
|||
|||   - "No clauses in ..."        -> Excluded (void etc, safe to exclude)
|||   - "Unhandled input for ..."  -> Bug (partial code, coverage gap)
|||   - "Nat case not covered"     -> Optimizer artifact (non-semantic)
|||   - Other                      -> Unknown (never exclude, investigate)
|||
||| This classification enables semantic coverage analysis that distinguishes
||| between genuine coverage gaps and expected/impossible branches.
module Coverage.Classification.CrashReason

import Data.String

%default total

-- =============================================================================
-- CRASH Reason Types
-- =============================================================================

||| Reason for CRASH in --dumpcases output
|||
||| Based on Idris2 community feedback:
|||   - CrashNoClauses: void/uninhabited types, EXCLUDED from denominator
|||   - CrashUnhandledInput: partial code bug, INCLUDED as coverage gap
|||   - CrashOptimizerNat: Nat->Integer translation artifact, non-semantic
|||   - CrashUnknown: Other CRASHes, conservative (never exclude)
public export
data CrashReason : Type where
  ||| "No clauses in ..." - empty function (void etc), excluded from denominator
  CrashNoClauses : CrashReason
  ||| "Unhandled input for ..." - partial code bug, included as coverage gap
  CrashUnhandledInput : CrashReason
  ||| "Nat case not covered" - optimizer artifact, non-semantic warning
  CrashOptimizerNat : CrashReason
  ||| Unknown CRASH - never exclude, show in Unknown bucket for investigation
  CrashUnknown : String -> CrashReason

public export
Show CrashReason where
  show CrashNoClauses = "NoClauses"
  show CrashUnhandledInput = "UnhandledInput"
  show CrashOptimizerNat = "OptimizerNat"
  show (CrashUnknown msg) = "Unknown(" ++ msg ++ ")"

public export
Eq CrashReason where
  CrashNoClauses == CrashNoClauses = True
  CrashUnhandledInput == CrashUnhandledInput = True
  CrashOptimizerNat == CrashOptimizerNat = True
  CrashUnknown m1 == CrashUnknown m2 = m1 == m2
  _ == _ = False

-- =============================================================================
-- CRASH Message Classification
-- =============================================================================

||| Determine CrashReason from CRASH message text
|||
||| Order matters: more specific patterns first
|||
||| @msg The CRASH message string from --dumpcases output
public export
classifyCrashMessage : String -> CrashReason
classifyCrashMessage msg =
  -- Most specific first
  if isInfixOf "No clauses in" msg then CrashNoClauses
  else if isInfixOf "Unhandled input for" msg then CrashUnhandledInput
  else if isInfixOf "Nat case not covered" msg then CrashOptimizerNat
  -- Fallback: everything else is Unknown (conservative - never exclude)
  else CrashUnknown msg

-- =============================================================================
-- Coverage Semantics
-- =============================================================================

||| Should this CRASH be excluded from coverage denominator?
|||
||| Only CrashNoClauses (void/uninhabited) is safe to exclude.
||| All others should be counted or investigated.
public export
shouldExcludeFromDenominator : CrashReason -> Bool
shouldExcludeFromDenominator CrashNoClauses = True
shouldExcludeFromDenominator _ = False

||| Is this a genuine coverage bug that should be fixed?
|||
||| CrashUnhandledInput indicates partial code that needs implementation.
public export
isCoverageBug : CrashReason -> Bool
isCoverageBug CrashUnhandledInput = True
isCoverageBug _ = False

||| Is this an optimizer artifact (non-semantic)?
|||
||| CrashOptimizerNat is from Nat->Integer translation, not real code.
public export
isOptimizerArtifact : CrashReason -> Bool
isOptimizerArtifact CrashOptimizerNat = True
isOptimizerArtifact _ = False

||| Should this CRASH be investigated further?
|||
||| Unknown CRASHes need investigation to classify properly.
public export
needsInvestigation : CrashReason -> Bool
needsInvestigation (CrashUnknown _) = True
needsInvestigation _ = False

-- =============================================================================
-- Case Kind (Canonical vs NonCanonical)
-- =============================================================================

||| Case classification for semantic coverage
|||
||| Canonical cases are reachable and counted in the denominator.
||| NonCanonical cases are CRASH branches with specific reasons.
public export
data CaseKind : Type where
  ||| Reachable case - included in coverage denominator
  Canonical : CaseKind
  ||| Unreachable/undefined - handled based on CrashReason
  NonCanonical : CrashReason -> CaseKind

public export
Show CaseKind where
  show Canonical = "Canonical"
  show (NonCanonical r) = "NonCanonical(" ++ show r ++ ")"

public export
Eq CaseKind where
  Canonical == Canonical = True
  NonCanonical r1 == NonCanonical r2 = r1 == r2
  _ == _ = False

||| Check if case should be counted in coverage denominator
public export
isCountedCase : CaseKind -> Bool
isCountedCase Canonical = True
isCountedCase (NonCanonical reason) = not (shouldExcludeFromDenominator reason)
