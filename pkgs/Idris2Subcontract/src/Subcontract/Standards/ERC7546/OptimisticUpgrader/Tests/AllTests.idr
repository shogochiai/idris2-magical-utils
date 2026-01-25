||| OptimisticUpgrader Tests
|||
||| SPEC-Test Parity for OptimisticUpgrader module
module Subcontract.Standards.ERC7546.OptimisticUpgrader.Tests.AllTests

import Subcontract.Standards.ERC7546.OptimisticUpgrader.Storages.Slots
import Subcontract.Standards.ERC7546.OptimisticUpgrader.Storages.Schema
import Subcontract.Standards.ERC7546.OptimisticUpgrader.Functions.Core

-- =============================================================================
-- Test Infrastructure
-- =============================================================================

public export
record TestDef where
  constructor MkTest
  specId : String
  description : String
  -- test : IO Bool  -- Uncomment when implementing

public export
test : String -> String -> TestDef
test sid desc = MkTest sid desc

-- =============================================================================
-- Test Definitions (SPEC-Test Parity)
-- =============================================================================

public export
allTests : List TestDef
allTests = [
  -- Proposal lifecycle
  test "OU_PROP_001" "proposeUpgrade creates proposal with correct state",
  test "OU_PROP_002" "proposeUpgrade fails if proposal already exists",
  
  -- Voting
  test "OU_VOTE_001" "vote records auditor approval",
  test "OU_VOTE_002" "vote fails for non-registered auditor",
  test "OU_VOTE_003" "vote fails for already-voted auditor",
  
  -- Tally
  test "OU_TALLY_001" "tally executes upgrade when n-of-n reached",
  test "OU_TALLY_002" "tally fails when quorum not reached",
  
  -- Storage
  test "OU_STOR_001" "Slots use ERC-7201 namespaced values",
  test "OU_STOR_002" "Schema correctly encodes Proposal record"
]
