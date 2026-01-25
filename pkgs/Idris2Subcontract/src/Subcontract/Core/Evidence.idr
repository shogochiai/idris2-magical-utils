||| Subcontract Core: Evidence (Observable Failure Data)
|||
||| MVP-1: Structured evidence for failures.
|||
||| EVM is deterministic - failures can be traced and reproduced.
||| Evidence captures the minimal information needed to:
||| - Understand what happened
||| - Reproduce the failure
||| - Support recovery decisions
module Subcontract.Core.Evidence

import public Subcontract.Core.Storable

%default total

-- =============================================================================
-- Evidence: Minimal Observable Data
-- =============================================================================

||| Evidence of what happened during execution.
||| Designed to be low-cost to construct and sufficient for debugging.
public export
record Evidence where
  constructor MkEvidence
  ||| Unique digest/hash of the failure context
  digest : Bits256
  ||| Human-readable tags (function name, context, etc)
  tags : List String
  ||| Storage slots that were read (for trace analysis)
  sloadSlots : List Bits256
  ||| Storage slots that were written (for state diff)
  sstoreSlots : List Bits256
  ||| External calls made: (target, selector)
  calls : List (Bits256, Bits256)

||| Empty evidence (minimal)
public export
emptyEvidence : Evidence
emptyEvidence = MkEvidence 0 [] [] [] []

||| Create evidence with just a tag
public export
tagEvidence : String -> Evidence
tagEvidence tag = MkEvidence 0 [tag] [] [] []

||| Create evidence with multiple tags
public export
tagsEvidence : List String -> Evidence
tagsEvidence ts = MkEvidence 0 ts [] [] []

||| Add a tag to existing evidence
public export
addTag : String -> Evidence -> Evidence
addTag t ev = { tags $= (t ::) } ev

||| Add sload slot to evidence
public export
addSload : Bits256 -> Evidence -> Evidence
addSload s ev = { sloadSlots $= (s ::) } ev

||| Add sstore slot to evidence
public export
addSstore : Bits256 -> Evidence -> Evidence
addSstore s ev = { sstoreSlots $= (s ::) } ev

||| Add external call to evidence
public export
addCall : Bits256 -> Bits256 -> Evidence -> Evidence
addCall target sel ev = { calls $= ((target, sel) ::) } ev

||| Set digest
public export
withDigest : Bits256 -> Evidence -> Evidence
withDigest d ev = { digest := d } ev

-- =============================================================================
-- Evidence Builder (Monadic Style)
-- =============================================================================

||| Evidence builder for accumulating evidence
public export
record EvidenceBuilder where
  constructor MkBuilder
  current : Evidence

||| Start building evidence
public export
startEvidence : EvidenceBuilder
startEvidence = MkBuilder emptyEvidence

||| Build with tag
public export
withTag : String -> EvidenceBuilder -> EvidenceBuilder
withTag t (MkBuilder ev) = MkBuilder (addTag t ev)

||| Build with sload
public export
withSload : Bits256 -> EvidenceBuilder -> EvidenceBuilder
withSload s (MkBuilder ev) = MkBuilder (addSload s ev)

||| Build with sstore
public export
withSstore : Bits256 -> EvidenceBuilder -> EvidenceBuilder
withSstore s (MkBuilder ev) = MkBuilder (addSstore s ev)

||| Build with call
public export
withCall : Bits256 -> Bits256 -> EvidenceBuilder -> EvidenceBuilder
withCall t s (MkBuilder ev) = MkBuilder (addCall t s ev)

||| Finalize evidence
public export
build : EvidenceBuilder -> Evidence
build (MkBuilder ev) = ev

-- =============================================================================
-- Evidence Merge
-- =============================================================================

||| Merge two evidence records
public export
mergeEvidence : Evidence -> Evidence -> Evidence
mergeEvidence e1 e2 = MkEvidence
  { digest = if e1.digest /= 0 then e1.digest else e2.digest
  , tags = e1.tags ++ e2.tags
  , sloadSlots = e1.sloadSlots ++ e2.sloadSlots
  , sstoreSlots = e1.sstoreSlots ++ e2.sstoreSlots
  , calls = e1.calls ++ e2.calls
  }

-- =============================================================================
-- Evidence Predicates
-- =============================================================================

||| Check if evidence has external calls
public export
hasExternalCalls : Evidence -> Bool
hasExternalCalls ev = not (null ev.calls)

||| Check if evidence has storage writes
public export
hasStorageWrites : Evidence -> Bool
hasStorageWrites ev = not (null ev.sstoreSlots)

||| Check if evidence has storage reads
public export
hasStorageReads : Evidence -> Bool
hasStorageReads ev = not (null ev.sloadSlots)

||| Count total operations in evidence
public export
operationCount : Evidence -> Nat
operationCount ev = 
  length ev.sloadSlots + length ev.sstoreSlots + length ev.calls
