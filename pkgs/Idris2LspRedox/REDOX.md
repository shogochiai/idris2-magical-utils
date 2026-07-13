# idris2-lsp-redox

This package vendors `idris-community/idris2-lsp` at commit
`800fe94ccd37855ada2f91ac4a20281f29a4ebff` and adds the Redox path snapshot
extension interface.

`idris2-lsp-redox` is the path-observable snapshot manager. It does not choose
intent, define project SpecIds, run AGA, authorize release, or certify docs,
tests, evidence, or memory. Those are external responsibilities. In the Luci
integration, Luci is the implementation of the `ExternalRestoreQEQ` step and
projects its accepted result back into a path promotion witness.

## Custom Methods

The initialize result advertises:

```json
{
  "redoxPathSnapshots": {
    "protocolVersion": "1.1",
    "schemaVersion": "1.1",
    "methods": [
      "idris2/redox/capabilities",
      "idris2/dumpPaths",
      "idris2/diffPaths",
      "idris2/getPathSnapshot",
      "idris2/getCurrentVerified",
      "idris2/preparePathPromotion",
      "idris2/promotePathSnapshot",
      "idris2/rejectPathSnapshot",
      "idris2/exportPathSnapshotStore",
      "idris2/importPathSnapshotStore"
    ],
    "canonicalEncoding": "redox-canonical-json-1"
  }
}
```

`idris2/redox/capabilities` returns the same capability object without requiring
an initialize round trip.

`idris2/dumpPaths` creates a working snapshot from synchronized LSP document
versions. If the request includes `expectedVersions`, it must exactly match the
server version map or the call returns `StaleVersion`.

The request may provide `projectId` directly. If absent, the server derives it
from `projectIdentity`; this is the integration hook for a project identity such
as `RepoId`, `WorktreeId`, `BaseCommit`, `HarnessVersion`, and
`SpecRegistryDigest`. The snapshot payload also records `specRegistryDigest`, so
a verified path snapshot cannot silently float across a different SpecId binding
registry.

Until compiler-native path extraction is wired in, callers may provide a
canonical `paths` array. Without `paths`, the snapshot is marked `Failed`, not
`Complete`; downstream diff conservatively returns fallback facts instead of
claiming no impact. The snapshot and diff include:

- `impactKind = "compiler-path"`
- `soundness = "over-approximation"`
- `impactPrecision` as one of `Exact`, `DefinitionFallback`, `ModuleFallback`,
  or `ProjectFallback`

`idris2/diffPaths` compares two stored snapshots by lineage id and returns
removed, added, modified, unchanged, relocated, fallback facts, direct impact,
closed impact, completeness, precision, and both compatibility and path-prefixed
digest fields. The 1.0 fields `impactDirect`, `impactClosed`, and `diffDigest`
are retained; 1.1 consumers should prefer `pathImpactDirect`,
`pathImpactClosed`, `pathImpactDigest`, and `pathDiffDigest`.

`idris2/getCurrentVerified` returns the current verified snapshot pointer for a
project. `idris2/getPathSnapshot` returns a stored payload and lifecycle state.

## Promotion

Promotion requires a path promotion witness. The server validates that the
witness is bound to the exact working snapshot, parent snapshot when present,
path diff digest, path impact digest, verified path SpecId set, QEQ verifier
version, and bound registry digest when the payload has one.

Required witness fields:

```json
{
  "workingSnapshotId": "redox-snapshot-...",
  "parentSnapshotId": "redox-snapshot-...",
  "pathDiffDigest": "len-...",
  "pathImpactDigest": "len-...",
  "verifiedPathSpecIds": ["SPEC-..."],
  "specRegistryDigest": "len-...",
  "qeqVerifierVersion": "lazy-qeq-...",
  "result": 1
}
```

For an initial snapshot with no parent, `pathDiffDigest` is not accepted; the
impact projection is the finite SpecId set present in the snapshot payload.

`idris2/preparePathPromotion` validates the witness and writes a prepared record
addressed by `preparedDigest`. `idris2/promotePathSnapshot` accepts either the
full witness payload or a `preparedDigest`. Promotion is idempotent after the
snapshot is already `Verified`, and it refuses to promote a snapshot that is no
longer the current working snapshot for its project.

`idris2/rejectPathSnapshot` rejects `Working` or `Prepared` snapshots and is
idempotent after `Rejected`.

## Recovery

`idris2/exportPathSnapshotStore` exports the in-memory payload, lifecycle,
current pointer, prepared promotion, and witness maps as canonical JSON.

`idris2/importPathSnapshotStore` imports that JSON back into the server. This is
the LSP side of the content-addressed recovery contract; Luci still owns source
tree storage, execution ledger, graph snapshot, and project QEQ certificate
storage.

## Luci Integration Boundary

The path server supplies only `U_path`. Luci must federate it with graph,
registry, artifact, and intent impact before constructing project `Qsave`.
Therefore `pathImpactClosed` is not the full Redox save impact by itself.

The expected composition is:

```text
dumpPaths -> diffPaths -> pathImpactClosed
          -> Luci ExternalRestoreQEQ
          -> Luci Save Certificate
          -> Path Promotion Witness
          -> preparePathPromotion -> promotePathSnapshot
```

The witness must bind the final post-AGA snapshot, not the initial edit snapshot.
If Luci changes source during AGA, it must dump again, recompute diff and impact,
and only promote the final stable snapshot.

## Build

```bash
idris2 --build pkgs/Idris2LspRedox/idris2-lsp.ipkg
```

The executable is `idris2-lsp-redox`.

The current protocol schema is in
[`schema/redox-path-snapshot-1.1.json`](schema/redox-path-snapshot-1.1.json).
The 1.0 schema is retained for historical consumers.
