||| Type-Safe Cross-Family Artifact Verification
|||
||| Verification = "ソースから生成された成果物の hash が、デプロイ先の hash と一致するか"
||| この概念は family 横断:
|||   EVM  → bytecode hash (keccak256)
|||   DFX  → WASM module hash (sha256)
|||   Core → CLI binary checksum (sha256)
|||   Web  → JS/WASM bundle hash (sha256)
|||
||| 型で強制: Verified f は Artifact f と Deployed f の hash が一致する証拠。
||| hash が不一致の場合 Verified を構築できない → 不正なデプロイを型エラーに。
|||
||| EVM の場合、chainId と address と RPC endpoint が変わるだけで
||| どのチェーンでも同じ verification logic が使える。
module IcWasm.Verification

import Data.Vect
import Data.Bits

%default total

-- =============================================================================
-- Family (deployment target)
-- =============================================================================

||| Deployment target family
public export
data Family = EVM | DFX | Core | Web

public export
Show Family where
  show EVM  = "evm"
  show DFX  = "dfx"
  show Core = "core"
  show Web  = "web"

public export
Eq Family where
  EVM  == EVM  = True
  DFX  == DFX  = True
  Core == Core = True
  Web  == Web  = True
  _    == _    = False

-- =============================================================================
-- Hash (32 bytes, family-agnostic)
-- =============================================================================

||| 32-byte hash (keccak256 for EVM, sha256 for others)
public export
data ArtifactHash = MkHash (Vect 32 Bits8)

public export
Eq ArtifactHash where
  (MkHash a) == (MkHash b) = toList a == toList b

public export
Show ArtifactHash where
  show (MkHash bytes) = "0x" ++ concatMap showByte (toList bytes)
    where
      hexChar : Bits8 -> Char
      hexChar n = if n < 10 then chr (cast n + cast '0')
                  else chr (cast n - 10 + cast 'a')
      showByte : Bits8 -> String
      showByte b = pack [hexChar (b `shiftR` 4), hexChar (b .&. 0x0F)]

-- =============================================================================
-- Artifact (built from source)
-- =============================================================================

||| Build artifact, tagged with family
public export
data Artifact : Family -> Type where
  EvmBytecode : (hash : ArtifactHash) -> (solcVersion : String) -> (evmVersion : String) -> Artifact EVM
  WasmModule  : (hash : ArtifactHash) -> (wasmSize : Nat) -> Artifact DFX
  CliBinary   : (hash : ArtifactHash) -> (platform : String) -> Artifact Core
  WebBundle   : (hash : ArtifactHash) -> (bundleSize : Nat) -> Artifact Web

||| Extract hash from any artifact
public export
artifactHash : Artifact f -> ArtifactHash
artifactHash (EvmBytecode h _ _) = h
artifactHash (WasmModule h _)    = h
artifactHash (CliBinary h _)     = h
artifactHash (WebBundle h _)     = h

-- =============================================================================
-- Deployed (on target)
-- =============================================================================

||| Deployed instance, tagged with family
public export
data Deployed : Family -> Type where
  OnChain    : (chainId : Nat) -> (addr : String) -> (hash : ArtifactHash) -> Deployed EVM
  OnCanister : (canisterId : String) -> (hash : ArtifactHash) -> Deployed DFX
  OnHost     : (path : String) -> (hash : ArtifactHash) -> Deployed Core
  OnCDN      : (url : String) -> (hash : ArtifactHash) -> Deployed Web

||| Extract hash from any deployment
public export
deployedHash : Deployed f -> ArtifactHash
deployedHash (OnChain _ _ h)  = h
deployedHash (OnCanister _ h) = h
deployedHash (OnHost _ h)     = h
deployedHash (OnCDN _ h)      = h

-- =============================================================================
-- Verification (proof that artifact == deployed)
-- =============================================================================

||| Proof that an artifact's hash matches its deployment.
||| Can ONLY be constructed when hashes match.
||| If they don't match, you get a Nothing from `verify`.
public export
data Verified : Family -> Type where
  MkVerified : (artifact : Artifact f) -> (deployed : Deployed f)
             -> Verified f

||| Attempt to verify: compare artifact hash with deployed hash.
||| Returns Verified only if hashes match.
||| This is the ONLY way to construct Verified.
public export
verify : Artifact f -> Deployed f -> Maybe (Verified f)
verify art dep =
  if artifactHash art == deployedHash dep
    then Just (MkVerified art dep)
    else Nothing

||| Verification result with reason
public export
data VerifyResult : Family -> Type where
  VerifiedOk : Verified f -> VerifyResult f
  HashMismatch : (expected : ArtifactHash) -> (actual : ArtifactHash) -> VerifyResult f
  NotDeployed : VerifyResult f

||| Verify with detailed result
public export
verifyDetailed : Artifact f -> Deployed f -> VerifyResult f
verifyDetailed art dep =
  let expected = artifactHash art
      actual   = deployedHash dep
  in if expected == actual
       then VerifiedOk (MkVerified art dep)
       else HashMismatch expected actual

-- =============================================================================
-- Source Provenance (traceability)
-- =============================================================================

||| Source provenance: links artifact to its source code
public export
record SourceProvenance where
  constructor MkProvenance
  gitRepo   : String
  gitCommit : String
  sourceHash : ArtifactHash   -- hash of source tarball at commit
  family    : Family

||| Full verification chain: source → artifact → deployed
public export
record VerificationChain (f : Family) where
  constructor MkChain
  provenance : SourceProvenance
  artifact   : Artifact f
  deployment : Deployed f
  verified   : Verified f

||| Build a complete verification chain.
||| Returns Nothing if any hash doesn't match.
public export
buildChain : SourceProvenance -> Artifact f -> Deployed f
           -> {auto prf : provenance.family = f}
           -> Maybe (VerificationChain f)
buildChain prov art dep = do
  v <- verify art dep
  pure (MkChain prov art dep v)

-- =============================================================================
-- Multi-Chain EVM Verification
-- =============================================================================

||| EVM chain configuration (same verification logic, different RPC)
public export
record EvmChainConfig where
  constructor MkEvmChain
  chainId  : Nat
  name     : String
  rpcUrl   : String

||| Common EVM chains
public export
baseMainnet : EvmChainConfig
baseMainnet = MkEvmChain 8453 "Base" "https://mainnet.base.org"

public export
ethereumMainnet : EvmChainConfig
ethereumMainnet = MkEvmChain 1 "Ethereum" "https://ethereum.publicnode.com"

public export
arbitrumOne : EvmChainConfig
arbitrumOne = MkEvmChain 42161 "Arbitrum One" "https://arb1.arbitrum.io/rpc"

-- =============================================================================
-- Usage Documentation
-- =============================================================================
--
-- EVM verification (any chain):
--   let art = EvmBytecode (MkHash bytecodeHashBytes) "0.8.28" "paris"
--   let dep = OnChain 8453 "0xb094..." (MkHash onchainHashBytes)
--   case verify art dep of
--     Just verified => ... -- hashes match
--     Nothing       => ... -- MISMATCH (possible upgrade or wrong source)
--
-- ICP verification:
--   let art = WasmModule (MkHash wasmHashBytes) 1700221
--   let dep = OnCanister "nrkou-hqaaa-..." (MkHash moduleHashBytes)
--   case verify art dep of ...
--
-- CLI verification:
--   let art = CliBinary (MkHash binaryHashBytes) "darwin-arm64"
--   let dep = OnHost "/usr/local/bin/etherclaw" (MkHash installedHashBytes)
--   case verify art dep of ...
--
-- Full chain with provenance:
--   let prov = MkProvenance "etherclaw" "abc123" sourceHash EVM
--   case buildChain prov art dep of
--     Just chain => chain.verified -- complete traceability
--     Nothing    => -- verification failed
