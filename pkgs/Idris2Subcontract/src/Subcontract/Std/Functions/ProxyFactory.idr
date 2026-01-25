||| Subcontract Standard Function: ProxyFactory
|||
||| Deploys ERC-7546 Proxy contracts using CREATE2.
||| Each proxy queries Dictionary for implementation by selector.
|||
||| The proxy is compiled from ERC7546Proxy.idr in idris2-yul.
module Subcontract.Std.Functions.ProxyFactory

import EVM.Primitives
import Subcontract.Standards.ERC7546.Slots
import Subcontract.Core.FR

-- =============================================================================
-- ERC-7546 Proxy Bytecode (compiled from idris2-yul)
-- =============================================================================

||| Runtime bytecode size in bytes
||| Compiled from idris2-yul/examples/ERC7546Proxy.idr
PROXY_RUNTIME_SIZE : Integer
PROXY_RUNTIME_SIZE = 693

||| Event: ProxyDeployed(address indexed proxy, address indexed dictionary)
EVENT_PROXY_DEPLOYED : Integer
EVENT_PROXY_DEPLOYED = 0x1c4a72e0d9e5a42c9a3a0a4e8c3e8e4e5d6c7b8a9

||| Total init code size
INIT_CODE_SIZE : Integer
INIT_CODE_SIZE = 771

-- =============================================================================
-- Runtime Bytecode Embedding
-- =============================================================================

||| Copy the pre-compiled ERC7546 Proxy runtime bytecode to memory
||| Bytecode compiled from idris2-yul/examples/ERC7546Proxy.idr
||| Runtime: 693 bytes (22 mstore calls, last writes 11 unused bytes)
copyRuntimeBytecode : Integer -> IO ()
copyRuntimeBytecode offset = do
  -- Runtime bytecode from ERC7546Proxy.idr compilation (693 bytes)
  -- Split into 32-byte (256-bit) words for mstore
  mstore (offset+0)   0x608060405261000d5f61000f565b005b6100189061001b565b90565b905f9161
  mstore (offset+32)  0x008a6100838261002e81610223565b61006a826100438161003e61012e565b61
  mstore (offset+64)  0x0164565b926100628261005c8160e0610056610126565b90610171565b5f6102
  mstore (offset+96)  0x08565b506004610208565b506100748261023e565b905f6024905f9260209461
  mstore (offset+128) 0x0152565b5f906102a1565b8060011461011d575f1461009c575b50565b6100a6
  mstore (offset+160) 0x815f610216565b6100b05f826102a1565b80600114610114575f146100c45750
  mstore (offset+192) 0x610099565b816100f5916100d282610261565b906100e083835f5f9061027a56
  mstore (offset+224) 0x5b506100ea8361023e565b915f5f925f9461024f565b6100fe8261017f565b90
  mstore (offset+256) 0x61010c83835f5f9061018b565b505f9061019a565b825f5f9061024a565b505f
  mstore (offset+288) 0x5f9061024a565b63dc9cc64590565b7f267691be3525af8a813d30db0c9e2bad
  mstore (offset+320) 0x08f63baecf6dceb85e2cf3676cff56f490565b9061016196959493929161019f
  mstore (offset+352) 0x565b90565b9061016e916101aa565b90565b9061017c92916101b0565b90565b
  mstore (offset+384) 0x610188906101b9565b90565b906101979392916101be565b90565b6101c7565b
  mstore (offset+416) 0x90919293949550fa90565b90505490565b9091501b90565bfd5b503d90565b90
  mstore (offset+448) 0x9192503e5f90565b909192506001146101d457fd5bf35b909150525f90565b90
  mstore (offset+480) 0x505190565b505a90565b90919293949550f490565b503690565b90503590565b
  mstore (offset+512) 0x90919250375f90565b9061021392916101d6565b90565b90610220916101de56
  mstore (offset+544) 0x5b90565b61023061023b915f61026d565b600160e01b90610289565b90565b61
  mstore (offset+576) 0x0247906101e4565b90565b6101b7565b9061025e9695949392916101e9565b90
  mstore (offset+608) 0x565b61026a906101f4565b90565b90610277916101f9565b90565b9061028693
  mstore (offset+640) 0x92916101ff565b90565b6102935f836102a1565b5f1461029d575f80fd5b0490
  -- Last partial word (21 bytes + 11 unused zero-padding)
  mstore (offset+672) 0x565b145f146102ae576001905b565b5f906102ac56000000000000000000000000

-- =============================================================================
-- Init Code Builder
-- =============================================================================

||| Build init code that:
||| 1. Stores dictionary address in DICTIONARY_SLOT
||| 2. Returns runtime bytecode
|||
||| Init code layout (78 bytes before runtime):
||| [00-32]   PUSH32 <dictionary>       (33 bytes: 0x7f + 32 bytes)
||| [33-65]   PUSH32 <DICTIONARY_SLOT>  (33 bytes: 0x7f + 32 bytes)
||| [66]      SSTORE                    (1 byte: 0x55)
||| [67-69]   PUSH2 <runtime_size>      (3 bytes: 0x61 + 2 bytes = 0x6102b5)
||| [70]      DUP1                      (1 byte: 0x80)
||| [71-73]   PUSH2 <runtime_offset>    (3 bytes: 0x61 + 2 bytes = 0x61004e)
||| [74]      PUSH0                     (1 byte: 0x5f)
||| [75]      CODECOPY                  (1 byte: 0x39)
||| [76]      PUSH0                     (1 byte: 0x5f)
||| [77]      RETURN                    (1 byte: 0xf3)
||| [78+]     <runtime bytecode>        (693 bytes)
|||
||| Total init code size: 78 + 693 = 771 bytes
buildInitCode : Integer -> IO ()
buildInitCode dictionary = do
  -- Offset 0: PUSH32 dictionary (0x7f = PUSH32)
  mstore8 0 0x7f
  mstore 1 dictionary

  -- Offset 33: PUSH32 DICTIONARY_SLOT (0x7f = PUSH32)
  mstore8 33 0x7f
  mstore 34 DICTIONARY_SLOT

  -- Offset 66: SSTORE (0x55)
  mstore8 66 0x55

  -- Offset 67: PUSH2 runtime_size (693 = 0x02b5)
  mstore8 67 0x61
  mstore8 68 0x02
  mstore8 69 0xb5

  -- Offset 70: DUP1 (0x80)
  mstore8 70 0x80

  -- Offset 71: PUSH2 runtime_offset (78 = 0x004e)
  mstore8 71 0x61
  mstore8 72 0x00
  mstore8 73 0x4e

  -- Offset 74: PUSH0 (0x5f)
  mstore8 74 0x5f

  -- Offset 75: CODECOPY (0x39)
  mstore8 75 0x39

  -- Offset 76: PUSH0 (0x5f)
  mstore8 76 0x5f

  -- Offset 77: RETURN (0xf3)
  mstore8 77 0xf3

  -- Offset 78+: Runtime bytecode (693 bytes)
  copyRuntimeBytecode 78

-- =============================================================================
-- CREATE2 Deployment
-- =============================================================================

||| Deploy ERC-7546 Proxy with CREATE2
||| Returns the deterministic proxy address, or reverts on failure
export
deployProxy : Integer -> Integer -> IO Integer
deployProxy dictionary salt = do
  -- Build init code in memory starting at offset 0
  buildInitCode dictionary

  -- CREATE2(value, offset, size, salt)
  addr <- create2 0 0 INIT_CODE_SIZE salt

  if addr == 0
    then do
      revertConflict ExternalCallForbidden  -- CREATE2 failed (reverts, never returns)
      pure 0  -- Unreachable, but needed for type checking
    else do
      -- Emit event: ProxyDeployed(proxy, dictionary)
      mstore 0 addr
      log2 0 32 EVENT_PROXY_DEPLOYED dictionary
      pure addr

||| Compute the deterministic address for a proxy
||| address = keccak256(0xff ++ factory ++ salt ++ keccak256(initCode))[12:]
export
computeProxyAddress : Integer -> Integer -> Integer -> IO Integer
computeProxyAddress factory dictionary salt = do
  -- Build init code to get its hash
  buildInitCode dictionary
  initCodeHash <- keccak256 0 INIT_CODE_SIZE

  -- Build CREATE2 address computation data
  -- [0xff (1 byte)][factory (20 bytes)][salt (32 bytes)][initCodeHash (32 bytes)]
  mstore8 0 0xff
  mstore 1 factory  -- Will be masked to 20 bytes by address()
  mstore 21 salt
  mstore 53 initCodeHash

  -- Hash and take last 20 bytes
  hash <- keccak256 0 85
  pure (hash `mod` (2 `prim__shl_Integer` 160))  -- Extract address from hash

