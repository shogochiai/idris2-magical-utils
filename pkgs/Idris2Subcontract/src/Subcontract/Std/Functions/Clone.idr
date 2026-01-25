||| Subcontract Standard Function: Clone
|||
||| Creates new proxy contracts pointing to the same dictionary.
||| Uses EIP-1167 minimal proxy pattern.
module Subcontract.Std.Functions.Clone

import EVM.Primitives
import Subcontract.Standards.ERC7546.Forward

-- =============================================================================
-- Event Signatures
-- =============================================================================

||| ProxyCreated(address indexed dictionary, address indexed proxy)
export
EVENT_PROXY_CREATED : Integer
EVENT_PROXY_CREATED = 0x9678a1e87ca9f1a37dc659a97b39d812d98cd236947e1b53b3d0d6fd346acb6e

-- =============================================================================
-- EIP-1167 Minimal Proxy Bytecode
-- =============================================================================

||| Create EIP-1167 minimal proxy pointing to dictionary
||| Uses the same bytecode as ProxyCreator
export
createMinimalProxy : Integer -> IO Integer
createMinimalProxy dictionary = do
  -- EIP-1167 minimal proxy bytecode:
  -- 0x363d3d373d3d3d363d73 <address> 5af43d82803e903d91602b57fd5bf3
  --
  -- Build in memory using shl/or opcodes
  let prefix10 = 0x363d3d373d3d3d363d73  -- 10 bytes
  let suffix2 = 0x5af4  -- 2 bytes

  -- Shift prefix left 176 bits (22 bytes)
  prefixShifted <- shl 176 prefix10
  -- Shift dictionary left 16 bits (2 bytes)
  dictShifted <- shl 16 dictionary
  -- Combine: prefix | dict | suffix2
  tmp1 <- or prefixShifted dictShifted
  word0 <- or tmp1 suffix2
  mstore 0 word0

  -- Remaining 13 bytes of suffix at offset 32
  -- 0x3d82803e903d91602b57fd5bf3 (13 bytes)
  let suffix13 = 0x3d82803e903d91602b57fd5bf3
  -- Shift left by 152 bits (19 bytes) to left-align
  word1 <- shl 152 suffix13
  mstore 32 word1

  -- Create the proxy contract with 0 ETH value
  -- Total bytecode size: 45 bytes
  create 0 0 45

-- =============================================================================
-- Clone Function
-- =============================================================================

||| Clone the current contract (create new proxy with same dictionary)
export
clone : IO Integer
clone = do
  dictionary <- getDictionary
  proxyAddr <- createMinimalProxy dictionary
  -- Emit event
  log2 0 0 EVENT_PROXY_CREATED dictionary
  pure proxyAddr

||| Clone with custom dictionary
export
cloneWithDictionary : Integer -> IO Integer
cloneWithDictionary dictionary = do
  proxyAddr <- createMinimalProxy dictionary
  log2 0 0 EVENT_PROXY_CREATED dictionary
  pure proxyAddr
