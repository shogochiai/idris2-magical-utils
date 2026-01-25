||| MC Standard Helper Library: Proxy Creator
||| Idris2 port of metacontract/mc src/std/functions/internal/ProxyCreator.sol
|||
||| Creates new proxy contracts using CREATE opcode
module Main

import EVM.Primitives

||| Bitwise OR (alias for convenience)
bitor : Integer -> Integer -> IO Integer
bitor a b = or a b

-- =============================================================================
-- Event Signature
-- =============================================================================

||| Event: ProxyCreated(address indexed dictionary, address indexed proxy)
||| Signature: keccak256("ProxyCreated(address,address)")
||| = 0x9678a1e87ca9f1a37dc659a97b39d812d98cd236947e1b53b3d0d6fd346acb6e
EVENT_PROXY_CREATED : Integer
EVENT_PROXY_CREATED = 0x9678a1e87ca9f1a37dc659a97b39d812d98cd236947e1b53b3d0d6fd346acb6e

-- =============================================================================
-- Proxy Bytecode
-- =============================================================================

||| Minimal proxy bytecode (EIP-1167 clone pattern)
||| This bytecode delegates all calls to the dictionary address
|||
||| Runtime code:
|||   CALLDATASIZE PUSH0 PUSH0 CALLDATACOPY  // Copy calldata to memory
|||   PUSH0 CALLDATASIZE PUSH0 PUSH0         // Prepare delegatecall args
|||   PUSH20 <dictionary>                     // Dictionary address (placeholder)
|||   GAS DELEGATECALL                        // Forward call
|||   RETURNDATASIZE PUSH0 PUSH0 RETURNDATACOPY // Copy return data
|||   PUSH0 RETURNDATASIZE                    // Return data location
|||   SWAP2                                   // Check success
|||   PUSH1 0x2b JUMPI                        // Jump to return if success
|||   REVERT                                  // Revert on failure
|||   JUMPDEST RETURN                         // Return on success
|||
||| For simplicity, we use a hardcoded minimal proxy pattern
PROXY_BYTECODE_PREFIX : Integer
PROXY_BYTECODE_PREFIX = 0x363d3d373d3d3d363d73  -- EIP-1167 prefix (10 bytes)

PROXY_BYTECODE_SUFFIX : Integer
PROXY_BYTECODE_SUFFIX = 0x5af43d82803e903d91602b57fd5bf3  -- EIP-1167 suffix (15 bytes)

-- =============================================================================
-- Proxy Creation Logic
-- =============================================================================

||| Create a minimal proxy pointing to dictionary
||| Returns the address of the newly created proxy
|||
||| Uses EVM opcodes for bit manipulation to avoid large literals
||| EIP-1167 minimal proxy: 45 bytes total
createProxy : Integer -> IO Integer
createProxy dictionary = do
  -- EIP-1167 minimal proxy bytecode:
  -- 0x363d3d373d3d3d363d73 <address> 5af43d82803e903d91602b57fd5bf3
  --
  -- Build in memory using shl/or opcodes

  -- First 32 bytes: prefix (10 bytes) + dictionary address (20 bytes) + 2 bytes of suffix
  let prefix10 = 0x363d3d373d3d3d363d73  -- 10 bytes
  let suffix2 = 0x5af4  -- 2 bytes

  -- Shift prefix left 176 bits (22 bytes)
  prefixShifted <- shl 176 prefix10
  -- Shift dictionary left 16 bits (2 bytes)
  dictShifted <- shl 16 dictionary
  -- Combine: prefix | dict | suffix2
  tmp1 <- bitor prefixShifted dictShifted
  word0 <- bitor tmp1 suffix2
  mstore 0 word0

  -- Remaining 13 bytes of suffix at offset 32
  -- 0x3d82803e903d91602b57fd5bf3 (13 bytes)
  let suffix13 = 0x3d82803e903d91602b57fd5bf3
  -- Shift left by 152 bits (19 bytes) to left-align in 32-byte word
  word1 <- shl 152 suffix13
  mstore 32 word1

  -- Create the proxy contract with 0 ETH value
  -- Total bytecode size: 45 bytes
  proxyAddr <- create 0 0 45

  pure proxyAddr

||| Emit ProxyCreated event
emitProxyCreated : Integer -> Integer -> IO ()
emitProxyCreated dictionary proxy = do
  -- No data (both args are indexed)
  -- log2 with empty data
  log2 0 0 EVENT_PROXY_CREATED dictionary

-- =============================================================================
-- Function Selectors
-- =============================================================================

-- create(address dictionary, bytes initData) -> address proxy
-- selector: 0xbeab7131 (example, would need actual keccak256)
SEL_CREATE : Integer
SEL_CREATE = 0xbeab7131

-- =============================================================================
-- Entry Point
-- =============================================================================

getSelector : IO Integer
getSelector = do
  data_ <- calldataload 0
  pure (data_ `div` (256 * 256 * 256 * 256 * 256 * 256 * 256 * 256 *
                     256 * 256 * 256 * 256 * 256 * 256 * 256 * 256 *
                     256 * 256 * 256 * 256 * 256 * 256 * 256 * 256 *
                     256 * 256 * 256 * 256))

returnAddress : Integer -> IO ()
returnAddress addr = do
  mstore 0 addr
  evmReturn 0 32

main : IO ()
main = do
  selector <- getSelector
  if selector == SEL_CREATE
    then do
      -- Read dictionary address from calldata (offset 4)
      dictionary <- calldataload 4
      -- For simplicity, ignore initData for now

      -- Create the proxy
      proxyAddr <- createProxy dictionary

      -- Check if creation succeeded (address != 0)
      if proxyAddr == 0
        then evmRevert 0 0
        else do
          emitProxyCreated dictionary proxyAddr
          returnAddress proxyAddr
    else evmRevert 0 0
