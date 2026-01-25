||| MC Standard Function: Receive
||| Idris2 port of metacontract/mc src/std/functions/Receive.sol
|||
||| Emits Received event when ETH is sent to the contract
module Main

import EVM.Primitives

-- =============================================================================
-- Event Signature
-- =============================================================================

||| Event: Received(address indexed from, uint256 amount)
||| Signature: keccak256("Received(address,uint256)")
||| = 0x88a5966d370b9919b20f3e2c13ff65706f196a4e32cc2c12bf57088f88525874
EVENT_RECEIVED : Integer
EVENT_RECEIVED = 0x88a5966d370b9919b20f3e2c13ff65706f196a4e32cc2c12bf57088f88525874

-- =============================================================================
-- Event Emission
-- =============================================================================

||| Emit Received event
||| Topics: [EVENT_RECEIVED, from (indexed)]
||| Data: amount (non-indexed, stored in memory)
emitReceived : Integer -> Integer -> IO ()
emitReceived from amount = do
  -- Store amount in memory at offset 0
  mstore 0 amount
  -- log2(offset, size, topic1, topic2)
  -- topic1 = event signature
  -- topic2 = indexed 'from' address
  log2 0 32 EVENT_RECEIVED from

-- =============================================================================
-- Receive Function (fallback for ETH transfers)
-- =============================================================================

||| Handle incoming ETH transfers
||| Equivalent to Solidity's receive() external payable
receive : IO ()
receive = do
  from <- caller
  amount <- callvalue
  emitReceived from amount

-- =============================================================================
-- Entry Point
-- =============================================================================

||| Main dispatcher
||| If calldata is empty, treat as receive()
main : IO ()
main = do
  size <- calldatasize
  if size == 0
    then do
      receive
      stop
    else stop  -- Unknown function, just stop
