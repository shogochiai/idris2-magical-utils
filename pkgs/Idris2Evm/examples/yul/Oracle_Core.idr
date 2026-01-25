||| Oracle Core - Decentralized Oracle for Market Resolution (standalone for idris2-yul)
||| Stake-weighted outcome determination
module Main

import EVM.Primitives

-- =============================================================================
-- Type Aliases
-- =============================================================================

Address : Type
Address = Integer

MarketId : Type
MarketId = Integer

Uint256 : Type
Uint256 = Integer

-- =============================================================================
-- Storage Layout
-- =============================================================================

SLOT_ORACLE : Integer
SLOT_ORACLE = 0x56789012eef012356789012e4ef012356789012e4ef012356789012e4ef01234

ORACLE_NEXT_REQUEST_ID : Integer
ORACLE_NEXT_REQUEST_ID = 0

ORACLE_REQ_REQUESTER : Integer
ORACLE_REQ_REQUESTER = 0

ORACLE_REQ_MARKET_ID : Integer
ORACLE_REQ_MARKET_ID = 1

ORACLE_REQ_RESOLVED : Integer
ORACLE_REQ_RESOLVED = 4

ORACLE_REQ_OUTCOME : Integer
ORACLE_REQ_OUTCOME = 5

ORACLE_REQ_STAKE_FOR : Integer
ORACLE_REQ_STAKE_FOR = 6

ORACLE_REQ_STAKE_AGAINST : Integer
ORACLE_REQ_STAKE_AGAINST = 7

-- =============================================================================
-- Function Selectors
-- =============================================================================

SEL_GET_NEXT_REQUEST_ID : Integer
SEL_GET_NEXT_REQUEST_ID = 0x62340001

SEL_CREATE_ORACLE_REQUEST : Integer
SEL_CREATE_ORACLE_REQUEST = 0x62340002

SEL_IS_RESOLVED : Integer
SEL_IS_RESOLVED = 0x62340003

SEL_GET_OUTCOME : Integer
SEL_GET_OUTCOME = 0x62340004

SEL_GET_STAKE_FOR : Integer
SEL_GET_STAKE_FOR = 0x62340005

SEL_GET_STAKE_AGAINST : Integer
SEL_GET_STAKE_AGAINST = 0x62340006

-- =============================================================================
-- Slot Calculation
-- =============================================================================

slotHash : Integer -> Integer -> IO Integer
slotHash key baseSlot = do
  mstore 0 key
  mstore 32 baseSlot
  keccak256 0 64

getOracleRequestSlot : Integer -> IO Integer
getOracleRequestSlot reqId = slotHash reqId (SLOT_ORACLE + 100)

-- =============================================================================
-- Entry Point Helpers
-- =============================================================================

-- =============================================================================
-- Storage Access
-- =============================================================================

getNextRequestId : IO Integer
getNextRequestId = sload (SLOT_ORACLE + ORACLE_NEXT_REQUEST_ID)

setNextRequestId : Integer -> IO ()
setNextRequestId rid = sstore (SLOT_ORACLE + ORACLE_NEXT_REQUEST_ID) rid

-- =============================================================================
-- Request Storage Access
-- =============================================================================

isRequestResolved : Integer -> IO Bool
isRequestResolved rid = do
  slot <- getOracleRequestSlot rid
  resolved <- sload (slot + ORACLE_REQ_RESOLVED)
  pure (resolved /= 0)

getRequestOutcome : Integer -> IO Bool
getRequestOutcome rid = do
  slot <- getOracleRequestSlot rid
  outcome <- sload (slot + ORACLE_REQ_OUTCOME)
  pure (outcome /= 0)

getStakeFor : Integer -> IO Uint256
getStakeFor rid = do
  slot <- getOracleRequestSlot rid
  sload (slot + ORACLE_REQ_STAKE_FOR)

getStakeAgainst : Integer -> IO Uint256
getStakeAgainst rid = do
  slot <- getOracleRequestSlot rid
  sload (slot + ORACLE_REQ_STAKE_AGAINST)

-- =============================================================================
-- Core Functions
-- =============================================================================

createOracleRequest : MarketId -> IO Integer
createOracleRequest mid = do
  rid <- getNextRequestId
  requester <- caller
  now <- timestamp

  slot <- getOracleRequestSlot rid
  sstore (slot + ORACLE_REQ_REQUESTER) requester
  sstore (slot + ORACLE_REQ_MARKET_ID) mid
  sstore (slot + ORACLE_REQ_RESOLVED) 0
  sstore (slot + ORACLE_REQ_OUTCOME) 0
  sstore (slot + ORACLE_REQ_STAKE_FOR) 0
  sstore (slot + ORACLE_REQ_STAKE_AGAINST) 0

  setNextRequestId (rid + 1)
  pure rid

-- =============================================================================
-- Main Entry Point
-- =============================================================================

main : IO ()
main = do
  selector <- getSelector

  if selector == SEL_GET_NEXT_REQUEST_ID
    then do
      rid <- getNextRequestId
      returnUint rid
    else if selector == SEL_CREATE_ORACLE_REQUEST
    then do
      mid <- calldataload 4
      rid <- createOracleRequest mid
      returnUint rid
    else if selector == SEL_IS_RESOLVED
    then do
      rid <- calldataload 4
      resolved <- isRequestResolved rid
      returnBool resolved
    else if selector == SEL_GET_OUTCOME
    then do
      rid <- calldataload 4
      outcome <- getRequestOutcome rid
      returnBool outcome
    else if selector == SEL_GET_STAKE_FOR
    then do
      rid <- calldataload 4
      stake <- getStakeFor rid
      returnUint stake
    else if selector == SEL_GET_STAKE_AGAINST
    then do
      rid <- calldataload 4
      stake <- getStakeAgainst rid
      returnUint stake
    else evmRevert 0 0
