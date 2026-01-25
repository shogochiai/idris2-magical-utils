||| IdeoCoin Core - Ideology Token Implementation
||| ERC20 token backed by PPM position baskets (standalone version for idris2-yul)
module Main

import EVM.Primitives

-- =============================================================================
-- Type Aliases
-- =============================================================================

Address : Type
Address = Integer

Uint256 : Type
Uint256 = Integer

-- =============================================================================
-- Storage Layout (ERC-7201 namespaced)
-- =============================================================================

SLOT_IDEOCOIN : Integer
SLOT_IDEOCOIN = 0x3456789012cdef013456789012cdef013456789012cdef013456789012cdef01

IDEOCOIN_TOTAL_SUPPLY : Integer
IDEOCOIN_TOTAL_SUPPLY = 0

IDEOCOIN_RESERVE_BALANCE : Integer
IDEOCOIN_RESERVE_BALANCE = 1

IDEOCOIN_CREATOR : Integer
IDEOCOIN_CREATOR = 2

IDEOCOIN_COUNCIL_SIZE : Integer
IDEOCOIN_COUNCIL_SIZE = 3

IDEOCOIN_PPM_COUNT : Integer
IDEOCOIN_PPM_COUNT = 4

-- =============================================================================
-- Function Selectors
-- =============================================================================

SEL_IDEO_TRANSFER : Integer
SEL_IDEO_TRANSFER = 0x42340001

SEL_IDEO_BALANCE_OF : Integer
SEL_IDEO_BALANCE_OF = 0x42340003

SEL_IDEO_TOTAL_SUPPLY : Integer
SEL_IDEO_TOTAL_SUPPLY = 0x42340006

SEL_IDEO_INITIALIZE : Integer
SEL_IDEO_INITIALIZE = 0x42340004

-- =============================================================================
-- Slot Calculation
-- =============================================================================

slotHash : Integer -> Integer -> IO Integer
slotHash key baseSlot = do
  mstore 0 key
  mstore 32 baseSlot
  keccak256 0 64

getBalanceSlot : Address -> Integer -> IO Integer
getBalanceSlot addr baseSlot = slotHash addr baseSlot

-- =============================================================================
-- Entry Point Helpers
-- =============================================================================

-- =============================================================================
-- Storage Access
-- =============================================================================

getIdeoCoinTotalSupply : IO Uint256
getIdeoCoinTotalSupply = sload (SLOT_IDEOCOIN + IDEOCOIN_TOTAL_SUPPLY)

setIdeoCoinTotalSupply : Uint256 -> IO ()
setIdeoCoinTotalSupply supply = sstore (SLOT_IDEOCOIN + IDEOCOIN_TOTAL_SUPPLY) supply

getIdeologyCreator : IO Address
getIdeologyCreator = sload (SLOT_IDEOCOIN + IDEOCOIN_CREATOR)

setIdeologyCreator : Address -> IO ()
setIdeologyCreator creator = sstore (SLOT_IDEOCOIN + IDEOCOIN_CREATOR) creator

setReserveBalance : Uint256 -> IO ()
setReserveBalance bal = sstore (SLOT_IDEOCOIN + IDEOCOIN_RESERVE_BALANCE) bal

setCouncilSize : Integer -> IO ()
setCouncilSize size = sstore (SLOT_IDEOCOIN + IDEOCOIN_COUNCIL_SIZE) size

setPPMCount : Integer -> IO ()
setPPMCount count = sstore (SLOT_IDEOCOIN + IDEOCOIN_PPM_COUNT) count

-- =============================================================================
-- ERC20 Balance/Allowance Storage
-- =============================================================================

getIdeoCoinBalance : Address -> IO Uint256
getIdeoCoinBalance addr = do
  slot <- getBalanceSlot addr (SLOT_IDEOCOIN + 100)
  sload slot

setIdeoCoinBalance : Address -> Uint256 -> IO ()
setIdeoCoinBalance addr bal = do
  slot <- getBalanceSlot addr (SLOT_IDEOCOIN + 100)
  sstore slot bal

-- =============================================================================
-- ERC20 Core Functions
-- =============================================================================

ideoCoinTransfer : Address -> Uint256 -> IO Bool
ideoCoinTransfer to amount = do
  from <- caller
  fromBal <- getIdeoCoinBalance from
  if fromBal < amount
    then pure False
    else do
      setIdeoCoinBalance from (fromBal - amount)
      toBal <- getIdeoCoinBalance to
      setIdeoCoinBalance to (toBal + amount)
      pure True

-- =============================================================================
-- Initialization
-- =============================================================================

initializeIdeoCoin : Address -> IO ()
initializeIdeoCoin creator = do
  setIdeologyCreator creator
  setIdeoCoinTotalSupply 0
  setReserveBalance 0
  setCouncilSize 0
  setPPMCount 0

-- =============================================================================
-- Main Entry Point
-- =============================================================================

main : IO ()
main = do
  selector <- getSelector

  if selector == SEL_IDEO_TOTAL_SUPPLY
    then do
      supply <- getIdeoCoinTotalSupply
      returnUint supply
    else if selector == SEL_IDEO_BALANCE_OF
    then do
      addr <- calldataload 4
      bal <- getIdeoCoinBalance addr
      returnUint bal
    else if selector == SEL_IDEO_TRANSFER
    then do
      to <- calldataload 4
      amount <- calldataload 36
      success <- ideoCoinTransfer to amount
      returnBool success
    else if selector == SEL_IDEO_INITIALIZE
    then do
      creator <- calldataload 4
      initializeIdeoCoin creator
      evmReturn 0 0
    else evmRevert 0 0
