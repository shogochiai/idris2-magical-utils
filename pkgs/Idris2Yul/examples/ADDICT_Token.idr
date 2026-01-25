||| ADDICT Token - ERC20 Implementation (standalone for idris2-yul)
||| Governance token with emission schedule
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
-- Constants
-- =============================================================================

E18 : Integer
E18 = 1000000000000000000

INITIAL_EMISSION_RATE : Integer
INITIAL_EMISSION_RATE = 10000000 * E18

HALVING_PERIOD : Integer
HALVING_PERIOD = 126144000  -- 4 years

-- =============================================================================
-- Storage Layout
-- =============================================================================

SLOT_ADDICT : Integer
SLOT_ADDICT = 0x2345678901bcdef02345678901bcdef02345678901bcdef02345678901bcdef0

ADDICT_TOTAL_SUPPLY : Integer
ADDICT_TOTAL_SUPPLY = 0

ADDICT_EMISSION_START_TIME : Integer
ADDICT_EMISSION_START_TIME = 1

ADDICT_LAST_MINT_TIME : Integer
ADDICT_LAST_MINT_TIME = 2

-- =============================================================================
-- Function Selectors
-- =============================================================================

SEL_TOTAL_SUPPLY : Integer
SEL_TOTAL_SUPPLY = 0x18160ddd

SEL_BALANCE_OF : Integer
SEL_BALANCE_OF = 0x70a08231

SEL_TRANSFER : Integer
SEL_TRANSFER = 0xa9059cbb

SEL_APPROVE : Integer
SEL_APPROVE = 0x095ea7b3

SEL_INITIALIZE : Integer
SEL_INITIALIZE = 0x52340001

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
-- Storage Access
-- =============================================================================

getTotalSupply : IO Uint256
getTotalSupply = sload (SLOT_ADDICT + ADDICT_TOTAL_SUPPLY)

setTotalSupply : Uint256 -> IO ()
setTotalSupply supply = sstore (SLOT_ADDICT + ADDICT_TOTAL_SUPPLY) supply

getEmissionStartTime : IO Integer
getEmissionStartTime = sload (SLOT_ADDICT + ADDICT_EMISSION_START_TIME)

setEmissionStartTime : Integer -> IO ()
setEmissionStartTime t = sstore (SLOT_ADDICT + ADDICT_EMISSION_START_TIME) t

setLastMintTime : Integer -> IO ()
setLastMintTime t = sstore (SLOT_ADDICT + ADDICT_LAST_MINT_TIME) t

-- =============================================================================
-- Balance Access
-- =============================================================================

getBalanceOf : Address -> IO Uint256
getBalanceOf addr = do
  slot <- getBalanceSlot addr (SLOT_ADDICT + 500)
  sload slot

setBalanceOf : Address -> Uint256 -> IO ()
setBalanceOf addr bal = do
  slot <- getBalanceSlot addr (SLOT_ADDICT + 500)
  sstore slot bal

-- =============================================================================
-- Core Functions
-- =============================================================================

transfer : Address -> Uint256 -> IO Bool
transfer to amount = do
  from <- caller
  fromBal <- getBalanceOf from
  if fromBal < amount
    then pure False
    else do
      setBalanceOf from (fromBal - amount)
      toBal <- getBalanceOf to
      setBalanceOf to (toBal + amount)
      pure True

initializeADDICT : IO ()
initializeADDICT = do
  now <- timestamp
  setEmissionStartTime now
  setLastMintTime now
  setTotalSupply 0

-- =============================================================================
-- Main Entry Point
-- =============================================================================

main : IO ()
main = do
  selector <- getSelector

  if selector == SEL_TOTAL_SUPPLY
    then do
      supply <- getTotalSupply
      returnUint supply
    else if selector == SEL_BALANCE_OF
    then do
      addr <- calldataload 4
      bal <- getBalanceOf addr
      returnUint bal
    else if selector == SEL_TRANSFER
    then do
      to <- calldataload 4
      amount <- calldataload 36
      success <- transfer to amount
      returnBool success
    else if selector == SEL_INITIALIZE
    then do
      initializeADDICT
      evmReturn 0 0
    else evmRevert 0 0
