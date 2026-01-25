||| PerpPredictionMarketFactory Core Functions
||| Market creation and management (standalone version for idris2-yul)
module Main

import EVM.Primitives

-- =============================================================================
-- Type Aliases
-- =============================================================================

Address : Type
Address = Integer

TokenId : Type
TokenId = Integer

MarketId : Type
MarketId = Integer

Uint256 : Type
Uint256 = Integer

Int256 : Type
Int256 = Integer

-- =============================================================================
-- Enums
-- =============================================================================

data MarketState = Active | Settled

marketStateToInt : MarketState -> Integer
marketStateToInt Active = 0
marketStateToInt Settled = 1

intToMarketState : Integer -> MarketState
intToMarketState 1 = Settled
intToMarketState _ = Active

-- =============================================================================
-- Constants
-- =============================================================================

E18 : Integer
E18 = 1000000000000000000

DEFAULT_FUNDING_PERIOD : Integer
DEFAULT_FUNDING_PERIOD = 28800

-- =============================================================================
-- Storage Layout (ERC-7201 namespaced)
-- =============================================================================

SLOT_PPM : Integer
SLOT_PPM = 0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef

PPM_NEXT_MARKET_ID : Integer
PPM_NEXT_MARKET_ID = 0

PPM_NEXT_TOKEN_ID : Integer
PPM_NEXT_TOKEN_ID = 1

PPM_ADMIN : Integer
PPM_ADMIN = 2

PPM_FUNDING_PERIOD : Integer
PPM_FUNDING_PERIOD = 4

PPM_FUNDING_FACTOR : Integer
PPM_FUNDING_FACTOR = 5

MARKET_ORACLE : Integer
MARKET_ORACLE = 1

MARKET_COLLATERAL_TOKEN : Integer
MARKET_COLLATERAL_TOKEN = 2

MARKET_STATE : Integer
MARKET_STATE = 3

MARKET_TOTAL_LONG_SIZE : Integer
MARKET_TOTAL_LONG_SIZE = 8

MARKET_TOTAL_SHORT_SIZE : Integer
MARKET_TOTAL_SHORT_SIZE = 9

-- =============================================================================
-- Function Selectors
-- =============================================================================

||| createMarket(address,address) -> 0x12340001
SEL_CREATE_MARKET : Integer
SEL_CREATE_MARKET = 0x12340001

||| getMarketPrice(uint256) -> 0x12340002
SEL_GET_MARKET_PRICE : Integer
SEL_GET_MARKET_PRICE = 0x12340002

||| isMarketActive(uint256) -> 0x12340003
SEL_IS_MARKET_ACTIVE : Integer
SEL_IS_MARKET_ACTIVE = 0x12340003

||| getNextMarketId() -> 0x12340005
SEL_GET_NEXT_MARKET_ID : Integer
SEL_GET_NEXT_MARKET_ID = 0x12340005

-- =============================================================================
-- Slot Calculation
-- =============================================================================

slotHash : Integer -> Integer -> IO Integer
slotHash key baseSlot = do
  mstore 0 key
  mstore 32 baseSlot
  keccak256 0 64

getMarketSlot : MarketId -> IO Integer
getMarketSlot mid = slotHash mid (SLOT_PPM + 100)

-- =============================================================================
-- Entry Point Helpers
-- =============================================================================

-- =============================================================================
-- Storage Access
-- =============================================================================

getNextMarketId : IO MarketId
getNextMarketId = sload (SLOT_PPM + PPM_NEXT_MARKET_ID)

setNextMarketId : MarketId -> IO ()
setNextMarketId mid = sstore (SLOT_PPM + PPM_NEXT_MARKET_ID) mid

getAdmin : IO Address
getAdmin = sload (SLOT_PPM + PPM_ADMIN)

setAdmin : Address -> IO ()
setAdmin admin = sstore (SLOT_PPM + PPM_ADMIN) admin

setNextTokenId : TokenId -> IO ()
setNextTokenId tid = sstore (SLOT_PPM + PPM_NEXT_TOKEN_ID) tid

setFundingPeriod : Integer -> IO ()
setFundingPeriod period = sstore (SLOT_PPM + PPM_FUNDING_PERIOD) period

setFundingFactor : Integer -> IO ()
setFundingFactor factor = sstore (SLOT_PPM + PPM_FUNDING_FACTOR) factor

-- =============================================================================
-- Market Field Access
-- =============================================================================

getMarketState : MarketId -> IO MarketState
getMarketState mid = do
  slot <- getMarketSlot mid
  stateInt <- sload (slot + MARKET_STATE)
  pure $ intToMarketState stateInt

getTotalLongSize : MarketId -> IO Uint256
getTotalLongSize mid = do
  slot <- getMarketSlot mid
  sload (slot + MARKET_TOTAL_LONG_SIZE)

getTotalShortSize : MarketId -> IO Uint256
getTotalShortSize mid = do
  slot <- getMarketSlot mid
  sload (slot + MARKET_TOTAL_SHORT_SIZE)

-- =============================================================================
-- Market Functions
-- =============================================================================

isMarketActive : MarketId -> IO Bool
isMarketActive mid = do
  state <- getMarketState mid
  pure $ case state of
    Active => True
    Settled => False

getMarketPrice : MarketId -> IO Uint256
getMarketPrice mid = do
  longSize <- getTotalLongSize mid
  shortSize <- getTotalShortSize mid
  let totalSize = longSize + shortSize
  if totalSize == 0
    then pure (E18 `div` 2)
    else pure $ (longSize * E18) `div` totalSize

createMarket : Address -> Address -> IO MarketId
createMarket oracle collateralToken = do
  mid <- getNextMarketId
  now <- timestamp
  slot <- getMarketSlot mid

  sstore (slot + MARKET_ORACLE) oracle
  sstore (slot + MARKET_COLLATERAL_TOKEN) collateralToken
  sstore (slot + MARKET_STATE) (marketStateToInt Active)

  setNextMarketId (mid + 1)
  pure mid

-- =============================================================================
-- Main Entry Point
-- =============================================================================

main : IO ()
main = do
  selector <- getSelector

  if selector == SEL_GET_NEXT_MARKET_ID
    then do
      mid <- getNextMarketId
      returnUint mid
    else if selector == SEL_GET_MARKET_PRICE
    then do
      mid <- calldataload 4
      price <- getMarketPrice mid
      returnUint price
    else if selector == SEL_IS_MARKET_ACTIVE
    then do
      mid <- calldataload 4
      active <- isMarketActive mid
      returnBool active
    else if selector == SEL_CREATE_MARKET
    then do
      oracle <- calldataload 4
      collateralToken <- calldataload 36
      mid <- createMarket oracle collateralToken
      returnUint mid
    else evmRevert 0 0
