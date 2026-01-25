||| PraddictFun Storage Schema
||| ERC-7201 namespaced storage slots for prediction market platform
module PraddictFun.Storages.Schema

import public EVM.Primitives

%default total

-- =============================================================================
-- Type Aliases
-- =============================================================================

public export
Address : Type
Address = Integer

public export
TokenId : Type
TokenId = Integer

public export
MarketId : Type
MarketId = Integer

public export
LenderId : Type
LenderId = Integer

public export
Uint256 : Type
Uint256 = Integer

public export
Int256 : Type
Int256 = Integer

-- =============================================================================
-- Enums
-- =============================================================================

||| Market state enum
public export
data MarketState = Active | Settled

public export
Eq MarketState where
  Active == Active = True
  Settled == Settled = True
  _ == _ = False

public export
marketStateToInt : MarketState -> Integer
marketStateToInt Active = 0
marketStateToInt Settled = 1

public export
intToMarketState : Integer -> MarketState
intToMarketState 1 = Settled
intToMarketState _ = Active

||| Position direction enum
public export
data PositionDirection = Long | Short

public export
Eq PositionDirection where
  Long == Long = True
  Short == Short = True
  _ == _ = False

public export
directionToInt : PositionDirection -> Integer
directionToInt Long = 0
directionToInt Short = 1

public export
intToDirection : Integer -> PositionDirection
intToDirection 1 = Short
intToDirection _ = Long

-- =============================================================================
-- ERC-7201 Namespace Slots
-- =============================================================================

||| PerpPredictionMarketFactory namespace
||| keccak256("praddictfun.storage.ppm") - 1
export
SLOT_PPM : Integer
SLOT_PPM = 0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef

||| ADDICT token namespace
||| keccak256("praddictfun.storage.addict") - 1
export
SLOT_ADDICT : Integer
SLOT_ADDICT = 0x2345678901bcdef02345678901bcdef02345678901bcdef02345678901bcdef0

||| IdeoCoin namespace
||| keccak256("praddictfun.storage.ideocoin") - 1
export
SLOT_IDEOCOIN : Integer
SLOT_IDEOCOIN = 0x3456789012cdef013456789012cdef013456789012cdef013456789012cdef01

||| Lender namespace
||| keccak256("praddictfun.storage.lender") - 1
export
SLOT_LENDER : Integer
SLOT_LENDER = 0x456789012ddef0124567890123def0124567890123def0124567890123def012

||| Oracle namespace
||| keccak256("praddictfun.storage.oracle") - 1
export
SLOT_ORACLE : Integer
SLOT_ORACLE = 0x56789012eef012356789012e4ef012356789012e4ef012356789012e4ef01234

-- =============================================================================
-- PPM Storage Layout
-- =============================================================================

-- PPM Global slots (relative to SLOT_PPM)
export PPM_NEXT_MARKET_ID : Integer
PPM_NEXT_MARKET_ID = 0

export PPM_NEXT_TOKEN_ID : Integer
PPM_NEXT_TOKEN_ID = 1

export PPM_ADMIN : Integer
PPM_ADMIN = 2

export PPM_LENDER : Integer
PPM_LENDER = 3

export PPM_FUNDING_PERIOD : Integer
PPM_FUNDING_PERIOD = 4

export PPM_FUNDING_FACTOR : Integer
PPM_FUNDING_FACTOR = 5

-- Market struct offsets (8 fields)
export MARKET_DESCRIPTION : Integer
MARKET_DESCRIPTION = 0

export MARKET_ORACLE : Integer
MARKET_ORACLE = 1

export MARKET_COLLATERAL_TOKEN : Integer
MARKET_COLLATERAL_TOKEN = 2

export MARKET_STATE : Integer
MARKET_STATE = 3

export MARKET_SETTLED_OUTCOME : Integer
MARKET_SETTLED_OUTCOME = 4

export MARKET_SETTLEMENT_TIMESTAMP : Integer
MARKET_SETTLEMENT_TIMESTAMP = 5

export MARKET_LAST_FUNDING_TIMESTAMP : Integer
MARKET_LAST_FUNDING_TIMESTAMP = 6

export MARKET_CUMULATIVE_FUNDING_RATE : Integer
MARKET_CUMULATIVE_FUNDING_RATE = 7

export MARKET_TOTAL_LONG_SIZE : Integer
MARKET_TOTAL_LONG_SIZE = 8

export MARKET_TOTAL_SHORT_SIZE : Integer
MARKET_TOTAL_SHORT_SIZE = 9

export MARKET_STRUCT_SIZE : Integer
MARKET_STRUCT_SIZE = 10

-- Position struct offsets (7 fields)
export POSITION_MARKET_ID : Integer
POSITION_MARKET_ID = 0

export POSITION_DIRECTION : Integer
POSITION_DIRECTION = 1

export POSITION_SIZE : Integer
POSITION_SIZE = 2

export POSITION_COLLATERAL : Integer
POSITION_COLLATERAL = 3

export POSITION_ENTRY_PRICE : Integer
POSITION_ENTRY_PRICE = 4

export POSITION_LAST_CUMULATIVE_FUNDING : Integer
POSITION_LAST_CUMULATIVE_FUNDING = 5

export POSITION_STRUCT_SIZE : Integer
POSITION_STRUCT_SIZE = 6

-- =============================================================================
-- ADDICT Storage Layout
-- =============================================================================

-- ADDICT Global slots
export ADDICT_TOTAL_SUPPLY : Integer
ADDICT_TOTAL_SUPPLY = 0

export ADDICT_EMISSION_START_TIME : Integer
ADDICT_EMISSION_START_TIME = 1

export ADDICT_LAST_MINT_TIME : Integer
ADDICT_LAST_MINT_TIME = 2

export ADDICT_TOTAL_LOCKED : Integer
ADDICT_TOTAL_LOCKED = 3

export ADDICT_TOTAL_WEIGHT : Integer
ADDICT_TOTAL_WEIGHT = 4

export ADDICT_GAUGE_COUNT : Integer
ADDICT_GAUGE_COUNT = 5

-- Lock struct offsets
export LOCK_AMOUNT : Integer
LOCK_AMOUNT = 0

export LOCK_END : Integer
LOCK_END = 1

export LOCK_STRUCT_SIZE : Integer
LOCK_STRUCT_SIZE = 2

-- Gauge struct offsets
export GAUGE_LP_TOKEN : Integer
GAUGE_LP_TOKEN = 0

export GAUGE_WEIGHT : Integer
GAUGE_WEIGHT = 1

export GAUGE_TOTAL_VOTES : Integer
GAUGE_TOTAL_VOTES = 2

export GAUGE_LAST_REWARD_TIME : Integer
GAUGE_LAST_REWARD_TIME = 3

export GAUGE_REWARD_PER_TOKEN : Integer
GAUGE_REWARD_PER_TOKEN = 4

export GAUGE_STRUCT_SIZE : Integer
GAUGE_STRUCT_SIZE = 5

-- =============================================================================
-- IdeoCoin Storage Layout
-- =============================================================================

export IDEOCOIN_TOTAL_SUPPLY : Integer
IDEOCOIN_TOTAL_SUPPLY = 0

export IDEOCOIN_RESERVE_BALANCE : Integer
IDEOCOIN_RESERVE_BALANCE = 1

export IDEOCOIN_CREATOR : Integer
IDEOCOIN_CREATOR = 2

export IDEOCOIN_COUNCIL_SIZE : Integer
IDEOCOIN_COUNCIL_SIZE = 3

export IDEOCOIN_PPM_COUNT : Integer
IDEOCOIN_PPM_COUNT = 4

export IDEOCOIN_TOTAL_STAKED : Integer
IDEOCOIN_TOTAL_STAKED = 5

export IDEOCOIN_ACC_REWARD_PER_SHARE : Integer
IDEOCOIN_ACC_REWARD_PER_SHARE = 6

-- PPMWeight struct
export PPM_WEIGHT_MARKET_ID : Integer
PPM_WEIGHT_MARKET_ID = 0

export PPM_WEIGHT_WEIGHT : Integer
PPM_WEIGHT_WEIGHT = 1

export PPM_WEIGHT_STRUCT_SIZE : Integer
PPM_WEIGHT_STRUCT_SIZE = 2

-- =============================================================================
-- Lender Storage Layout
-- =============================================================================

export LENDER_OWNER : Integer
LENDER_OWNER = 0

export LENDER_NEXT_LENDER_ID : Integer
LENDER_NEXT_LENDER_ID = 1

export LENDER_REENTRANCY_LOCK : Integer
LENDER_REENTRANCY_LOCK = 2

export LENDER_HEAD : Integer
LENDER_HEAD = 3

export LENDER_TAIL : Integer
LENDER_TAIL = 4

-- LenderConfig struct
export LENDER_CONFIG_OWNER : Integer
LENDER_CONFIG_OWNER = 0

export LENDER_CONFIG_BORROWING_FEE : Integer
LENDER_CONFIG_BORROWING_FEE = 1

export LENDER_CONFIG_LIQUIDATION_THRESHOLD : Integer
LENDER_CONFIG_LIQUIDATION_THRESHOLD = 2

export LENDER_CONFIG_TOTAL_BORROWED : Integer
LENDER_CONFIG_TOTAL_BORROWED = 3

export LENDER_CONFIG_TOTAL_COLLATERAL : Integer
LENDER_CONFIG_TOTAL_COLLATERAL = 4

export LENDER_CONFIG_ACTIVE : Integer
LENDER_CONFIG_ACTIVE = 5

export LENDER_CONFIG_STRUCT_SIZE : Integer
LENDER_CONFIG_STRUCT_SIZE = 6

-- BorrowingPosition struct
export BORROW_COLLATERAL_AMOUNT : Integer
BORROW_COLLATERAL_AMOUNT = 0

export BORROW_DEBT : Integer
BORROW_DEBT = 1

export BORROW_COLLATERAL_TOKEN : Integer
BORROW_COLLATERAL_TOKEN = 2

export BORROW_LAST_UPDATE : Integer
BORROW_LAST_UPDATE = 3

export BORROW_STRUCT_SIZE : Integer
BORROW_STRUCT_SIZE = 4

-- =============================================================================
-- Oracle Storage Layout
-- =============================================================================

export ORACLE_NEXT_REQUEST_ID : Integer
ORACLE_NEXT_REQUEST_ID = 0

-- OracleRequest struct
export ORACLE_REQ_REQUESTER : Integer
ORACLE_REQ_REQUESTER = 0

export ORACLE_REQ_MARKET_ID : Integer
ORACLE_REQ_MARKET_ID = 1

export ORACLE_REQ_CREATED_AT : Integer
ORACLE_REQ_CREATED_AT = 2

export ORACLE_REQ_DEADLINE : Integer
ORACLE_REQ_DEADLINE = 3

export ORACLE_REQ_RESOLVED : Integer
ORACLE_REQ_RESOLVED = 4

export ORACLE_REQ_OUTCOME : Integer
ORACLE_REQ_OUTCOME = 5

export ORACLE_REQ_STAKE_FOR : Integer
ORACLE_REQ_STAKE_FOR = 6

export ORACLE_REQ_STAKE_AGAINST : Integer
ORACLE_REQ_STAKE_AGAINST = 7

export ORACLE_REQ_STRUCT_SIZE : Integer
ORACLE_REQ_STRUCT_SIZE = 8

-- =============================================================================
-- Slot Calculation Functions
-- =============================================================================

||| Calculate slot hash for composite key
export
slotHash : Integer -> Integer -> Integer
slotHash key baseSlot =
  let combined = key * 0x10000000000000000 + baseSlot
  in keccak256 combined 32

||| Get market slot: keccak256(marketId . SLOT_PPM + 100)
export
getMarketSlot : MarketId -> Integer
getMarketSlot mid = slotHash mid (SLOT_PPM + 100)

||| Get position slot: keccak256(tokenId . SLOT_PPM + 200)
export
getPositionSlot : TokenId -> Integer
getPositionSlot tid = slotHash tid (SLOT_PPM + 200)

||| Get NFT owner slot: keccak256(tokenId . SLOT_PPM + 300)
export
getNFTOwnerSlot : TokenId -> Integer
getNFTOwnerSlot tid = slotHash tid (SLOT_PPM + 300)

||| Get balance slot for address: keccak256(addr . baseSlot)
export
getBalanceSlot : Address -> Integer -> Integer
getBalanceSlot addr baseSlot = slotHash addr baseSlot

||| Get lock slot: keccak256(addr . SLOT_ADDICT + 100)
export
getLockSlot : Address -> Integer
getLockSlot addr = slotHash addr (SLOT_ADDICT + 100)

||| Get gauge slot: keccak256(gaugeAddr . SLOT_ADDICT + 200)
export
getGaugeSlot : Address -> Integer
getGaugeSlot gaugeAddr = slotHash gaugeAddr (SLOT_ADDICT + 200)

||| Get lender config slot: keccak256(lenderId . SLOT_LENDER + 100)
export
getLenderConfigSlot : LenderId -> Integer
getLenderConfigSlot lid = slotHash lid (SLOT_LENDER + 100)

||| Get borrowing position slot: keccak256(user . keccak256(lenderId . SLOT_LENDER + 200))
export
getBorrowPositionSlot : LenderId -> Address -> Integer
getBorrowPositionSlot lid user =
  let lenderPosBase = slotHash lid (SLOT_LENDER + 200)
  in slotHash user lenderPosBase

||| Get oracle request slot: keccak256(requestId . SLOT_ORACLE + 100)
export
getOracleRequestSlot : Integer -> Integer
getOracleRequestSlot reqId = slotHash reqId (SLOT_ORACLE + 100)

-- =============================================================================
-- Constants
-- =============================================================================

||| 1e18 scaling factor
export
E18 : Integer
E18 = 1000000000000000000

||| Basis points denominator (10000 = 100%)
export
BPS : Integer
BPS = 10000

||| Default funding period (8 hours in seconds)
export
DEFAULT_FUNDING_PERIOD : Integer
DEFAULT_FUNDING_PERIOD = 28800

||| Minimum lock time (1 week)
export
MIN_LOCK_TIME : Integer
MIN_LOCK_TIME = 604800

||| Maximum lock time (4 years)
export
MAX_LOCK_TIME : Integer
MAX_LOCK_TIME = 126144000

||| Initial emission rate (10M/year scaled by 1e18)
export
INITIAL_EMISSION_RATE : Integer
INITIAL_EMISSION_RATE = 10000000 * E18

||| Halving period (4 years)
export
HALVING_PERIOD : Integer
HALVING_PERIOD = 126144000

||| Minimum boost (1x)
export
MIN_BOOST : Integer
MIN_BOOST = E18

||| Maximum boost (2.5x)
export
MAX_BOOST : Integer
MAX_BOOST = 25 * E18 `div` 10

-- =============================================================================
-- Fixed-Point Math Helpers
-- =============================================================================

||| Multiply two 1e18-scaled values
export
mulE18 : Integer -> Integer -> Integer
mulE18 a b = (a * b) `div` E18

||| Divide to get 1e18-scaled result
export
divE18 : Integer -> Integer -> Integer
divE18 a b = if b == 0 then 0 else (a * E18) `div` b

||| Calculate percentage in basis points
export
bpsOf : Integer -> Integer -> Integer
bpsOf amount bps = (amount * bps) `div` BPS

||| Clamp value between min and max
export
clamp : Integer -> Integer -> Integer -> Integer
clamp minVal maxVal val = max minVal (min maxVal val)
