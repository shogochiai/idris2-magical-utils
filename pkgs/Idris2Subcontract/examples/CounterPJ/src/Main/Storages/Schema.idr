||| CounterPJ Storage Schema
|||
||| Minimal example: single counter with owner.
|||
||| ```solidity
||| /// @custom:storage-location erc7201:counterpj.counter.v1
||| struct $Counter {
|||     uint256 count;
|||     address owner;
||| }
||| ```
module Main.Storages.Schema

import public Subcontract.Core.Schema

-- =============================================================================
-- Storage Root
-- =============================================================================

public export
COUNTER_ROOT : Integer
COUNTER_ROOT = 0xabc123def456789abc123def456789abc123def456789abc123def45678900

-- =============================================================================
-- Schema
-- =============================================================================

public export
CounterSchema : Schema
CounterSchema = MkSchema "counterpj.counter.v1" COUNTER_ROOT
  [ Value "count" TUint256
  , Value "owner" TAddress
  ]

-- =============================================================================
-- Accessors
-- =============================================================================

export
getCount : IO Integer
getCount = do
  result <- schemaValue CounterSchema "count"
  pure (maybe 0 id result)

export
setCount : Integer -> IO ()
setCount val = do
  _ <- schemaSetValue CounterSchema "count" val
  pure ()

export
getOwner : IO Integer
getOwner = do
  result <- schemaValue CounterSchema "owner"
  pure (maybe 0 id result)

export
setOwner : Integer -> IO ()
setOwner addr = do
  _ <- schemaSetValue CounterSchema "owner" addr
  pure ()
