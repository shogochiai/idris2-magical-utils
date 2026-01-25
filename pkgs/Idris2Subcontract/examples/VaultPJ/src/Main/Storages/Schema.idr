||| VaultPJ Storage Schema
|||
||| Simple ETH vault with deposits and withdrawals.
|||
||| ```solidity
||| /// @custom:storage-location erc7201:vaultpj.vault.v1
||| struct $Vault {
|||     mapping(address => uint256) deposits;
|||     uint256 totalDeposits;
|||     address owner;
|||     bool paused;
||| }
||| ```
module Main.Storages.Schema

import public Subcontract.Core.Schema

-- =============================================================================
-- Storage Root
-- =============================================================================

public export
VAULT_ROOT : Integer
VAULT_ROOT = 0xdef456789abc123def456789abc123def456789abc123def456789abc12300

-- =============================================================================
-- Schema
-- =============================================================================

public export
VaultSchema : Schema
VaultSchema = MkSchema "vaultpj.vault.v1" VAULT_ROOT
  [ Mapping "deposits" TAddress TUint256
  , Value "totalDeposits" TUint256
  , Value "owner" TAddress
  , Value "paused" TBool
  ]

-- =============================================================================
-- Accessors
-- =============================================================================

export
getDeposit : Integer -> IO Integer
getDeposit addr = do
  result <- schemaMapping VaultSchema "deposits" addr
  pure (maybe 0 id result)

export
setDeposit : Integer -> Integer -> IO ()
setDeposit addr val = do
  _ <- schemaSetMapping VaultSchema "deposits" addr val
  pure ()

export
getTotalDeposits : IO Integer
getTotalDeposits = do
  result <- schemaValue VaultSchema "totalDeposits"
  pure (maybe 0 id result)

export
setTotalDeposits : Integer -> IO ()
setTotalDeposits val = do
  _ <- schemaSetValue VaultSchema "totalDeposits" val
  pure ()

export
getOwner : IO Integer
getOwner = do
  result <- schemaValue VaultSchema "owner"
  pure (maybe 0 id result)

export
setOwner : Integer -> IO ()
setOwner addr = do
  _ <- schemaSetValue VaultSchema "owner" addr
  pure ()

export
isPaused : IO Bool
isPaused = do
  result <- schemaValue VaultSchema "paused"
  pure (maybe False (\v => v /= 0) result)

export
setPaused : Bool -> IO ()
setPaused val = do
  _ <- schemaSetValue VaultSchema "paused" (if val then 1 else 0)
  pure ()
