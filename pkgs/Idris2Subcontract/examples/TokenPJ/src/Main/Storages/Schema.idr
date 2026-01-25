||| TokenPJ Storage Schema
|||
||| Defines the storage layout for the Token contract.
||| This mirrors the Solidity struct pattern:
|||
||| ```solidity
||| /// @custom:storage-location erc7201:tokenpj.token.v1
||| struct $Token {
|||     uint256 totalSupply;
|||     mapping(address => uint256) balances;
|||     mapping(address => mapping(address => uint256)) allowances;
|||     string name;
|||     string symbol;
|||     uint8 decimals;
|||     address owner;
||| }
||| ```
module Main.Storages.Schema

import public Subcontract.Core.Schema

-- =============================================================================
-- Storage Root
-- =============================================================================

||| ERC-7201 namespace: "tokenpj.token.v1"
||| Computed: keccak256(keccak256("tokenpj.token.v1") - 1) & ~0xff
public export
TOKEN_ROOT : Integer
TOKEN_ROOT = 0x8a3c5e7f9b1d2a4c6e8f0b2d4a6c8e0f2a4b6c8d0e2f4a6b8c0d2e4f6a8b0c00

-- =============================================================================
-- Schema Definition
-- =============================================================================

||| Token storage schema
public export
TokenSchema : Schema
TokenSchema = MkSchema "tokenpj.token.v1" TOKEN_ROOT
  [ Value "totalSupply" TUint256      -- slot+0: Total token supply
  , Mapping "balances" TAddress TUint256  -- slot+1: User balances
  , Mapping2 "allowances" TAddress TAddress TUint256  -- slot+2: Allowances
  , Value "decimals" TUint8           -- slot+3: Token decimals (usually 18)
  , Value "owner" TAddress            -- slot+4: Contract owner
  ]

-- =============================================================================
-- Accessor Helpers
-- =============================================================================

||| Get total supply
export
getTotalSupply : IO Integer
getTotalSupply = do
  result <- schemaValue TokenSchema "totalSupply"
  pure (maybe 0 id result)

||| Set total supply
export
setTotalSupply : Integer -> IO ()
setTotalSupply val = do
  _ <- schemaSetValue TokenSchema "totalSupply" val
  pure ()

||| Get balance of address
export
getBalance : Integer -> IO Integer
getBalance addr = do
  result <- schemaMapping TokenSchema "balances" addr
  pure (maybe 0 id result)

||| Set balance of address
export
setBalance : Integer -> Integer -> IO ()
setBalance addr val = do
  _ <- schemaSetMapping TokenSchema "balances" addr val
  pure ()

||| Get allowance
export
getAllowance : Integer -> Integer -> IO Integer
getAllowance owner spender = do
  result <- schemaMapping2 TokenSchema "allowances" owner spender
  pure (maybe 0 id result)

||| Set allowance
export
setAllowance : Integer -> Integer -> Integer -> IO ()
setAllowance owner spender val = do
  _ <- schemaSetMapping2 TokenSchema "allowances" owner spender val
  pure ()

||| Get owner
export
getOwner : IO Integer
getOwner = do
  result <- schemaValue TokenSchema "owner"
  pure (maybe 0 id result)

||| Set owner
export
setOwner : Integer -> IO ()
setOwner addr = do
  _ <- schemaSetValue TokenSchema "owner" addr
  pure ()
