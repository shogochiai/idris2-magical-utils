||| Test importing MC Storage API from idris2-mc
|||
||| Demonstrates using ERC-7201 storage slot calculations
module Main

import MC.Std.Storage.ERC7201
import MC.Std.Storage.Schema

import EVM.Primitives

-- =============================================================================
-- Local aliases
-- =============================================================================

localMstore : Integer -> Integer -> IO ()
localMstore = mstore

-- =============================================================================
-- Use MC Storage API
-- =============================================================================

||| Example: Read admin address using MC Schema
readAdminExample : IO Integer
readAdminExample = do
  -- Use pre-computed slot from MC library
  readAddress SLOT_MC_ADMIN

||| Example: Check feature toggle using MC Schema
checkFeatureExample : Integer -> IO Bool
checkFeatureExample selector = do
  slot <- mappingSlot SLOT_MC_FEATURE_TOGGLE selector
  readBool slot

||| Example: Get member at index using MC Schema
getMemberExample : Integer -> IO Integer
getMemberExample index = do
  slot <- arrayElementSlot SLOT_MC_MEMBER index 1
  readAddress slot

-- =============================================================================
-- Custom Schema using MC API
-- =============================================================================

||| Define a custom schema using MC's Schema DSL
MyTokenSchema : Schema
MyTokenSchema = MkSchema
  "myapp.token"
  0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcd00
  [ ValueField "totalSupply" TUint256 0
  , MappingField "balances" TAddress TUint256 1
  , NestedMappingField "allowances" TAddress TAddress TUint256 2
  ]

-- =============================================================================
-- Function Selectors
-- =============================================================================

-- getAdminSlot() returns slot value
SEL_GET_ADMIN_SLOT : Integer
SEL_GET_ADMIN_SLOT = 0x12345678

-- getFeatureToggleSlot() returns slot value
SEL_GET_FEATURE_TOGGLE_SLOT : Integer
SEL_GET_FEATURE_TOGGLE_SLOT = 0x23456789

-- getMemberSlot() returns slot value
SEL_GET_MEMBER_SLOT : Integer
SEL_GET_MEMBER_SLOT = 0x34567890

-- getTokenSchemaRoot() returns custom schema root
SEL_GET_TOKEN_SCHEMA_ROOT : Integer
SEL_GET_TOKEN_SCHEMA_ROOT = 0x45678901

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

returnUint : Integer -> IO ()
returnUint val = do
  localMstore 0 val
  evmReturn 0 32

main : IO ()
main = do
  selector <- getSelector

  if selector == SEL_GET_ADMIN_SLOT
    then returnUint SLOT_MC_ADMIN

    else if selector == SEL_GET_FEATURE_TOGGLE_SLOT
    then returnUint SLOT_MC_FEATURE_TOGGLE

    else if selector == SEL_GET_MEMBER_SLOT
    then returnUint SLOT_MC_MEMBER

    else if selector == SEL_GET_TOKEN_SCHEMA_ROOT
    then returnUint (schemaRoot MyTokenSchema)

    else evmRevert 0 0
