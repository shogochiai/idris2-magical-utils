||| MC Core: Minimal Proxy Contract
||| Idris2 implementation of EIP-1167 minimal proxy pattern
|||
||| Forwards all calls to a dictionary (implementation) contract via DELEGATECALL
module Main

import EVM.Primitives

-- =============================================================================
-- Storage Layout
-- =============================================================================

||| Storage slot for dictionary (implementation) address
||| Using EIP-1967 style slot: keccak256("eip1967.proxy.implementation") - 1
||| For simplicity, we use slot 0
DICTIONARY_SLOT : Integer
DICTIONARY_SLOT = 0

-- =============================================================================
-- Proxy Logic
-- =============================================================================

||| Forward call to dictionary via DELEGATECALL
||| 1. Copy calldata to memory
||| 2. DELEGATECALL to dictionary with all gas
||| 3. Copy return data to memory
||| 4. Return or revert based on success
forwardCall : IO ()
forwardCall = do
  -- Get dictionary address from storage
  dictionary <- sload DICTIONARY_SLOT

  -- Get calldata size
  cdSize <- calldatasize

  -- Copy calldata to memory at offset 0
  calldatacopy 0 0 cdSize

  -- Get available gas (minus some overhead)
  availableGas <- gas

  -- DELEGATECALL to dictionary
  -- delegatecall(gas, address, argsOffset, argsSize, retOffset, retSize)
  -- We don't pre-allocate return buffer, will use returndatacopy
  success <- delegatecall availableGas dictionary 0 cdSize 0 0

  -- Get return data size
  rdSize <- returndatasize

  -- Copy return data to memory at offset 0
  returndatacopy 0 0 rdSize

  -- Conditional return or revert (no branching in Idris, handled by EVM)
  returnOrRevert success 0 rdSize

-- =============================================================================
-- Entry Point
-- =============================================================================

main : IO ()
main = forwardCall
