||| TokenPJ Deploy Script
|||
||| Deploys the TokenPJ contract system:
||| 1. Deploy TokenImpl (this contract's implementation)
||| 2. Deploy Dictionary with selector → TokenImpl mappings
||| 3. Deploy Proxy pointing to Dictionary
||| 4. Initialize storage (set owner, etc.)
|||
||| Usage:
|||   idris2-yul --script deploy.idr --rpc $RPC_URL --private-key $PRIVATE_KEY
module Deploy

import EVM.Primitives
import Subcontract.Standards.ERC7546.Proxy
import Subcontract.Standards.ERC7546.Forward
import Main.Storages.Schema

-- =============================================================================
-- Deployment Configuration
-- =============================================================================

||| Initial owner (deployer)
||| In real deployment, this comes from environment
INITIAL_OWNER : Integer
INITIAL_OWNER = 0  -- Will be set to msg.sender

-- =============================================================================
-- Initialization
-- =============================================================================

||| Initialize the token after proxy deployment
||| Called once when proxy is first deployed
export
initialize : Integer -> IO ()
initialize owner = do
  -- Set owner
  setOwner owner
  -- Could set name, symbol, decimals here if stored

||| Initialize entry point
||| Selector: initialize(address) => 0xc4d66de8
export
initializeMain : IO ()
initializeMain = do
  -- Only allow if owner not set (first time)
  currentOwner <- getOwner
  if currentOwner /= 0
    then evmRevert 0 0  -- Already initialized
    else do
      -- Get owner from calldata
      owner <- calldataload 4
      initialize owner
      stop

-- =============================================================================
-- Deploy Notes
-- =============================================================================

-- Deployment steps (execute via forge/cast):
--
-- 1. Build contracts:
--    cd idris2-yul
--    ./scripts/build-contract.sh ../idris2-subcontract/examples/TokenPJ/src/Main.idr
--    ./scripts/build-contract.sh ../idris2-subcontract/examples/TokenPJ/scripts/deploy.idr
--
-- 2. Deploy TokenImpl:
--    IMPL=$(cast send --create $IMPL_BYTECODE --rpc-url $RPC --private-key $PK)
--
-- 3. Deploy Dictionary with mappings:
--    # Dictionary maps each selector to IMPL address
--    # Use Dictionary.setImplementation for each function
--
-- 4. Deploy Proxy:
--    PROXY=$(cast send --create $PROXY_BYTECODE --rpc-url $RPC --private-key $PK)
--
-- 5. Set Dictionary in Proxy:
--    cast send $PROXY "setDictionary(address)" $DICTIONARY --rpc-url $RPC --private-key $PK
--
-- 6. Initialize:
--    cast send $PROXY "initialize(address)" $OWNER --rpc-url $RPC --private-key $PK
