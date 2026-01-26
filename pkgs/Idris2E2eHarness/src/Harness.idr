||| E2E Test Harness for Cross-Chain Testing
|||
||| Provides abstractions for:
||| - EVM testing via Anvil + Cast
||| - ICP testing via DFX + Replica
||| - Cross-chain E2E test orchestration
|||
||| Usage:
||| ```idris
||| import Harness
|||
||| myTest : List TestStep
||| myTest =
|||   [ deployContractStep "OUF" "src/OUF.sol:OUF" rpc key []
|||   , deployCanisterStep "OUC" "ouc" Nothing
|||   , evmTxStep "ProposeUpgrade" oufAddr "proposeUpgrade(address,address,bytes4)" [...] rpc key
|||   , waitStep 5
|||   , canisterCallStep "FetchLogs" "ouc" "fetchEvmLogs" "()"
|||   , assertStep "EventsStored" "dfx canister call ouc getEventsCount | grep -q '1'"
|||   ]
||| ```
module Harness

import public Harness.Types
import public Harness.Evm
import public Harness.Icp
import public Harness.Anvil
import public Harness.Replica
import public Harness.Runner
