module Harness.Types

import Data.List

%default total

||| Result of a process execution
public export
record ProcessResult where
  constructor MkProcessResult
  exitCode : Int
  stdout : String
  stderr : String

||| Check if process succeeded
public export
isSuccess : ProcessResult -> Bool
isSuccess r = r.exitCode == 0

||| EVM chain configuration
public export
record ChainConfig where
  constructor MkChainConfig
  chainId : Nat
  rpcUrl : String
  blockNumber : Maybe Nat  -- Fork block number (Nothing = latest)

||| Common chain configurations
public export
baseMainnet : ChainConfig
baseMainnet = MkChainConfig 8453 "https://mainnet.base.org" Nothing

public export
ethereumMainnet : ChainConfig
ethereumMainnet = MkChainConfig 1 "https://eth.llamarpc.com" Nothing

public export
baseSepolia : ChainConfig
baseSepolia = MkChainConfig 84532 "https://sepolia.base.org" Nothing

||| Anvil instance state
public export
record AnvilState where
  constructor MkAnvilState
  pid : Int
  port : Nat
  chainConfig : ChainConfig

||| ICP Replica state
public export
record ReplicaState where
  constructor MkReplicaState
  pid : Int
  port : Nat
  networkName : String  -- "local" or custom

||| Deployed contract info
public export
record DeployedContract where
  constructor MkDeployedContract
  address : String
  deployBlock : Nat
  txHash : String

||| Canister deployment info
public export
record DeployedCanister where
  constructor MkDeployedCanister
  canisterId : String
  modulePath : String

||| E2E test environment combining EVM and ICP
public export
record E2eEnv where
  constructor MkE2eEnv
  anvil : Maybe AnvilState
  replica : Maybe ReplicaState
  contracts : List (String, DeployedContract)  -- name -> contract
  canisters : List (String, DeployedCanister)  -- name -> canister

||| Empty E2E environment
public export
emptyEnv : E2eEnv
emptyEnv = MkE2eEnv Nothing Nothing [] []

||| Test step result
public export
data StepResult : Type where
  StepOk : String -> StepResult
  StepFail : String -> String -> StepResult  -- step name, error message

||| Check if step succeeded
public export
stepSucceeded : StepResult -> Bool
stepSucceeded (StepOk _) = True
stepSucceeded (StepFail _ _) = False

||| E2E test case
public export
record E2eTestCase where
  constructor MkE2eTestCase
  name : String
  description : String
  tags : List String  -- "evm", "icp", "cross-chain", etc.
