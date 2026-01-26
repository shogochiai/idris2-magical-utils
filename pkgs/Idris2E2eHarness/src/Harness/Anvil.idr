module Harness.Anvil

import Harness.Types

%default total

||| Anvil configuration
public export
record AnvilConfig where
  constructor MkAnvilConfig
  port : Nat
  forkConfig : Maybe ChainConfig
  blockTime : Maybe Nat  -- Auto-mine interval in seconds
  accounts : Nat         -- Number of test accounts
  balance : Nat          -- Initial balance in ETH

||| Default anvil configuration (no fork, instant mining)
public export
defaultAnvilConfig : AnvilConfig
defaultAnvilConfig = MkAnvilConfig 8545 Nothing Nothing 10 10000

||| Anvil config for forking a chain
public export
forkConfig : ChainConfig -> Nat -> AnvilConfig
forkConfig chain port = MkAnvilConfig port (Just chain) Nothing 10 10000

||| Build anvil start command
public export
buildAnvilCmd : AnvilConfig -> String
buildAnvilCmd cfg =
  let portPart = " --port " ++ show cfg.port
      forkPart = case cfg.forkConfig of
                   Nothing => ""
                   Just fc => " --fork-url " ++ fc.rpcUrl
                              ++ maybe "" (\b => " --fork-block-number " ++ show b) fc.blockNumber
      timePart = maybe "" (\t => " --block-time " ++ show t) cfg.blockTime
      accPart = " --accounts " ++ show cfg.accounts
      balPart = " --balance " ++ show cfg.balance
  in "anvil" ++ portPart ++ forkPart ++ timePart ++ accPart ++ balPart

||| Build command to check if anvil is ready
public export
buildHealthCheckCmd : Nat -> String
buildHealthCheckCmd port =
  "cast block-number --rpc-url http://localhost:" ++ show port

||| Default test accounts (anvil's deterministic accounts)
||| These are generated from the default mnemonic
public export
defaultTestAccounts : List (String, String)  -- (address, private key)
defaultTestAccounts =
  [ ("0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266", "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80")
  , ("0x70997970C51812dc3A010C7d01b50e0d17dc79C8", "0x59c6995e998f97a5a0044966f0945389dc9e86dae88c7a8412f4603b6b78690d")
  , ("0x3C44CdDdB6a900fa2b585dd299e03d12FA4293BC", "0x5de4111afa1a4b94908f83103eb1f1706367c2e68ca870fc3fb9a804cdab365a")
  , ("0x90F79bf6EB2c4f870365E785982E1f101E93b906", "0x7c852118294e51e653712a81e05800f419141751be58f605c371e15141b007a6")
  , ("0x15d34AAf54267DB7D7c367839AAf71A00a2C6A65", "0x47e179ec197488593b187f80a00eb0da91f1b9d0b13f8733639f19c30a34926a")
  ]

||| Get first test account
public export
defaultDeployer : (String, String)
defaultDeployer = case defaultTestAccounts of
  (x :: _) => x
  [] => ("", "")

||| Build anvil RPC URL for given port
public export
anvilRpcUrl : Nat -> String
anvilRpcUrl port = "http://localhost:" ++ show port
