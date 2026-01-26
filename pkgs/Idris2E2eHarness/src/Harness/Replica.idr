module Harness.Replica

import Harness.Types

%default total

||| DFX replica configuration
public export
record ReplicaConfig where
  constructor MkReplicaConfig
  port : Nat
  clean : Bool      -- Start with clean state
  background : Bool -- Run in background

||| Default replica configuration
public export
defaultReplicaConfig : ReplicaConfig
defaultReplicaConfig = MkReplicaConfig 4943 True True

||| Build dfx start command
public export
buildStartCmd : ReplicaConfig -> String
buildStartCmd cfg =
  let cleanPart = if cfg.clean then " --clean" else ""
      bgPart = if cfg.background then " --background" else ""
      portPart = " --host 127.0.0.1:" ++ show cfg.port
  in "dfx start" ++ cleanPart ++ bgPart ++ portPart

||| Build dfx stop command
public export
buildStopCmd : String
buildStopCmd = "dfx stop"

||| Build command to check if replica is ready
public export
buildHealthCheckCmd : String
buildHealthCheckCmd = "dfx ping"

||| Build dfx identity new command (for test identity)
public export
buildCreateIdentityCmd : String -> String
buildCreateIdentityCmd name =
  "dfx identity new " ++ name ++ " --storage-mode plaintext"

||| Build dfx identity use command
public export
buildUseIdentityCmd : String -> String
buildUseIdentityCmd name = "dfx identity use " ++ name

||| Build dfx wallet balance command
public export
buildWalletBalanceCmd : String -> String
buildWalletBalanceCmd network =
  "dfx wallet balance --network " ++ network

||| Build dfx ledger fabricate-cycles command (local only)
public export
buildFabricateCyclesCmd : String -> Nat -> String
buildFabricateCyclesCmd canister amount =
  "dfx ledger fabricate-cycles --canister " ++ canister
  ++ " --amount " ++ show amount

||| Replica URL for given port
public export
replicaUrl : Nat -> String
replicaUrl port = "http://127.0.0.1:" ++ show port
