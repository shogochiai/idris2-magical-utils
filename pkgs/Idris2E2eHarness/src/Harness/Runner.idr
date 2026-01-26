module Harness.Runner

import Harness.Types
import Harness.Anvil
import Harness.Replica
import Harness.Evm
import Harness.Icp
import Data.List
import Data.String

%default total

||| Join lines with newline separator
joinLines : List String -> String
joinLines [] = ""
joinLines [x] = x
joinLines (x :: xs) = x ++ "\n" ++ joinLines xs

||| Test runner configuration
public export
record RunnerConfig where
  constructor MkRunnerConfig
  anvilConfig : Maybe AnvilConfig
  replicaConfig : Maybe ReplicaConfig
  verbose : Bool
  stopOnFail : Bool

||| Default runner with both EVM and ICP
public export
defaultRunner : RunnerConfig
defaultRunner = MkRunnerConfig
  (Just defaultAnvilConfig)
  (Just defaultReplicaConfig)
  True
  True

||| EVM-only runner
public export
evmOnlyRunner : RunnerConfig
evmOnlyRunner = MkRunnerConfig
  (Just defaultAnvilConfig)
  Nothing
  True
  True

||| ICP-only runner
public export
icpOnlyRunner : RunnerConfig
icpOnlyRunner = MkRunnerConfig
  Nothing
  (Just defaultReplicaConfig)
  True
  True

||| Test step definition
public export
record TestStep where
  constructor MkTestStep
  name : String
  command : String
  expectSuccess : Bool
  extractResult : Maybe (String -> String)  -- Optional result extractor

||| Build setup commands for environment
public export
buildSetupCmds : RunnerConfig -> List String
buildSetupCmds cfg =
  let anvilCmd = case cfg.anvilConfig of
                   Just ac => [buildAnvilCmd ac ++ " &"]
                   Nothing => []
      replicaCmd = case cfg.replicaConfig of
                     Just rc => [buildStartCmd rc]
                     Nothing => []
  in anvilCmd ++ replicaCmd

||| Build teardown commands
public export
buildTeardownCmds : RunnerConfig -> List String
buildTeardownCmds cfg =
  let anvilKill = case cfg.anvilConfig of
                    Just _ => ["pkill -f 'anvil --port'"]
                    Nothing => []
      replicaStop = case cfg.replicaConfig of
                      Just _ => [buildStopCmd]
                      Nothing => []
  in anvilKill ++ replicaStop

||| Build health check commands
public export
buildHealthChecks : RunnerConfig -> List String
buildHealthChecks cfg =
  let anvilCheck = case cfg.anvilConfig of
                     Just ac => [Anvil.buildHealthCheckCmd ac.port]
                     Nothing => []
      replicaCheck = case cfg.replicaConfig of
                       Just _ => [Replica.buildHealthCheckCmd]
                       Nothing => []
  in anvilCheck ++ replicaCheck

||| Create EVM contract deployment step
public export
deployContractStep : (name : String) -> (contractPath : String) -> (rpcUrl : String) -> (privateKey : String) -> (args : List String) -> TestStep
deployContractStep name path rpc key args =
  let cmd = buildDeployCmd (castWithKey rpc key) path args
  in MkTestStep ("Deploy " ++ name) cmd True Nothing

||| Create canister deployment step
public export
deployCanisterStep : (name : String) -> (canister : String) -> (wasmPath : Maybe String) -> TestStep
deployCanisterStep name canister wasm =
  let cmd = buildDeployCmd dfxLocal canister wasm
  in MkTestStep ("Deploy " ++ name) cmd True Nothing

||| Create EVM transaction step
public export
evmTxStep : (name : String) -> (contract : String) -> (sig : String) -> (args : List String) -> (rpc : String) -> (key : String) -> TestStep
evmTxStep name contract sig args rpc key =
  let cmd = buildSendCmd (castWithKey rpc key) contract sig args
  in MkTestStep name cmd True Nothing

||| Create canister call step
public export
canisterCallStep : (name : String) -> (canister : String) -> (method : String) -> (args : String) -> TestStep
canisterCallStep name canister method args =
  let cmd = buildCanisterCall dfxLocal canister method args
  in MkTestStep name cmd True Nothing

||| Create wait/sleep step
public export
waitStep : (seconds : Nat) -> TestStep
waitStep secs = MkTestStep ("Wait " ++ show secs ++ "s") ("sleep " ++ show secs) True Nothing

||| Create assertion step (runs command and checks exit code)
public export
assertStep : (name : String) -> (command : String) -> TestStep
assertStep name cmd = MkTestStep ("Assert: " ++ name) cmd True Nothing

||| Format test step for shell execution with logging
public export
formatStepForShell : TestStep -> String
formatStepForShell step =
  "echo '=== " ++ step.name ++ " ===' && " ++ step.command

||| Format all steps as shell script
public export
formatAsScript : (testName : String) -> List TestStep -> String
formatAsScript name steps =
  let header = "#!/bin/bash\nset -e\necho 'Running: " ++ name ++ "'\n\n"
      body = joinLines (map formatStepForShell steps)
      footer = "\necho 'Test passed: " ++ name ++ "'\n"
  in header ++ body ++ footer
