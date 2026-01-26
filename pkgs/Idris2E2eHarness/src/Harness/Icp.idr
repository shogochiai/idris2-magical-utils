module Harness.Icp

import Harness.Types
import Data.String
import Data.List

%default total

||| Check if needle is substring of haystack
covering
isSubstr : String -> String -> Bool
isSubstr needle haystack = go 0
  where
    covering
    go : Nat -> Bool
    go idx =
      if idx + length needle > length haystack
        then False
        else substr idx (length needle) haystack == needle || go (S idx)

||| DFX command abstraction
||| Wraps dfx CLI for ICP canister interactions
public export
record DfxCmd where
  constructor MkDfxCmd
  network : String  -- "local", "ic", or custom
  identity : Maybe String

||| Create dfx command for local network
public export
dfxLocal : DfxCmd
dfxLocal = MkDfxCmd "local" Nothing

||| Create dfx command for IC mainnet
public export
dfxMainnet : DfxCmd
dfxMainnet = MkDfxCmd "ic" Nothing

||| Create dfx command with identity
public export
dfxWithIdentity : String -> String -> DfxCmd
dfxWithIdentity net ident = MkDfxCmd net (Just ident)

||| Build dfx canister call command
public export
buildCanisterCall : DfxCmd -> (canister : String) -> (method : String) -> (args : String) -> String
buildCanisterCall cmd canister method args =
  let identPart = maybe "" (\i => " --identity " ++ i) cmd.identity
  in "dfx canister call " ++ canister ++ " " ++ method ++ " '" ++ args ++ "'"
     ++ " --network " ++ cmd.network ++ identPart

||| Build dfx canister call with raw argument
public export
buildCanisterCallRaw : DfxCmd -> (canisterId : String) -> (method : String) -> (argBytes : String) -> String
buildCanisterCallRaw cmd canisterId method argBytes =
  let identPart = maybe "" (\i => " --identity " ++ i) cmd.identity
  in "dfx canister call " ++ canisterId ++ " " ++ method
     ++ " --argument-file /dev/stdin"
     ++ " --network " ++ cmd.network ++ identPart
     ++ " <<< '" ++ argBytes ++ "'"

||| Build dfx deploy command
public export
buildDeployCmd : DfxCmd -> (canister : String) -> (wasmPath : Maybe String) -> String
buildDeployCmd cmd canister mWasm =
  let wasmPart = maybe "" (\w => " --wasm " ++ w) mWasm
      identPart = maybe "" (\i => " --identity " ++ i) cmd.identity
  in "dfx deploy " ++ canister ++ wasmPart
     ++ " --network " ++ cmd.network ++ identPart

||| Build dfx canister status command
public export
buildStatusCmd : DfxCmd -> (canister : String) -> String
buildStatusCmd cmd canister =
  let identPart = maybe "" (\i => " --identity " ++ i) cmd.identity
  in "dfx canister status " ++ canister
     ++ " --network " ++ cmd.network ++ identPart

||| Build dfx identity get-principal command
public export
buildGetPrincipalCmd : DfxCmd -> String
buildGetPrincipalCmd cmd =
  let identPart = maybe "" (\i => " --identity " ++ i) cmd.identity
  in "dfx identity get-principal" ++ identPart

||| Build dfx ledger balance command
public export
buildBalanceCmd : DfxCmd -> (account : String) -> String
buildBalanceCmd cmd account =
  let identPart = maybe "" (\i => " --identity " ++ i) cmd.identity
  in "dfx ledger balance " ++ account
     ++ " --network " ++ cmd.network ++ identPart

||| Build dfx canister id command
public export
buildGetIdCmd : DfxCmd -> (canister : String) -> String
buildGetIdCmd cmd canister =
  "dfx canister id " ++ canister ++ " --network " ++ cmd.network

||| Parse canister ID from dfx deploy output
covering
public export
parseCanisterId : String -> Maybe String
parseCanisterId output =
  let ls = lines output
      idLines = filter (\s => isSubstr "canister" s && isSubstr "-" s) ls
  in case idLines of
       (l :: _) => Nothing  -- Placeholder: would need proper parsing
       [] => Nothing
