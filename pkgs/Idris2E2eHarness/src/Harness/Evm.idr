module Harness.Evm

import Harness.Types
import Data.String
import Data.List

%default total

||| Join strings with space separator
joinWithSpace : List String -> String
joinWithSpace [] = ""
joinWithSpace [x] = x
joinWithSpace (x :: xs) = x ++ " " ++ joinWithSpace xs

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

||| Cast command abstraction
||| Wraps foundry's cast CLI for EVM interactions
public export
record CastCmd where
  constructor MkCastCmd
  rpcUrl : String
  privateKey : Maybe String  -- For sending transactions

||| Create cast command for read-only operations
public export
castReadOnly : String -> CastCmd
castReadOnly rpc = MkCastCmd rpc Nothing

||| Create cast command with signing capability
public export
castWithKey : String -> String -> CastCmd
castWithKey rpc key = MkCastCmd rpc (Just key)

||| Build cast call command string
public export
buildCallCmd : CastCmd -> (contract : String) -> (sig : String) -> (args : List String) -> String
buildCallCmd cmd contract sig args =
  let argsStr = joinWithSpace args
      baseCmd = "cast call " ++ contract ++ " '" ++ sig ++ "' " ++ argsStr
  in baseCmd ++ " --rpc-url " ++ cmd.rpcUrl

||| Build cast send command string
public export
buildSendCmd : CastCmd -> (contract : String) -> (sig : String) -> (args : List String) -> String
buildSendCmd cmd contract sig args =
  let argsStr = joinWithSpace args
      keyPart = maybe "" (\k => " --private-key " ++ k) cmd.privateKey
      baseCmd = "cast send " ++ contract ++ " '" ++ sig ++ "' " ++ argsStr
  in baseCmd ++ " --rpc-url " ++ cmd.rpcUrl ++ keyPart

||| Build cast logs command for event fetching
public export
buildLogsCmd : CastCmd -> (topic : String) -> (fromBlock : Nat) -> (toBlock : Maybe Nat) -> String
buildLogsCmd cmd topic fromBlock mToBlock =
  let toBlockStr = maybe "latest" show mToBlock
  in "cast logs --from-block " ++ show fromBlock ++ " --to-block " ++ toBlockStr
     ++ " --topic " ++ topic ++ " --rpc-url " ++ cmd.rpcUrl

||| Build cast receipt command
public export
buildReceiptCmd : CastCmd -> (txHash : String) -> String
buildReceiptCmd cmd txHash =
  "cast receipt " ++ txHash ++ " --rpc-url " ++ cmd.rpcUrl

||| Build cast block command
public export
buildBlockCmd : CastCmd -> String
buildBlockCmd cmd = "cast block-number --rpc-url " ++ cmd.rpcUrl

||| Parse transaction receipt for logs count
||| Returns number of logs emitted (basic parsing)
covering
public export
parseLogsCount : String -> Nat
parseLogsCount output =
  let ls = lines output
      logLines = filter (isSubstr "logIndex") ls
  in length logLines

||| Contract deployment helper (build forge create command)
public export
buildDeployCmd : CastCmd -> (contractPath : String) -> (constructorArgs : List String) -> String
buildDeployCmd cmd path args =
  let argsStr = if null args then "" else " --constructor-args " ++ joinWithSpace args
      keyPart = maybe "" (\k => " --private-key " ++ k) cmd.privateKey
  in "forge create " ++ path ++ argsStr ++ " --rpc-url " ++ cmd.rpcUrl ++ keyPart
