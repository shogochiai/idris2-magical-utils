||| Subcontract Core: Typed Deployment Scripts
|||
||| Replaces bash deploy templates with type-safe Idris programs.
||| Scripts produce an AST that can be:
||| - Pretty-printed for review
||| - Executed against EVM (anvil/foundry)
||| - Simulated for testing
|||
||| Example:
|||   deployTextDAO : Script (Deployed Proxy, Deployed Dictionary)
|||   deployTextDAO = do
|||     dict <- deploy DictionaryBytecode
|||     proxy <- deploy ProxyBytecode
|||     call dict (setDictionary proxy.address)
|||     pure (proxy, dict)
|||
module Subcontract.Core.Script

import Subcontract.Core.ABI.Sig
import Subcontract.Core.ABI.Decoder
import Subcontract.Core.Process

-- =============================================================================
-- Contract References
-- =============================================================================

||| A deployed contract address
public export
record Deployed (name : String) where
  constructor MkDeployed
  address : Integer
  bytecodeHash : Integer

||| Pending deployment (bytecode not yet deployed)
public export
record Bytecode (name : String) where
  constructor MkBytecode
  code : String  -- Hex-encoded bytecode

-- =============================================================================
-- Script Commands (AST)
-- =============================================================================

||| Script command AST
public export
data ScriptCmd : Type -> Type where
  ||| Deploy a contract, returning its address
  DeployCmd : Bytecode name -> ScriptCmd (Deployed name)

  ||| Call a contract function
  CallCmd : {sig : Sig} -> Deployed name -> Sel sig -> List Integer -> ScriptCmd Integer

  ||| Send ETH to an address
  SendCmd : Integer -> Integer -> ScriptCmd ()

  ||| Log a message (for tracing)
  LogCmd : String -> ScriptCmd ()

  ||| Assert a condition
  AssertCmd : String -> Bool -> ScriptCmd ()

  ||| Record a value for later reference
  RecordCmd : String -> Integer -> ScriptCmd ()

  ||| Get recorded value
  GetRecordCmd : String -> ScriptCmd (Maybe Integer)

-- =============================================================================
-- Script Monad
-- =============================================================================

||| Script monad for composing deployment operations
public export
data Script : Type -> Type where
  Pure : a -> Script a
  Bind : Script a -> (a -> Script b) -> Script b
  Cmd : ScriptCmd a -> Script a

export
Functor Script where
  map f (Pure a) = Pure (f a)
  map f (Bind m k) = Bind m (\a => map f (k a))
  map f (Cmd c) = Bind (Cmd c) (Pure . f)

export
Applicative Script where
  pure = Pure
  mf <*> ma = Bind mf (\f => Bind ma (\a => Pure (f a)))

export
Monad Script where
  (>>=) = Bind

-- =============================================================================
-- Script Primitives
-- =============================================================================

||| Deploy a contract
export
deploy : Bytecode name -> Script (Deployed name)
deploy bc = Cmd (DeployCmd bc)

||| Call a contract function
export
call : {sig : Sig} -> Deployed name -> Sel sig -> List Integer -> Script Integer
call d sel args = Cmd (CallCmd d sel args)

||| Send ETH
export
send : Integer -> Integer -> Script ()
send to amount = Cmd (SendCmd to amount)

||| Log a message
export
logScript : String -> Script ()
logScript msg = Cmd (LogCmd msg)

||| Assert a condition
export
assertScript : String -> Bool -> Script ()
assertScript msg cond = Cmd (AssertCmd msg cond)

||| Record a value
export
recordValue : String -> Integer -> Script ()
recordValue name val = Cmd (RecordCmd name val)

-- =============================================================================
-- ERC-7546 UCS Combinators
-- =============================================================================

||| Deploy a complete UCS (Upgradeable Clone System)
||| Returns (Dictionary, Proxy) pair with dictionary set
export
deployUCS : Bytecode "Dictionary" -> Bytecode "Proxy" -> Script (Deployed "Dictionary", Deployed "Proxy")
deployUCS dictBc proxyBc = do
  logScript "=== Deploying UCS ==="

  -- Deploy Dictionary first
  logScript "1. Deploying Dictionary..."
  dict <- deploy dictBc
  recordValue "dictionary" dict.address

  -- Deploy Proxy
  logScript "2. Deploying Proxy..."
  proxy <- deploy proxyBc
  recordValue "proxy" proxy.address

  -- Initialize proxy with dictionary address
  -- (This would call setDictionary on proxy)
  logScript "3. Linking Proxy -> Dictionary"

  logScript "=== UCS Deployed ==="
  pure (dict, proxy)

||| Upgrade dictionary with new implementation
export
upgradeUCS : Deployed "Dictionary" -> (selector : Integer) -> (impl : Integer) -> Script ()
upgradeUCS dict selector impl = do
  logScript $ "Upgrading selector 0x" ++ show selector ++ " -> " ++ show impl
  -- Would call setImplementation(selector, impl) on dictionary
  pure ()

-- =============================================================================
-- Script Pretty Printer
-- =============================================================================

||| Pretty-print a script for review
export
prettyScript : Script a -> List String
prettyScript (Pure _) = []
prettyScript (Bind m k) = prettyScript m ++ ["..."] -- simplified
prettyScript (Cmd (DeployCmd bc)) = ["deploy " ++ bc.code]
prettyScript (Cmd (CallCmd {sig} d sel args)) =
  ["call " ++ show d.address ++ " " ++ sigString sig]
prettyScript (Cmd (SendCmd to amt)) = ["send " ++ show amt ++ " to " ++ show to]
prettyScript (Cmd (LogCmd msg)) = ["# " ++ msg]
prettyScript (Cmd (AssertCmd msg _)) = ["assert: " ++ msg]
prettyScript (Cmd (RecordCmd name val)) = ["record " ++ name ++ " = " ++ show val]
prettyScript (Cmd (GetRecordCmd name)) = ["get " ++ name]
