||| Subcontract Core: Pure Dry-Run Planner
|||
||| Maps a `Script` free monad (Subcontract.Core.Script) to the sequence of
||| transactions it WOULD send — without any IO, RPC, or signing. This is the
||| first stage of turning a script into transactable EVM transactions; it is
||| also the auditor-facing artifact of a release: reviewers approve exactly
||| the tx sequence this planner produces.
|||
||| The planner is deterministic and pure: `planScript` only inspects the AST
||| and never touches the network, so it is trivially testable and reviewable.
|||
|||   planScript (do
|||     dict <- deploy DictionaryBytecode
|||     proxy <- deploy ProxyBytecode
|||     send proxy.address 1
|||     assertScript "proxy up" (proxy.address /= 0))
|||   -- => txs   = [deploy to=Nothing, deploy to=Nothing, send to=Just proxy]
|||   --    logs  = []
|||   --    asserts = [("proxy up", True)]
module Subcontract.Core.ScriptDryRun

import Subcontract.Core.Script
import Subcontract.Core.ABI.Sig

-- =============================================================================
-- Planned Transactions
-- =============================================================================

||| A single transaction the script intends to send.
||| `to = Nothing` means contract creation (the data is initcode).
public export
record PlannedTx where
  constructor MkPlannedTx
  label : String
  to    : Maybe Integer
  value : Integer
  data_ : String

||| The full dry-run output of a script: the tx sequence plus the side
||| channels that carry no tx of their own.
public export
record DryRunPlan where
  constructor MkDryRunPlan
  txs    : List PlannedTx
  logs   : List String
  asserts : List (String, Bool)

-- =============================================================================
-- Hex Helpers
-- =============================================================================

hexDigit : Integer -> Char
hexDigit n = if n < 10 then chr (ord '0' + cast n) else chr (ord 'a' + cast (n - 10))

||| Minimal hex representation of an integer (no 0x prefix). Negatives are
||| encoded by their absolute value; byte-exact two's-complement encoding is
||| out of scope for a dry-run plan.
export
toHex : Integer -> String
toHex n = go (if n < 0 then negate n else n) ""
  where
    go : Integer -> String -> String
    go 0 acc = if acc == "" then "0" else acc
    go m acc = go (m `div` 16) (strCons (hexDigit (m `mod` 16)) acc)

||| Left-pad a string to exactly `width` characters with `c`.
export
padHex : (width : Nat) -> Char -> String -> String
padHex width c s =
  let extra = if length s >= width then 0 else width `minus` length s
  in pack (replicate extra c) ++ s

||| The 4-byte function selector as 8 hex chars (no 0x prefix).
export
selectorHex : Sel sig -> String
selectorHex sel = padHex 8 '0' (toHex (selValue sel))

||| One ABI static argument encoded as a 32-byte (64 hex char) word.
export
encodeArg : Integer -> String
encodeArg n = padHex 64 '0' (toHex n)

-- =============================================================================
-- Planning
-- =============================================================================

mergePlans : DryRunPlan -> DryRunPlan -> DryRunPlan
mergePlans (MkDryRunPlan t1 l1 a1) (MkDryRunPlan t2 l2 a2) =
  MkDryRunPlan (t1 ++ t2) (l1 ++ l2) (a1 ++ a2)

||| The side-effect of one command as a plan fragment.
|||
||| - `DeployCmd`  -> one contract-creation tx (to = Nothing, data = initcode)
||| - `CallCmd`    -> one tx (to = Just target, data = selector ++ ABI args)
||| - `SendCmd`    -> one tx (to = Just target, value = amount, data = "")
||| - `LogCmd`/`RecordCmd` -> appended to logs, no tx
||| - `AssertCmd`  -> appended to asserts, no tx
||| - `GetRecordCmd` -> no on-chain effect, no tx
planCmd : ScriptCmd a -> DryRunPlan
planCmd (DeployCmd bc) =
  MkDryRunPlan [MkPlannedTx "deploy" Nothing 0 bc.code] [] []
planCmd (CallCmd d sel args) =
  MkDryRunPlan [MkPlannedTx "call" (Just d.address) 0 (selectorHex sel ++ concatMap encodeArg args)] [] []
planCmd (SendCmd to amt) =
  MkDryRunPlan [MkPlannedTx "send" (Just to) amt ""] [] []
planCmd (LogCmd msg) =
  MkDryRunPlan [] [msg] []
planCmd (RecordCmd name val) =
  MkDryRunPlan [] [name ++ " = " ++ show val] []
planCmd (AssertCmd msg cond) =
  MkDryRunPlan [] [] [(msg, cond)]
planCmd (GetRecordCmd name) =
  MkDryRunPlan [] [] []

||| A command's result value as the continuation sees it. A pure plan cannot
||| know real on-chain results, so each value is the type-correct placeholder
||| (address 0, value 0, no record).
cmdResult : ScriptCmd a -> a
cmdResult (DeployCmd _) = MkDeployed 0 0
cmdResult (CallCmd _ _ _) = 0
cmdResult (SendCmd _ _) = ()
cmdResult (LogCmd _) = ()
cmdResult (AssertCmd _ _) = ()
cmdResult (RecordCmd _ _) = ()
cmdResult (GetRecordCmd _) = Nothing

||| Walk a script, carrying the placeholder value through each continuation so
||| that later commands are still planned. Pure — no IO, no RPC, no signing.
planScriptStep : Script a -> (DryRunPlan, a)
planScriptStep (Pure v) = (MkDryRunPlan [] [] [], v)
planScriptStep (Bind m k) =
  let (p, v) = planScriptStep m
      (q, w) = planScriptStep (k v)
  in (mergePlans p q, w)
planScriptStep (Cmd c) = (planCmd c, cmdResult c)

||| Plan the transactions a script would send.
|||
||| - `deploy bytecode`   -> one PlannedTx with `to = Nothing`, data = initcode
||| - `call target sel a` -> one PlannedTx with `to = Just target`, data =
|||   the 4-byte selector followed by the ABI args
||| - `send to amount`    -> one PlannedTx with `to = Just to`, value = amount
||| - `logScript` / `recordValue` -> appended to `logs`, no tx
||| - `assertScript`      -> appended to `asserts`, no tx
|||
||| Command order is preserved in `txs`.
export
planScript : Script a -> DryRunPlan
planScript s = fst (planScriptStep s)
