||| Subcontract Facade: one verified Solidity contract for a Dictionary-dispersed
||| ERC-7546 call surface.
|||
||| ERC-7546 dispatches selectors from a Dictionary, so an explorer cannot show
||| the proxy's call surface. `facadeSolidity` bundles every `FacadeFn` into a
||| single verified Solidity contract source that an explorer can display: the
||| proxy's external functions, with their Solidity types and mutability.
module Subcontract.Standards.ERC7546.Facade

import public Subcontract.Core.ABI.Sig

%default total

-- =============================================================================
-- Facade model
-- =============================================================================

||| Solidity function mutability.
public export
data StateMutability = View | Nonpayable | Payable

||| One external function of the facade: its ABI signature and mutability.
public export
record FacadeFn where
  constructor MkFacadeFn
  sig : Sig
  mut : StateMutability

-- =============================================================================
-- Rendering helpers
-- =============================================================================

joinWith : String -> List String -> String
joinWith sep [] = ""
joinWith sep [x] = x
joinWith sep (x :: xs) = x ++ sep ++ joinWith sep xs

||| Solidity visibility + mutability keyword for a StateMutability.
mutabilityKw : StateMutability -> String
mutabilityKw View = " external view"
mutabilityKw Nonpayable = " external"
mutabilityKw Payable = " external payable"

||| Solidity returns clause for a list of return types.
||| Empty list renders nothing; one type renders ` returns (t)`;
||| several types render ` returns (t1, t2, ...)` in order.
returnsClause : List ABIStaticType -> String
returnsClause [] = ""
returnsClause [t] = " returns (" ++ abiTypeStr t ++ ")"
returnsClause (t :: t2 :: rest) =
  " returns (" ++ joinWith "," (map abiTypeStr (t :: t2 :: rest)) ++ ")"

||| Render one FacadeFn as a single `function` declaration line with a
||| types-only argument list, the appropriate mutability keyword, a returns
||| clause matching the rets arity, and an empty body.
fnLine : FacadeFn -> String
fnLine (MkFacadeFn (MkSig n args rets) mut) =
  "    function " ++ n ++ "(" ++ joinWith "," (map abiTypeStr args) ++ ")"
  ++ mutabilityKw mut
  ++ returnsClause rets
  ++ " {}\n"

-- =============================================================================
-- Facade generation
-- =============================================================================

||| Render a complete, verified single-contract Solidity source for the facade.
|||
||| The output begins with an SPDX license identifier line, declares
||| `pragma solidity ^0.8.20;`, opens `contract <name> {`, emits one
||| `function` declaration per `FacadeFn` (types-only arguments, mutability
||| keyword per `mut`, returns clause per `rets` arity, empty body), and closes
||| the contract block.
public export
facadeSolidity : String -> List FacadeFn -> String
facadeSolidity name fns =
  "// SPDX-License-Identifier: MIT\n"
  ++ "pragma solidity ^0.8.20;\n"
  ++ "\n"
  ++ "contract " ++ name ++ " {\n"
  ++ concatMap fnLine fns
  ++ "}\n"
