||| Subcontract Facade: one verified Solidity contract for a Dictionary-dispersed
||| ERC-7546 call surface.
|||
||| ERC-7546 dispatches selectors from a Dictionary, so an explorer cannot show
||| the proxy's call surface. `facadeSolidity` bundles every `FacadeFn` into a
||| single verified Solidity contract source that an explorer can display: the
||| proxy's external functions, with their Solidity types and mutability.
module Subcontract.Standards.ERC7546.Facade

import public Subcontract.Core.ABI.Sig
import public Subcontract.Core.ABI.Keccak

import Data.String

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

-- =============================================================================
-- Content addressing
-- =============================================================================

||| Canonical mutability token for a StateMutability, as it appears in the
||| canonical function line.
canonicalMut : StateMutability -> String
canonicalMut View = "view"
canonicalMut Nonpayable = "nonpayable"
canonicalMut Payable = "payable"

||| One canonical line per FacadeFn: `name(argTypes)->(retTypes):mutability`.
|||
||| The argument and return types are the canonical Solidity type strings
||| (comma-joined), and the mutability is the canonical token. Return types
||| and mutability are deliberately part of the line, so two facades that
||| differ only in what they return or how they mutate hash differently.
canonicalLine : FacadeFn -> String
canonicalLine (MkFacadeFn (MkSig n args rets) mut) =
  n ++ "(" ++ joinWith "," (map abiTypeStr args) ++ ")"
  ++ "->"
  ++ "(" ++ joinWith "," (map abiTypeStr rets) ++ ")"
  ++ ":" ++ canonicalMut mut

||| Canonical, order-insensitive rendering of a facade's signature set.
|||
||| Each `FacadeFn` is rendered as a single `name(argTypes)->(retTypes):mutability`
||| line; the lines are sorted ascending and joined with newlines. Two facades
||| with the same signature set in a different order canonicalize to the same
||| string.
public export
facadeCanonical : List FacadeFn -> String
facadeCanonical fns = joinWith "\n" (sort (map canonicalLine fns))

||| Content hash of a facade's signature set.
|||
||| The lowercase-hex Keccak256 of `facadeCanonical`, so a facade is
||| content-addressed by its own signature set: same set (any order) hashes
||| the same, and any change to a name, argument type, return type, or
||| mutability changes the hash.
public export
facadeShapeHash : List FacadeFn -> String
facadeShapeHash = keccak256Hex . facadeCanonical

||| Encode an Integer as exactly n big-endian bytes.
toBytesN : Nat -> Integer -> List Bits8
toBytesN Z _ = []
toBytesN (S k) m = toBytesN k (m `div` 256) ++ [cast (m `mod` 256)]

||| Value of a hex character, or 0 for anything else.
hexVal : Char -> Integer
hexVal c =
  if ord c >= ord '0' && ord c <= ord '9' then cast (ord c - ord '0')
  else if ord c >= ord 'a' && ord c <= ord 'f' then cast (ord c - ord 'a' + 10)
  else if ord c >= ord 'A' && ord c <= ord 'F' then cast (ord c - ord 'A' + 10)
  else 0

||| Decode a hex string (optionally 0x-prefixed, either case) into bytes.
hexDecode : String -> List Bits8
hexDecode s = go (unpack (trimPrefix s))
  where
    trimPrefix : String -> String
    trimPrefix t = if isPrefixOf "0x" t || isPrefixOf "0X" t then substr 2 (length t) t else t

    go : List Char -> List Bits8
    go [] = []
    go (a :: b :: rest) = cast (hexVal a * 16 + hexVal b) :: go rest
    go [a] = [cast (hexVal a * 16)]

||| Decode a hex string and left-pad to exactly 32 bytes (bytes32 semantics).
hexToBytes32 : String -> List Bits8
hexToBytes32 s =
  let bs = hexDecode s
      pad = 32 `minus` length bs
  in if pad == 0 then bs else replicate pad 0 ++ bs

||| Interpret a byte list as a big-endian Integer.
bytesToInteger : List Bits8 -> Integer
bytesToInteger bs = foldl (\acc, b => acc * 256 + cast {to=Integer} b) 0 bs

||| Deterministic CREATE2 address of a facade.
|||
||| Per EIP-1014 the created address is the low 20 bytes of
||| `keccak256(0xff ++ factory ++ salt ++ initCodeHash)`; `factory` is encoded
||| as 20 big-endian bytes, `salt` and `initCodeHash` as bytes32 values
||| (hex-encoded, left-padded). Because the salt and the init code hash are
||| given, the address is a pure function of the source: a facade that is
||| already deployed and verified at this address needs no further action.
public export
facadeCreate2Address : (factory : Integer) -> (salt : String) -> (initCodeHash : String) -> Integer
facadeCreate2Address factory salt initCodeHash =
  bytesToInteger (drop 12 (keccak256Bytes (0xff :: (toBytesN 20 factory ++ hexToBytes32 salt ++ hexToBytes32 initCodeHash))))
