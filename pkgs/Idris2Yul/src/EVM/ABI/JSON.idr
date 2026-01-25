||| EVM ABI JSON Generator
|||
||| Converts ABI type definitions to Ethereum-compatible JSON format.
||| The output can be used with Etherscan, web3.js, ethers.js, etc.
|||
||| Example:
|||   json <- toABIJson myContractABI
|||   -- Produces: [{"type":"function","name":"transfer",...}]
|||
module EVM.ABI.JSON

import EVM.ABI.Types
import Data.List
import Data.String

-- =============================================================================
-- JSON String Helpers
-- =============================================================================

||| Escape a string for JSON
escapeJson : String -> String
escapeJson = pack . concatMap escapeChar . unpack
  where
    escapeChar : Char -> List Char
    escapeChar '"' = ['\\', '"']
    escapeChar '\\' = ['\\', '\\']
    escapeChar '\n' = ['\\', 'n']
    escapeChar '\r' = ['\\', 'r']
    escapeChar '\t' = ['\\', 't']
    escapeChar c = [c]

||| Create a JSON string value
jsonString : String -> String
jsonString s = "\"" ++ escapeJson s ++ "\""

||| Create a JSON boolean
jsonBool : Bool -> String
jsonBool True = "true"
jsonBool False = "false"

||| Create a JSON array
jsonArray : List String -> String
jsonArray xs = "[" ++ joinWith "," xs ++ "]"
  where
    joinWith : String -> List String -> String
    joinWith sep [] = ""
    joinWith sep [x] = x
    joinWith sep (x :: xs) = x ++ sep ++ joinWith sep xs

||| Create a JSON object from key-value pairs
jsonObject : List (String, String) -> String
jsonObject kvs = "{" ++ joinWith "," (map mkPair kvs) ++ "}"
  where
    mkPair : (String, String) -> String
    mkPair (k, v) = jsonString k ++ ":" ++ v

    joinWith : String -> List String -> String
    joinWith sep [] = ""
    joinWith sep [x] = x
    joinWith sep (x :: xs) = x ++ sep ++ joinWith sep xs

-- =============================================================================
-- ABI to JSON Conversion
-- =============================================================================

||| Convert a Param to JSON object
paramToJson : Param -> String
paramToJson p = jsonObject $
  [ ("name", jsonString p.paramName)
  , ("type", jsonString (abiTypeToString p.paramType))
  ] ++ if p.indexed then [("indexed", jsonBool True)] else []

||| Convert a Function to JSON object
export
functionToJson : Function -> String
functionToJson f = jsonObject
  [ ("type", jsonString "function")
  , ("name", jsonString f.funcName)
  , ("inputs", jsonArray (map paramToJson f.inputs))
  , ("outputs", jsonArray (map paramToJson f.outputs))
  , ("stateMutability", jsonString (mutabilityToString f.mutability))
  ]

||| Convert an Event to JSON object
export
eventToJson : Event -> String
eventToJson e = jsonObject $
  [ ("type", jsonString "event")
  , ("name", jsonString e.eventName)
  , ("inputs", jsonArray (map paramToJson e.params))
  ] ++ if e.anonymous then [("anonymous", jsonBool True)] else []

||| Convert an Error to JSON object
export
errorToJson : Error -> String
errorToJson e = jsonObject
  [ ("type", jsonString "error")
  , ("name", jsonString e.errorName)
  , ("inputs", jsonArray (map paramToJson e.params))
  ]

||| Convert a complete ContractABI to JSON array
export
toABIJson : ContractABI -> String
toABIJson abi = jsonArray $
  map functionToJson abi.functions ++
  map eventToJson abi.events ++
  map errorToJson abi.errors

||| Pretty-print ABI JSON with indentation
export
toABIJsonPretty : ContractABI -> String
toABIJsonPretty abi = "[\n" ++ entries ++ "\n]"
  where
    joinWith' : String -> List String -> String
    joinWith' sep [] = ""
    joinWith' sep [x] = x
    joinWith' sep (x :: xs) = x ++ sep ++ joinWith' sep xs

    indent : String -> String
    indent s = "  " ++ s

    allItems : List String
    allItems = map functionToJson abi.functions ++
               map eventToJson abi.events ++
               map errorToJson abi.errors

    entries : String
    entries = joinWith' ",\n" (map indent allItems)

-- =============================================================================
-- Solidity Interface Generation
-- =============================================================================

||| Generate Solidity function signature
funcToSolidity : Function -> String
funcToSolidity f =
  "function " ++ f.funcName ++ "(" ++ inputParams ++ ") external"
    ++ mutMod ++ outputPart ++ ";"
  where
    joinWith' : String -> List String -> String
    joinWith' sep [] = ""
    joinWith' sep [x] = x
    joinWith' sep (x :: xs) = x ++ sep ++ joinWith' sep xs

    paramSol : Param -> String
    paramSol p = abiTypeToString p.paramType ++
      (if p.paramName == "" then "" else " " ++ p.paramName)

    inputParams : String
    inputParams = joinWith' ", " (map paramSol f.inputs)

    mutMod : String
    mutMod = case f.mutability of
      Pure => " pure"
      View => " view"
      Payable => " payable"
      Nonpayable => ""

    outputPart : String
    outputPart = case f.outputs of
      [] => ""
      outs => " returns (" ++ joinWith' ", " (map paramSol outs) ++ ")"

||| Generate Solidity event signature
eventToSolidity : Event -> String
eventToSolidity e =
  "event " ++ e.eventName ++ "(" ++ params ++ ");"
  where
    joinWith' : String -> List String -> String
    joinWith' sep [] = ""
    joinWith' sep [x] = x
    joinWith' sep (x :: xs) = x ++ sep ++ joinWith' sep xs

    paramSol : Param -> String
    paramSol p = abiTypeToString p.paramType ++
      (if p.indexed then " indexed" else "") ++
      (if p.paramName == "" then "" else " " ++ p.paramName)

    params : String
    params = joinWith' ", " (map paramSol e.params)

||| Generate Solidity error signature
errorToSolidity : Error -> String
errorToSolidity e =
  "error " ++ e.errorName ++ "(" ++ params ++ ");"
  where
    joinWith' : String -> List String -> String
    joinWith' sep [] = ""
    joinWith' sep [x] = x
    joinWith' sep (x :: xs) = x ++ sep ++ joinWith' sep xs

    paramSol : Param -> String
    paramSol p = abiTypeToString p.paramType ++
      (if p.paramName == "" then "" else " " ++ p.paramName)

    params : String
    params = joinWith' ", " (map paramSol e.params)

||| Generate complete Solidity interface
export
toSolidityInterface : ContractABI -> String
toSolidityInterface abi = unlines' header ++ body ++ "}"
  where
    unlines' : List String -> String
    unlines' [] = ""
    unlines' (x :: xs) = x ++ "\n" ++ unlines' xs

    indent : String -> String
    indent s = "    " ++ s

    header : List String
    header = [ "// SPDX-License-Identifier: MIT"
             , "pragma solidity ^0.8.0;"
             , ""
             , "interface I" ++ abi.contractName ++ " {"
             ]

    body : String
    body = unlines' $
      map (indent . funcToSolidity) abi.functions ++
      (if null abi.events then [] else [""] ++ map (indent . eventToSolidity) abi.events) ++
      (if null abi.errors then [] else [""] ++ map (indent . errorToSolidity) abi.errors)
