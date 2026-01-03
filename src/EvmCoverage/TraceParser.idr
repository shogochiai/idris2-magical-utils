||| EVM Trace Parser for idris2-evm-run --trace output
|||
||| Parses CSV trace output from idris2-evm-run to calculate opcode coverage.
||| Format: step,pc,opcode,name,stack_depth
module EvmCoverage.TraceParser

import Data.List
import Data.List1
import Data.String
import Data.Maybe
import System.File

%default covering

-- =============================================================================
-- Trace Entry Types
-- =============================================================================

||| A single trace entry from idris2-evm-run --trace
public export
record TraceEntry where
  constructor MkTraceEntry
  step : Nat
  pc : Nat
  opcode : Nat
  opcodeName : String
  stackDepth : Nat

public export
Show TraceEntry where
  show e = show e.step ++ "," ++ show e.pc ++ "," ++
           show e.opcode ++ "," ++ e.opcodeName ++ "," ++ show e.stackDepth

public export
Eq TraceEntry where
  e1 == e2 = e1.step == e2.step && e1.pc == e2.pc && e1.opcode == e2.opcode

-- =============================================================================
-- Opcode Coverage Types
-- =============================================================================

||| Coverage for a specific opcode
public export
record OpcodeCoverage where
  constructor MkOpcodeCoverage
  opcode : Nat
  opcodeName : String
  hitCount : Nat
  pcLocations : List Nat

public export
Show OpcodeCoverage where
  show oc = oc.opcodeName ++ ": " ++ show oc.hitCount ++ " hits at " ++
            show (length oc.pcLocations) ++ " locations"

||| Aggregated trace coverage
public export
record TraceCoverage where
  constructor MkTraceCoverage
  totalSteps : Nat
  uniqueOpcodes : Nat
  uniquePCs : Nat
  opcodeCoverage : List OpcodeCoverage

public export
Show TraceCoverage where
  show tc = "TraceCoverage(steps=" ++ show tc.totalSteps ++
            ", opcodes=" ++ show tc.uniqueOpcodes ++
            ", pcs=" ++ show tc.uniquePCs ++ ")"

-- =============================================================================
-- Parsing
-- =============================================================================

||| Parse a single CSV line into TraceEntry
export
parseTraceLine : String -> Maybe TraceEntry
parseTraceLine line =
  let parts = map trim $ forget $ split (== ',') line
  in case parts of
    [stepStr, pcStr, opcodeStr, name, depthStr] =>
      do step <- parsePositive stepStr
         pc <- parsePositive pcStr
         opcode <- parsePositive opcodeStr
         depth <- parsePositive depthStr
         pure $ MkTraceEntry step pc opcode name depth
    _ => Nothing

||| Parse entire trace CSV content
export
parseTraceCSV : String -> List TraceEntry
parseTraceCSV content =
  let ls = lines content
      dataLines = drop 1 ls  -- Skip header
  in mapMaybe parseTraceLine dataLines

-- =============================================================================
-- Coverage Calculation
-- =============================================================================

||| Insert or update opcode in coverage list
insertOpcode : TraceEntry -> List OpcodeCoverage -> List OpcodeCoverage
insertOpcode e [] = [MkOpcodeCoverage e.opcode e.opcodeName 1 [e.pc]]
insertOpcode e (oc :: rest) =
  if oc.opcode == e.opcode
    then { hitCount $= (+1), pcLocations $= (e.pc ::) } oc :: rest
    else oc :: insertOpcode e rest

||| Calculate unique PCs from trace
uniquePCCount : List TraceEntry -> Nat
uniquePCCount entries = length $ nub $ map (.pc) entries

||| Calculate trace coverage from entries
export
calculateTraceCoverage : List TraceEntry -> TraceCoverage
calculateTraceCoverage entries =
  let opcodeCov = foldr insertOpcode [] entries
      uniqueOps = length opcodeCov
      uniquePCs = uniquePCCount entries
  in MkTraceCoverage (length entries) uniqueOps uniquePCs opcodeCov

-- =============================================================================
-- Coverage Analysis
-- =============================================================================

||| All possible EVM opcodes (simplified set of important ones)
export
allOpcodes : List (Nat, String)
allOpcodes =
  [ (0x00, "STOP"), (0x01, "ADD"), (0x02, "MUL"), (0x03, "SUB"), (0x04, "DIV")
  , (0x05, "SDIV"), (0x06, "MOD"), (0x07, "SMOD"), (0x08, "ADDMOD"), (0x09, "MULMOD")
  , (0x0a, "EXP"), (0x0b, "SIGNEXTEND")
  , (0x10, "LT"), (0x11, "GT"), (0x12, "SLT"), (0x13, "SGT"), (0x14, "EQ")
  , (0x15, "ISZERO"), (0x16, "AND"), (0x17, "OR"), (0x18, "XOR"), (0x19, "NOT")
  , (0x1a, "BYTE"), (0x1b, "SHL"), (0x1c, "SHR"), (0x1d, "SAR")
  , (0x20, "KECCAK256")
  , (0x30, "ADDRESS"), (0x31, "BALANCE"), (0x32, "ORIGIN"), (0x33, "CALLER")
  , (0x34, "CALLVALUE"), (0x35, "CALLDATALOAD"), (0x36, "CALLDATASIZE")
  , (0x37, "CALLDATACOPY"), (0x38, "CODESIZE"), (0x39, "CODECOPY")
  , (0x3a, "GASPRICE"), (0x3b, "EXTCODESIZE"), (0x3c, "EXTCODECOPY")
  , (0x3d, "RETURNDATASIZE"), (0x3e, "RETURNDATACOPY"), (0x3f, "EXTCODEHASH")
  , (0x40, "BLOCKHASH"), (0x41, "COINBASE"), (0x42, "TIMESTAMP"), (0x43, "NUMBER")
  , (0x44, "DIFFICULTY"), (0x45, "GASLIMIT"), (0x46, "CHAINID"), (0x47, "SELFBALANCE")
  , (0x48, "BASEFEE")
  , (0x50, "POP"), (0x51, "MLOAD"), (0x52, "MSTORE"), (0x53, "MSTORE8")
  , (0x54, "SLOAD"), (0x55, "SSTORE"), (0x56, "JUMP"), (0x57, "JUMPI")
  , (0x58, "PC"), (0x59, "MSIZE"), (0x5a, "GAS"), (0x5b, "JUMPDEST")
  , (0x5f, "PUSH0")
  , (0x60, "PUSH1"), (0x61, "PUSH2"), (0x62, "PUSH3"), (0x63, "PUSH4")
  , (0x80, "DUP1"), (0x81, "DUP2"), (0x82, "DUP3"), (0x83, "DUP4")
  , (0x90, "SWAP1"), (0x91, "SWAP2"), (0x92, "SWAP3"), (0x93, "SWAP4")
  , (0xf0, "CREATE"), (0xf1, "CALL"), (0xf2, "CALLCODE"), (0xf3, "RETURN")
  , (0xf4, "DELEGATECALL"), (0xf5, "CREATE2"), (0xfa, "STATICCALL")
  , (0xfd, "REVERT"), (0xfe, "INVALID"), (0xff, "SELFDESTRUCT")
  ]

||| Get opcodes that were not executed
export
uncoveredOpcodes : TraceCoverage -> List (Nat, String)
uncoveredOpcodes tc =
  let coveredCodes = map (.opcode) tc.opcodeCoverage
  in filter (\(code, _) => not (elem code coveredCodes)) allOpcodes

||| Calculate opcode coverage percentage (covered / total important opcodes)
export
opcodeCoveragePercent : TraceCoverage -> Double
opcodeCoveragePercent tc =
  let totalOps = length allOpcodes
      coveredOps = tc.uniqueOpcodes
  in if totalOps == 0 then 100.0
     else cast coveredOps / cast totalOps * 100.0

-- =============================================================================
-- File I/O
-- =============================================================================

||| Read and parse trace file
export
readTraceFile : String -> IO (Either String (List TraceEntry))
readTraceFile path = do
  Right content <- readFile path
    | Left err => pure $ Left $ "Failed to read trace file: " ++ show err
  let entries = parseTraceCSV content
  pure $ Right entries

||| Analyze trace file and return coverage
export
analyzeTraceFile : String -> IO (Either String TraceCoverage)
analyzeTraceFile path = do
  Right entries <- readTraceFile path
    | Left err => pure $ Left err
  pure $ Right $ calculateTraceCoverage entries
