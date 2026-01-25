||| EVM Opcode Compatibility Tests
||| Tests opcode availability by hardfork version
module Compiler.EVM.Tests.AllTests

import Data.List
import Data.String
import Data.SortedSet
import Data.Vect
import System
import System.File

import Compiler.EVM.YulIR
import EVM.ABI.Types
import EVM.Storage.Storable
import EVM.Storage.Namespace

%default covering

-- =============================================================================
-- REQ_EVM_*: EVM Opcode Compatibility Tests (Convention: DocCommentReq)
-- =============================================================================

||| REQ_EVM_000: Module loads correctly
export
test_REQ_EVM_000_moduleLoads : IO Bool
test_REQ_EVM_000_moduleLoads = pure True

-- =============================================================================
-- Opcode Definitions by Hardfork
-- =============================================================================

-- =============================================================================
-- Stop and Arithmetic Operations (0x00-0x0B)
-- =============================================================================

||| STOP: Halts execution (0x00)
export
STOP : String
STOP = "00"

||| ADD: Addition operation (0x01)
export
ADD : String
ADD = "01"

||| MUL: Multiplication operation (0x02)
export
MUL : String
MUL = "02"

||| SUB: Subtraction operation (0x03)
export
SUB : String
SUB = "03"

||| DIV: Integer division operation (0x04)
export
DIV : String
DIV = "04"

||| SDIV: Signed integer division (0x05)
export
SDIV : String
SDIV = "05"

||| MOD: Modulo remainder operation (0x06)
export
MOD : String
MOD = "06"

||| SMOD: Signed modulo remainder (0x07)
export
SMOD : String
SMOD = "07"

||| ADDMOD: Modulo addition (0x08)
export
ADDMOD : String
ADDMOD = "08"

||| MULMOD: Modulo multiplication (0x09)
export
MULMOD : String
MULMOD = "09"

||| EXP: Exponential operation (0x0a)
export
EXP : String
EXP = "0a"

||| SIGNEXTEND: Extend length of signed integer (0x0b)
export
SIGNEXTEND : String
SIGNEXTEND = "0b"

-- =============================================================================
-- Comparison & Bitwise Logic Operations (0x10-0x1D)
-- =============================================================================

||| LT: Less-than comparison (0x10)
export
OP_LT : String
OP_LT = "10"

||| GT: Greater-than comparison (0x11)
export
OP_GT : String
OP_GT = "11"

||| SLT: Signed less-than comparison (0x12)
export
SLT : String
SLT = "12"

||| SGT: Signed greater-than comparison (0x13)
export
SGT : String
SGT = "13"

||| EQ: Equality comparison (0x14)
export
OP_EQ : String
OP_EQ = "14"

||| ISZERO: Simple not operator (0x15)
export
ISZERO : String
ISZERO = "15"

||| AND: Bitwise AND operation (0x16)
export
AND : String
AND = "16"

||| OR: Bitwise OR operation (0x17)
export
OR : String
OR = "17"

||| XOR: Bitwise XOR operation (0x18)
export
XOR : String
XOR = "18"

||| NOT: Bitwise NOT operation (0x19)
export
NOT : String
NOT = "19"

||| BYTE: Retrieve single byte from word (0x1a)
export
BYTE : String
BYTE = "1a"

||| SHL: Shift left (0x1b)
export
SHL : String
SHL = "1b"

||| SHR: Logical shift right (0x1c)
export
SHR : String
SHR = "1c"

||| SAR: Arithmetic shift right (0x1d)
export
SAR : String
SAR = "1d"

-- =============================================================================
-- SHA3 (0x20)
-- =============================================================================

||| KECCAK256: Compute Keccak-256 hash (0x20)
export
KECCAK256 : String
KECCAK256 = "20"

-- =============================================================================
-- Environmental Information (0x30-0x3F)
-- =============================================================================

||| ADDRESS: Get address of currently executing account (0x30)
export
ADDRESS : String
ADDRESS = "30"

||| BALANCE: Get balance of the given account (0x31)
export
BALANCE : String
BALANCE = "31"

||| ORIGIN: Get execution origination address (0x32)
export
ORIGIN : String
ORIGIN = "32"

||| CALLER: Get caller address (0x33)
export
CALLER : String
CALLER = "33"

||| CALLVALUE: Get deposited value by the instruction/transaction (0x34)
export
CALLVALUE : String
CALLVALUE = "34"

||| CALLDATALOAD: Get input data of current environment (0x35)
export
CALLDATALOAD : String
CALLDATALOAD = "35"

||| CALLDATASIZE: Get size of input data (0x36)
export
CALLDATASIZE : String
CALLDATASIZE = "36"

||| CALLDATACOPY: Copy input data to memory (0x37)
export
CALLDATACOPY : String
CALLDATACOPY = "37"

||| CODESIZE: Get size of code running in current environment (0x38)
export
CODESIZE : String
CODESIZE = "38"

||| CODECOPY: Copy code running to memory (0x39)
export
CODECOPY : String
CODECOPY = "39"

||| GASPRICE: Get price of gas in current environment (0x3a)
export
GASPRICE : String
GASPRICE = "3a"

||| EXTCODESIZE: Get size of an account's code (0x3b)
export
EXTCODESIZE : String
EXTCODESIZE = "3b"

||| EXTCODECOPY: Copy an account's code to memory (0x3c)
export
EXTCODECOPY : String
EXTCODECOPY = "3c"

||| RETURNDATASIZE: Get size of output data from the previous call (0x3d)
export
RETURNDATASIZE : String
RETURNDATASIZE = "3d"

||| RETURNDATACOPY: Copy output data from the previous call to memory (0x3e)
export
RETURNDATACOPY : String
RETURNDATACOPY = "3e"

||| EXTCODEHASH: Get hash of an account's code (0x3f)
export
EXTCODEHASH : String
EXTCODEHASH = "3f"

-- =============================================================================
-- Block Information (0x40-0x4A)
-- =============================================================================

||| BLOCKHASH: Get hash of most recent complete block (0x40)
export
BLOCKHASH : String
BLOCKHASH = "40"

||| COINBASE: Get the block's beneficiary address (0x41)
export
COINBASE : String
COINBASE = "41"

||| TIMESTAMP: Get the block's timestamp (0x42)
export
TIMESTAMP : String
TIMESTAMP = "42"

||| NUMBER: Get the block's number (0x43)
export
NUMBER : String
NUMBER = "43"

||| PREVRANDAO: Get previous block's RANDAO mix (0x44) - was DIFFICULTY pre-Paris
export
PREVRANDAO : String
PREVRANDAO = "44"

||| GASLIMIT: Get the block's gas limit (0x45)
export
GASLIMIT : String
GASLIMIT = "45"

||| CHAINID: Get the chain ID (0x46)
export
CHAINID : String
CHAINID = "46"

||| SELFBALANCE: Get balance of currently executing account (0x47)
export
SELFBALANCE : String
SELFBALANCE = "47"

||| BASEFEE: Get the base fee (0x48)
export
BASEFEE : String
BASEFEE = "48"

||| BLOBHASH: Get versioned hash at index (0x49) - Cancun+
export
BLOBHASH : String
BLOBHASH = "49"

||| BLOBBASEFEE: Get current blob base fee (0x4a) - Cancun+
export
BLOBBASEFEE : String
BLOBBASEFEE = "4a"

-- =============================================================================
-- Stack, Memory, Storage and Flow Operations (0x50-0x5F)
-- =============================================================================

||| POP: Remove item from stack (0x50)
export
POP : String
POP = "50"

||| MLOAD: Load word from memory (0x51)
export
MLOAD : String
MLOAD = "51"

||| MSTORE: Save word to memory (0x52)
export
MSTORE : String
MSTORE = "52"

||| MSTORE8: Save byte to memory (0x53)
export
MSTORE8 : String
MSTORE8 = "53"

||| SLOAD: Load word from storage (0x54)
export
SLOAD : String
SLOAD = "54"

||| SSTORE: Save word to storage (0x55)
export
SSTORE : String
SSTORE = "55"

||| JUMP: Alter the program counter (0x56)
export
JUMP : String
JUMP = "56"

||| JUMPI: Conditionally alter the program counter (0x57)
export
JUMPI : String
JUMPI = "57"

||| PC: Get the value of the program counter (0x58)
export
PC : String
PC = "58"

||| MSIZE: Get the size of active memory in bytes (0x59)
export
MSIZE : String
MSIZE = "59"

||| GAS: Get the amount of available gas (0x5a)
export
GAS : String
GAS = "5a"

||| JUMPDEST: Mark a valid destination for jumps (0x5b)
export
JUMPDEST : String
JUMPDEST = "5b"

||| TLOAD: Load word from transient storage (0x5c) - Cancun+
export
TLOAD : String
TLOAD = "5c"

||| TSTORE: Save word to transient storage (0x5d) - Cancun+
export
TSTORE : String
TSTORE = "5d"

||| MCOPY: Copy memory areas (0x5e) - Cancun+
export
MCOPY : String
MCOPY = "5e"

||| PUSH0: Place value 0 on stack (0x5f) - Shanghai+
export
PUSH0 : String
PUSH0 = "5f"

-- =============================================================================
-- Push Operations (0x60-0x7F)
-- =============================================================================

||| PUSH1: Place 1-byte item on stack (0x60)
export
PUSH1 : String
PUSH1 = "60"

||| PUSH2: Place 2-byte item on stack (0x61)
export
PUSH2 : String
PUSH2 = "61"

||| PUSH3: Place 3-byte item on stack (0x62)
export
PUSH3 : String
PUSH3 = "62"

||| PUSH4: Place 4-byte item on stack (0x63)
export
PUSH4 : String
PUSH4 = "63"

||| PUSH5: Place 5-byte item on stack (0x64)
export
PUSH5 : String
PUSH5 = "64"

||| PUSH6: Place 6-byte item on stack (0x65)
export
PUSH6 : String
PUSH6 = "65"

||| PUSH7: Place 7-byte item on stack (0x66)
export
PUSH7 : String
PUSH7 = "66"

||| PUSH8: Place 8-byte item on stack (0x67)
export
PUSH8 : String
PUSH8 = "67"

||| PUSH9: Place 9-byte item on stack (0x68)
export
PUSH9 : String
PUSH9 = "68"

||| PUSH10: Place 10-byte item on stack (0x69)
export
PUSH10 : String
PUSH10 = "69"

||| PUSH11: Place 11-byte item on stack (0x6a)
export
PUSH11 : String
PUSH11 = "6a"

||| PUSH12: Place 12-byte item on stack (0x6b)
export
PUSH12 : String
PUSH12 = "6b"

||| PUSH13: Place 13-byte item on stack (0x6c)
export
PUSH13 : String
PUSH13 = "6c"

||| PUSH14: Place 14-byte item on stack (0x6d)
export
PUSH14 : String
PUSH14 = "6d"

||| PUSH15: Place 15-byte item on stack (0x6e)
export
PUSH15 : String
PUSH15 = "6e"

||| PUSH16: Place 16-byte item on stack (0x6f)
export
PUSH16 : String
PUSH16 = "6f"

||| PUSH17: Place 17-byte item on stack (0x70)
export
PUSH17 : String
PUSH17 = "70"

||| PUSH18: Place 18-byte item on stack (0x71)
export
PUSH18 : String
PUSH18 = "71"

||| PUSH19: Place 19-byte item on stack (0x72)
export
PUSH19 : String
PUSH19 = "72"

||| PUSH20: Place 20-byte item on stack (0x73)
export
PUSH20 : String
PUSH20 = "73"

||| PUSH21: Place 21-byte item on stack (0x74)
export
PUSH21 : String
PUSH21 = "74"

||| PUSH22: Place 22-byte item on stack (0x75)
export
PUSH22 : String
PUSH22 = "75"

||| PUSH23: Place 23-byte item on stack (0x76)
export
PUSH23 : String
PUSH23 = "76"

||| PUSH24: Place 24-byte item on stack (0x77)
export
PUSH24 : String
PUSH24 = "77"

||| PUSH25: Place 25-byte item on stack (0x78)
export
PUSH25 : String
PUSH25 = "78"

||| PUSH26: Place 26-byte item on stack (0x79)
export
PUSH26 : String
PUSH26 = "79"

||| PUSH27: Place 27-byte item on stack (0x7a)
export
PUSH27 : String
PUSH27 = "7a"

||| PUSH28: Place 28-byte item on stack (0x7b)
export
PUSH28 : String
PUSH28 = "7b"

||| PUSH29: Place 29-byte item on stack (0x7c)
export
PUSH29 : String
PUSH29 = "7c"

||| PUSH30: Place 30-byte item on stack (0x7d)
export
PUSH30 : String
PUSH30 = "7d"

||| PUSH31: Place 31-byte item on stack (0x7e)
export
PUSH31 : String
PUSH31 = "7e"

||| PUSH32: Place 32-byte item on stack (0x7f)
export
PUSH32 : String
PUSH32 = "7f"

-- =============================================================================
-- Duplication Operations (0x80-0x8F)
-- =============================================================================

||| DUP1: Duplicate 1st stack item (0x80)
export
DUP1 : String
DUP1 = "80"

||| DUP2: Duplicate 2nd stack item (0x81)
export
DUP2 : String
DUP2 = "81"

||| DUP3: Duplicate 3rd stack item (0x82)
export
DUP3 : String
DUP3 = "82"

||| DUP4: Duplicate 4th stack item (0x83)
export
DUP4 : String
DUP4 = "83"

||| DUP5: Duplicate 5th stack item (0x84)
export
DUP5 : String
DUP5 = "84"

||| DUP6: Duplicate 6th stack item (0x85)
export
DUP6 : String
DUP6 = "85"

||| DUP7: Duplicate 7th stack item (0x86)
export
DUP7 : String
DUP7 = "86"

||| DUP8: Duplicate 8th stack item (0x87)
export
DUP8 : String
DUP8 = "87"

||| DUP9: Duplicate 9th stack item (0x88)
export
DUP9 : String
DUP9 = "88"

||| DUP10: Duplicate 10th stack item (0x89)
export
DUP10 : String
DUP10 = "89"

||| DUP11: Duplicate 11th stack item (0x8a)
export
DUP11 : String
DUP11 = "8a"

||| DUP12: Duplicate 12th stack item (0x8b)
export
DUP12 : String
DUP12 = "8b"

||| DUP13: Duplicate 13th stack item (0x8c)
export
DUP13 : String
DUP13 = "8c"

||| DUP14: Duplicate 14th stack item (0x8d)
export
DUP14 : String
DUP14 = "8d"

||| DUP15: Duplicate 15th stack item (0x8e)
export
DUP15 : String
DUP15 = "8e"

||| DUP16: Duplicate 16th stack item (0x8f)
export
DUP16 : String
DUP16 = "8f"

-- =============================================================================
-- Exchange Operations (0x90-0x9F)
-- =============================================================================

||| SWAP1: Exchange 1st and 2nd stack items (0x90)
export
SWAP1 : String
SWAP1 = "90"

||| SWAP2: Exchange 1st and 3rd stack items (0x91)
export
SWAP2 : String
SWAP2 = "91"

||| SWAP3: Exchange 1st and 4th stack items (0x92)
export
SWAP3 : String
SWAP3 = "92"

||| SWAP4: Exchange 1st and 5th stack items (0x93)
export
SWAP4 : String
SWAP4 = "93"

||| SWAP5: Exchange 1st and 6th stack items (0x94)
export
SWAP5 : String
SWAP5 = "94"

||| SWAP6: Exchange 1st and 7th stack items (0x95)
export
SWAP6 : String
SWAP6 = "95"

||| SWAP7: Exchange 1st and 8th stack items (0x96)
export
SWAP7 : String
SWAP7 = "96"

||| SWAP8: Exchange 1st and 9th stack items (0x97)
export
SWAP8 : String
SWAP8 = "97"

||| SWAP9: Exchange 1st and 10th stack items (0x98)
export
SWAP9 : String
SWAP9 = "98"

||| SWAP10: Exchange 1st and 11th stack items (0x99)
export
SWAP10 : String
SWAP10 = "99"

||| SWAP11: Exchange 1st and 12th stack items (0x9a)
export
SWAP11 : String
SWAP11 = "9a"

||| SWAP12: Exchange 1st and 13th stack items (0x9b)
export
SWAP12 : String
SWAP12 = "9b"

||| SWAP13: Exchange 1st and 14th stack items (0x9c)
export
SWAP13 : String
SWAP13 = "9c"

||| SWAP14: Exchange 1st and 15th stack items (0x9d)
export
SWAP14 : String
SWAP14 = "9d"

||| SWAP15: Exchange 1st and 16th stack items (0x9e)
export
SWAP15 : String
SWAP15 = "9e"

||| SWAP16: Exchange 1st and 17th stack items (0x9f)
export
SWAP16 : String
SWAP16 = "9f"

-- =============================================================================
-- Logging Operations (0xA0-0xA4)
-- =============================================================================

||| LOG0: Append log record with no topics (0xa0)
export
LOG0 : String
LOG0 = "a0"

||| LOG1: Append log record with 1 topic (0xa1)
export
LOG1 : String
LOG1 = "a1"

||| LOG2: Append log record with 2 topics (0xa2)
export
LOG2 : String
LOG2 = "a2"

||| LOG3: Append log record with 3 topics (0xa3)
export
LOG3 : String
LOG3 = "a3"

||| LOG4: Append log record with 4 topics (0xa4)
export
LOG4 : String
LOG4 = "a4"

-- =============================================================================
-- System Operations (0xF0-0xFF)
-- =============================================================================

||| CREATE: Create a new account with associated code (0xf0)
export
CREATE : String
CREATE = "f0"

||| CALL: Message-call into an account (0xf1)
export
CALL : String
CALL = "f1"

||| CALLCODE: Message-call with alternative account's code (0xf2)
export
CALLCODE : String
CALLCODE = "f2"

||| RETURN: Halt execution returning output data (0xf3)
export
RETURN : String
RETURN = "f3"

||| DELEGATECALL: Message-call with caller's context (0xf4)
export
DELEGATECALL : String
DELEGATECALL = "f4"

||| CREATE2: Create with deterministic address (0xf5)
export
CREATE2 : String
CREATE2 = "f5"

||| STATICCALL: Static message-call (0xfa)
export
STATICCALL : String
STATICCALL = "fa"

||| REVERT: Halt and revert state changes (0xfd)
export
REVERT : String
REVERT = "fd"

||| INVALID: Designated invalid instruction (0xfe)
export
INVALID : String
INVALID = "fe"

||| SELFDESTRUCT: Halt and register account for deletion (0xff)
export
SELFDESTRUCT : String
SELFDESTRUCT = "ff"

-- =============================================================================
-- EVM Version Types
-- =============================================================================

||| EVM versions in order of release
public export
data EVMVersion = London | Paris | Shanghai | Cancun | Prague | Osaka

export
Show EVMVersion where
  show London = "london"
  show Paris = "paris"
  show Shanghai = "shanghai"
  show Cancun = "cancun"
  show Prague = "prague"
  show Osaka = "osaka"

||| List of all supported EVM versions
export
allVersions : List EVMVersion
allVersions = [London, Paris, Shanghai, Cancun, Prague, Osaka]

-- =============================================================================
-- Bytecode Analysis
-- =============================================================================

||| Check if bytecode contains a specific opcode (hex string)
||| Note: Simple substring check - may have false positives for data
export
containsOpcode : (opcode : String) -> (bytecode : String) -> Bool
containsOpcode opcode bytecode = isInfixOf opcode (toLower bytecode)

||| Check if an opcode is available in a given EVM version
export
opcodeAvailable : EVMVersion -> String -> Bool
opcodeAvailable London _ = False  -- PUSH0 not available
opcodeAvailable Paris _ = False
opcodeAvailable Shanghai op = op == PUSH0
opcodeAvailable Cancun op = op == PUSH0 || op == TLOAD || op == TSTORE || op == MCOPY
opcodeAvailable Prague op = op == PUSH0 || op == TLOAD || op == TSTORE || op == MCOPY
opcodeAvailable Osaka op = op == PUSH0 || op == TLOAD || op == TSTORE || op == MCOPY

-- =============================================================================
-- REQ_EVM_*: EVM Compatibility Tests
-- =============================================================================

||| REQ_EVM_001: PUSH0 opcode defined correctly
export
test_REQ_EVM_001_push0Defined : IO Bool
test_REQ_EVM_001_push0Defined = do
  -- P: PUSH0 opcode hex value
  let push0Hex = PUSH0
  -- R: PUSH0 SHALL be defined as 0x5f
  let correct = push0Hex == "5f"
  -- O: PUSH0 is correctly defined
  pure correct

||| REQ_EVM_002: Cancun opcodes defined
export
test_REQ_EVM_002_cancunOpcodes : IO Bool
test_REQ_EVM_002_cancunOpcodes = do
  -- P: Cancun-specific opcodes
  let tload = TLOAD
      tstore = TSTORE
      mcopy = MCOPY
  -- R: Cancun opcodes SHALL be TLOAD=0x5c, TSTORE=0x5d, MCOPY=0x5e
  let correct = tload == "5c" && tstore == "5d" && mcopy == "5e"
  -- O: Cancun opcodes are correctly defined
  pure correct

||| REQ_EVM_003: PUSH0 not available in London
export
test_REQ_EVM_003_londonNoPush0 : IO Bool
test_REQ_EVM_003_londonNoPush0 = do
  -- P: Check London EVM version
  let available = opcodeAvailable London PUSH0
  -- R: PUSH0 SHALL NOT be available in London
  let correct = not available
  -- O: London correctly reports no PUSH0
  pure correct

||| REQ_EVM_004: PUSH0 available in Shanghai+
export
test_REQ_EVM_004_shanghaiHasPush0 : IO Bool
test_REQ_EVM_004_shanghaiHasPush0 = do
  -- P: Check Shanghai and later versions
  let shanghai = opcodeAvailable Shanghai PUSH0
      cancun = opcodeAvailable Cancun PUSH0
      prague = opcodeAvailable Prague PUSH0
      osaka = opcodeAvailable Osaka PUSH0
  -- R: PUSH0 SHALL be available in Shanghai and later
  let correct = shanghai && cancun && prague && osaka
  -- O: All post-Shanghai versions have PUSH0
  pure correct

||| REQ_EVM_005: Transient storage in Cancun+
export
test_REQ_EVM_005_cancunTransientStorage : IO Bool
test_REQ_EVM_005_cancunTransientStorage = do
  -- P: Check TLOAD/TSTORE availability
  let cancunTload = opcodeAvailable Cancun TLOAD
      cancunTstore = opcodeAvailable Cancun TSTORE
      shanghaiTload = opcodeAvailable Shanghai TLOAD
  -- R: TLOAD/TSTORE SHALL be in Cancun+ only
  let correct = cancunTload && cancunTstore && not shanghaiTload
  -- O: Transient storage correctly gated to Cancun+
  pure correct

||| REQ_EVM_006: All versions enumerated
export
test_REQ_EVM_006_allVersions : IO Bool
test_REQ_EVM_006_allVersions = do
  -- P: List of all EVM versions
  let versions = allVersions
  -- R: SHALL include all supported hardforks
  let correct = length versions == 6
  -- O: All 6 hardfork versions are enumerated
  pure correct

-- =============================================================================
-- REQ_EVM_0xx: Individual Opcode Tests
-- =============================================================================

||| REQ_EVM_010: STOP opcode (0x00)
export
test_REQ_EVM_010_stop : IO Bool
test_REQ_EVM_010_stop = pure (STOP == "00")

||| REQ_EVM_011: ADD opcode (0x01)
export
test_REQ_EVM_011_add : IO Bool
test_REQ_EVM_011_add = pure (ADD == "01")

||| REQ_EVM_012: MUL opcode (0x02)
export
test_REQ_EVM_012_mul : IO Bool
test_REQ_EVM_012_mul = pure (MUL == "02")

||| REQ_EVM_013: SUB opcode (0x03)
export
test_REQ_EVM_013_sub : IO Bool
test_REQ_EVM_013_sub = pure (SUB == "03")

||| REQ_EVM_014: DIV opcode (0x04)
export
test_REQ_EVM_014_div : IO Bool
test_REQ_EVM_014_div = pure (DIV == "04")

||| REQ_EVM_015: SDIV opcode (0x05)
export
test_REQ_EVM_015_sdiv : IO Bool
test_REQ_EVM_015_sdiv = pure (SDIV == "05")

||| REQ_EVM_016: MOD opcode (0x06)
export
test_REQ_EVM_016_mod : IO Bool
test_REQ_EVM_016_mod = pure (MOD == "06")

||| REQ_EVM_017: SMOD opcode (0x07)
export
test_REQ_EVM_017_smod : IO Bool
test_REQ_EVM_017_smod = pure (SMOD == "07")

||| REQ_EVM_018: ADDMOD opcode (0x08)
export
test_REQ_EVM_018_addmod : IO Bool
test_REQ_EVM_018_addmod = pure (ADDMOD == "08")

||| REQ_EVM_019: MULMOD opcode (0x09)
export
test_REQ_EVM_019_mulmod : IO Bool
test_REQ_EVM_019_mulmod = pure (MULMOD == "09")

||| REQ_EVM_020: EXP opcode (0x0a)
export
test_REQ_EVM_020_exp : IO Bool
test_REQ_EVM_020_exp = pure (EXP == "0a")

||| REQ_EVM_021: SIGNEXTEND opcode (0x0b)
export
test_REQ_EVM_021_signextend : IO Bool
test_REQ_EVM_021_signextend = pure (SIGNEXTEND == "0b")

||| REQ_EVM_022: LT opcode (0x10)
export
test_REQ_EVM_022_lt : IO Bool
test_REQ_EVM_022_lt = pure (OP_LT == "10")

||| REQ_EVM_023: GT opcode (0x11)
export
test_REQ_EVM_023_gt : IO Bool
test_REQ_EVM_023_gt = pure (OP_GT == "11")

||| REQ_EVM_024: SLT opcode (0x12)
export
test_REQ_EVM_024_slt : IO Bool
test_REQ_EVM_024_slt = pure (SLT == "12")

||| REQ_EVM_025: SGT opcode (0x13)
export
test_REQ_EVM_025_sgt : IO Bool
test_REQ_EVM_025_sgt = pure (SGT == "13")

||| REQ_EVM_026: EQ opcode (0x14)
export
test_REQ_EVM_026_eq : IO Bool
test_REQ_EVM_026_eq = pure (OP_EQ == "14")

||| REQ_EVM_027: ISZERO opcode (0x15)
export
test_REQ_EVM_027_iszero : IO Bool
test_REQ_EVM_027_iszero = pure (ISZERO == "15")

||| REQ_EVM_028: AND opcode (0x16)
export
test_REQ_EVM_028_and : IO Bool
test_REQ_EVM_028_and = pure (AND == "16")

||| REQ_EVM_029: OR opcode (0x17)
export
test_REQ_EVM_029_or : IO Bool
test_REQ_EVM_029_or = pure (OR == "17")

||| REQ_EVM_030: XOR opcode (0x18)
export
test_REQ_EVM_030_xor : IO Bool
test_REQ_EVM_030_xor = pure (XOR == "18")

||| REQ_EVM_031: NOT opcode (0x19)
export
test_REQ_EVM_031_not : IO Bool
test_REQ_EVM_031_not = pure (NOT == "19")

||| REQ_EVM_032: BYTE opcode (0x1a)
export
test_REQ_EVM_032_byte : IO Bool
test_REQ_EVM_032_byte = pure (BYTE == "1a")

||| REQ_EVM_033: SHL opcode (0x1b)
export
test_REQ_EVM_033_shl : IO Bool
test_REQ_EVM_033_shl = pure (SHL == "1b")

||| REQ_EVM_034: SHR opcode (0x1c)
export
test_REQ_EVM_034_shr : IO Bool
test_REQ_EVM_034_shr = pure (SHR == "1c")

||| REQ_EVM_035: SAR opcode (0x1d)
export
test_REQ_EVM_035_sar : IO Bool
test_REQ_EVM_035_sar = pure (SAR == "1d")

||| REQ_EVM_036: KECCAK256 opcode (0x20)
export
test_REQ_EVM_036_keccak256 : IO Bool
test_REQ_EVM_036_keccak256 = pure (KECCAK256 == "20")

||| REQ_EVM_037: ADDRESS opcode (0x30)
export
test_REQ_EVM_037_address : IO Bool
test_REQ_EVM_037_address = pure (ADDRESS == "30")

||| REQ_EVM_038: BALANCE opcode (0x31)
export
test_REQ_EVM_038_balance : IO Bool
test_REQ_EVM_038_balance = pure (BALANCE == "31")

||| REQ_EVM_039: ORIGIN opcode (0x32)
export
test_REQ_EVM_039_origin : IO Bool
test_REQ_EVM_039_origin = pure (ORIGIN == "32")

||| REQ_EVM_040: CALLER opcode (0x33)
export
test_REQ_EVM_040_caller : IO Bool
test_REQ_EVM_040_caller = pure (CALLER == "33")

||| REQ_EVM_041: CALLVALUE opcode (0x34)
export
test_REQ_EVM_041_callvalue : IO Bool
test_REQ_EVM_041_callvalue = pure (CALLVALUE == "34")

||| REQ_EVM_042: CALLDATALOAD opcode (0x35)
export
test_REQ_EVM_042_calldataload : IO Bool
test_REQ_EVM_042_calldataload = pure (CALLDATALOAD == "35")

||| REQ_EVM_043: CALLDATASIZE opcode (0x36)
export
test_REQ_EVM_043_calldatasize : IO Bool
test_REQ_EVM_043_calldatasize = pure (CALLDATASIZE == "36")

||| REQ_EVM_044: CALLDATACOPY opcode (0x37)
export
test_REQ_EVM_044_calldatacopy : IO Bool
test_REQ_EVM_044_calldatacopy = pure (CALLDATACOPY == "37")

||| REQ_EVM_045: CODESIZE opcode (0x38)
export
test_REQ_EVM_045_codesize : IO Bool
test_REQ_EVM_045_codesize = pure (CODESIZE == "38")

||| REQ_EVM_046: CODECOPY opcode (0x39)
export
test_REQ_EVM_046_codecopy : IO Bool
test_REQ_EVM_046_codecopy = pure (CODECOPY == "39")

||| REQ_EVM_047: GASPRICE opcode (0x3a)
export
test_REQ_EVM_047_gasprice : IO Bool
test_REQ_EVM_047_gasprice = pure (GASPRICE == "3a")

||| REQ_EVM_048: EXTCODESIZE opcode (0x3b)
export
test_REQ_EVM_048_extcodesize : IO Bool
test_REQ_EVM_048_extcodesize = pure (EXTCODESIZE == "3b")

||| REQ_EVM_049: EXTCODECOPY opcode (0x3c)
export
test_REQ_EVM_049_extcodecopy : IO Bool
test_REQ_EVM_049_extcodecopy = pure (EXTCODECOPY == "3c")

||| REQ_EVM_050: RETURNDATASIZE opcode (0x3d)
export
test_REQ_EVM_050_returndatasize : IO Bool
test_REQ_EVM_050_returndatasize = pure (RETURNDATASIZE == "3d")

||| REQ_EVM_051: RETURNDATACOPY opcode (0x3e)
export
test_REQ_EVM_051_returndatacopy : IO Bool
test_REQ_EVM_051_returndatacopy = pure (RETURNDATACOPY == "3e")

||| REQ_EVM_052: EXTCODEHASH opcode (0x3f)
export
test_REQ_EVM_052_extcodehash : IO Bool
test_REQ_EVM_052_extcodehash = pure (EXTCODEHASH == "3f")

||| REQ_EVM_053: BLOCKHASH opcode (0x40)
export
test_REQ_EVM_053_blockhash : IO Bool
test_REQ_EVM_053_blockhash = pure (BLOCKHASH == "40")

||| REQ_EVM_054: COINBASE opcode (0x41)
export
test_REQ_EVM_054_coinbase : IO Bool
test_REQ_EVM_054_coinbase = pure (COINBASE == "41")

||| REQ_EVM_055: TIMESTAMP opcode (0x42)
export
test_REQ_EVM_055_timestamp : IO Bool
test_REQ_EVM_055_timestamp = pure (TIMESTAMP == "42")

||| REQ_EVM_056: NUMBER opcode (0x43)
export
test_REQ_EVM_056_number : IO Bool
test_REQ_EVM_056_number = pure (NUMBER == "43")

||| REQ_EVM_057: PREVRANDAO opcode (0x44)
export
test_REQ_EVM_057_prevrandao : IO Bool
test_REQ_EVM_057_prevrandao = pure (PREVRANDAO == "44")

||| REQ_EVM_058: GASLIMIT opcode (0x45)
export
test_REQ_EVM_058_gaslimit : IO Bool
test_REQ_EVM_058_gaslimit = pure (GASLIMIT == "45")

||| REQ_EVM_059: CHAINID opcode (0x46)
export
test_REQ_EVM_059_chainid : IO Bool
test_REQ_EVM_059_chainid = pure (CHAINID == "46")

||| REQ_EVM_060: SELFBALANCE opcode (0x47)
export
test_REQ_EVM_060_selfbalance : IO Bool
test_REQ_EVM_060_selfbalance = pure (SELFBALANCE == "47")

||| REQ_EVM_061: BASEFEE opcode (0x48)
export
test_REQ_EVM_061_basefee : IO Bool
test_REQ_EVM_061_basefee = pure (BASEFEE == "48")

||| REQ_EVM_062: BLOBHASH opcode (0x49)
export
test_REQ_EVM_062_blobhash : IO Bool
test_REQ_EVM_062_blobhash = pure (BLOBHASH == "49")

||| REQ_EVM_063: BLOBBASEFEE opcode (0x4a)
export
test_REQ_EVM_063_blobbasefee : IO Bool
test_REQ_EVM_063_blobbasefee = pure (BLOBBASEFEE == "4a")

||| REQ_EVM_064: POP opcode (0x50)
export
test_REQ_EVM_064_pop : IO Bool
test_REQ_EVM_064_pop = pure (POP == "50")

||| REQ_EVM_065: MLOAD opcode (0x51)
export
test_REQ_EVM_065_mload : IO Bool
test_REQ_EVM_065_mload = pure (MLOAD == "51")

||| REQ_EVM_066: MSTORE opcode (0x52)
export
test_REQ_EVM_066_mstore : IO Bool
test_REQ_EVM_066_mstore = pure (MSTORE == "52")

||| REQ_EVM_067: MSTORE8 opcode (0x53)
export
test_REQ_EVM_067_mstore8 : IO Bool
test_REQ_EVM_067_mstore8 = pure (MSTORE8 == "53")

||| REQ_EVM_068: SLOAD opcode (0x54)
export
test_REQ_EVM_068_sload : IO Bool
test_REQ_EVM_068_sload = pure (SLOAD == "54")

||| REQ_EVM_069: SSTORE opcode (0x55)
export
test_REQ_EVM_069_sstore : IO Bool
test_REQ_EVM_069_sstore = pure (SSTORE == "55")

||| REQ_EVM_070: JUMP opcode (0x56)
export
test_REQ_EVM_070_jump : IO Bool
test_REQ_EVM_070_jump = pure (JUMP == "56")

||| REQ_EVM_071: JUMPI opcode (0x57)
export
test_REQ_EVM_071_jumpi : IO Bool
test_REQ_EVM_071_jumpi = pure (JUMPI == "57")

||| REQ_EVM_072: PC opcode (0x58)
export
test_REQ_EVM_072_pc : IO Bool
test_REQ_EVM_072_pc = pure (PC == "58")

||| REQ_EVM_073: MSIZE opcode (0x59)
export
test_REQ_EVM_073_msize : IO Bool
test_REQ_EVM_073_msize = pure (MSIZE == "59")

||| REQ_EVM_074: GAS opcode (0x5a)
export
test_REQ_EVM_074_gas : IO Bool
test_REQ_EVM_074_gas = pure (GAS == "5a")

||| REQ_EVM_075: JUMPDEST opcode (0x5b)
export
test_REQ_EVM_075_jumpdest : IO Bool
test_REQ_EVM_075_jumpdest = pure (JUMPDEST == "5b")

||| REQ_EVM_076: TLOAD opcode (0x5c)
export
test_REQ_EVM_076_tload : IO Bool
test_REQ_EVM_076_tload = pure (TLOAD == "5c")

||| REQ_EVM_077: TSTORE opcode (0x5d)
export
test_REQ_EVM_077_tstore : IO Bool
test_REQ_EVM_077_tstore = pure (TSTORE == "5d")

||| REQ_EVM_078: MCOPY opcode (0x5e)
export
test_REQ_EVM_078_mcopy : IO Bool
test_REQ_EVM_078_mcopy = pure (MCOPY == "5e")

||| REQ_EVM_079: PUSH0 opcode (0x5f)
export
test_REQ_EVM_079_push0 : IO Bool
test_REQ_EVM_079_push0 = pure (PUSH0 == "5f")

||| REQ_EVM_080: PUSH1 opcode (0x60)
export
test_REQ_EVM_080_push1 : IO Bool
test_REQ_EVM_080_push1 = pure (PUSH1 == "60")

||| REQ_EVM_081: PUSH2 opcode (0x61)
export
test_REQ_EVM_081_push2 : IO Bool
test_REQ_EVM_081_push2 = pure (PUSH2 == "61")

||| REQ_EVM_082: PUSH3 opcode (0x62)
export
test_REQ_EVM_082_push3 : IO Bool
test_REQ_EVM_082_push3 = pure (PUSH3 == "62")

||| REQ_EVM_083: PUSH4 opcode (0x63)
export
test_REQ_EVM_083_push4 : IO Bool
test_REQ_EVM_083_push4 = pure (PUSH4 == "63")

||| REQ_EVM_084: PUSH5 opcode (0x64)
export
test_REQ_EVM_084_push5 : IO Bool
test_REQ_EVM_084_push5 = pure (PUSH5 == "64")

||| REQ_EVM_085: PUSH6 opcode (0x65)
export
test_REQ_EVM_085_push6 : IO Bool
test_REQ_EVM_085_push6 = pure (PUSH6 == "65")

||| REQ_EVM_086: PUSH7 opcode (0x66)
export
test_REQ_EVM_086_push7 : IO Bool
test_REQ_EVM_086_push7 = pure (PUSH7 == "66")

||| REQ_EVM_087: PUSH8 opcode (0x67)
export
test_REQ_EVM_087_push8 : IO Bool
test_REQ_EVM_087_push8 = pure (PUSH8 == "67")

||| REQ_EVM_088: PUSH9 opcode (0x68)
export
test_REQ_EVM_088_push9 : IO Bool
test_REQ_EVM_088_push9 = pure (PUSH9 == "68")

||| REQ_EVM_089: PUSH10 opcode (0x69)
export
test_REQ_EVM_089_push10 : IO Bool
test_REQ_EVM_089_push10 = pure (PUSH10 == "69")

||| REQ_EVM_090: PUSH11 opcode (0x6a)
export
test_REQ_EVM_090_push11 : IO Bool
test_REQ_EVM_090_push11 = pure (PUSH11 == "6a")

||| REQ_EVM_091: PUSH12 opcode (0x6b)
export
test_REQ_EVM_091_push12 : IO Bool
test_REQ_EVM_091_push12 = pure (PUSH12 == "6b")

||| REQ_EVM_092: PUSH13 opcode (0x6c)
export
test_REQ_EVM_092_push13 : IO Bool
test_REQ_EVM_092_push13 = pure (PUSH13 == "6c")

||| REQ_EVM_093: PUSH14 opcode (0x6d)
export
test_REQ_EVM_093_push14 : IO Bool
test_REQ_EVM_093_push14 = pure (PUSH14 == "6d")

||| REQ_EVM_094: PUSH15 opcode (0x6e)
export
test_REQ_EVM_094_push15 : IO Bool
test_REQ_EVM_094_push15 = pure (PUSH15 == "6e")

||| REQ_EVM_095: PUSH16 opcode (0x6f)
export
test_REQ_EVM_095_push16 : IO Bool
test_REQ_EVM_095_push16 = pure (PUSH16 == "6f")

||| REQ_EVM_096: PUSH17 opcode (0x70)
export
test_REQ_EVM_096_push17 : IO Bool
test_REQ_EVM_096_push17 = pure (PUSH17 == "70")

||| REQ_EVM_097: PUSH18 opcode (0x71)
export
test_REQ_EVM_097_push18 : IO Bool
test_REQ_EVM_097_push18 = pure (PUSH18 == "71")

||| REQ_EVM_098: PUSH19 opcode (0x72)
export
test_REQ_EVM_098_push19 : IO Bool
test_REQ_EVM_098_push19 = pure (PUSH19 == "72")

||| REQ_EVM_099: PUSH20 opcode (0x73)
export
test_REQ_EVM_099_push20 : IO Bool
test_REQ_EVM_099_push20 = pure (PUSH20 == "73")

||| REQ_EVM_100: PUSH21 opcode (0x74)
export
test_REQ_EVM_100_push21 : IO Bool
test_REQ_EVM_100_push21 = pure (PUSH21 == "74")

||| REQ_EVM_101: PUSH22 opcode (0x75)
export
test_REQ_EVM_101_push22 : IO Bool
test_REQ_EVM_101_push22 = pure (PUSH22 == "75")

||| REQ_EVM_102: PUSH23 opcode (0x76)
export
test_REQ_EVM_102_push23 : IO Bool
test_REQ_EVM_102_push23 = pure (PUSH23 == "76")

||| REQ_EVM_103: PUSH24 opcode (0x77)
export
test_REQ_EVM_103_push24 : IO Bool
test_REQ_EVM_103_push24 = pure (PUSH24 == "77")

||| REQ_EVM_104: PUSH25 opcode (0x78)
export
test_REQ_EVM_104_push25 : IO Bool
test_REQ_EVM_104_push25 = pure (PUSH25 == "78")

||| REQ_EVM_105: PUSH26 opcode (0x79)
export
test_REQ_EVM_105_push26 : IO Bool
test_REQ_EVM_105_push26 = pure (PUSH26 == "79")

||| REQ_EVM_106: PUSH27 opcode (0x7a)
export
test_REQ_EVM_106_push27 : IO Bool
test_REQ_EVM_106_push27 = pure (PUSH27 == "7a")

||| REQ_EVM_107: PUSH28 opcode (0x7b)
export
test_REQ_EVM_107_push28 : IO Bool
test_REQ_EVM_107_push28 = pure (PUSH28 == "7b")

||| REQ_EVM_108: PUSH29 opcode (0x7c)
export
test_REQ_EVM_108_push29 : IO Bool
test_REQ_EVM_108_push29 = pure (PUSH29 == "7c")

||| REQ_EVM_109: PUSH30 opcode (0x7d)
export
test_REQ_EVM_109_push30 : IO Bool
test_REQ_EVM_109_push30 = pure (PUSH30 == "7d")

||| REQ_EVM_110: PUSH31 opcode (0x7e)
export
test_REQ_EVM_110_push31 : IO Bool
test_REQ_EVM_110_push31 = pure (PUSH31 == "7e")

||| REQ_EVM_111: PUSH32 opcode (0x7f)
export
test_REQ_EVM_111_push32 : IO Bool
test_REQ_EVM_111_push32 = pure (PUSH32 == "7f")

||| REQ_EVM_112: DUP1 opcode (0x80)
export
test_REQ_EVM_112_dup1 : IO Bool
test_REQ_EVM_112_dup1 = pure (DUP1 == "80")

||| REQ_EVM_113: DUP2 opcode (0x81)
export
test_REQ_EVM_113_dup2 : IO Bool
test_REQ_EVM_113_dup2 = pure (DUP2 == "81")

||| REQ_EVM_114: DUP3 opcode (0x82)
export
test_REQ_EVM_114_dup3 : IO Bool
test_REQ_EVM_114_dup3 = pure (DUP3 == "82")

||| REQ_EVM_115: DUP4 opcode (0x83)
export
test_REQ_EVM_115_dup4 : IO Bool
test_REQ_EVM_115_dup4 = pure (DUP4 == "83")

||| REQ_EVM_116: DUP5 opcode (0x84)
export
test_REQ_EVM_116_dup5 : IO Bool
test_REQ_EVM_116_dup5 = pure (DUP5 == "84")

||| REQ_EVM_117: DUP6 opcode (0x85)
export
test_REQ_EVM_117_dup6 : IO Bool
test_REQ_EVM_117_dup6 = pure (DUP6 == "85")

||| REQ_EVM_118: DUP7 opcode (0x86)
export
test_REQ_EVM_118_dup7 : IO Bool
test_REQ_EVM_118_dup7 = pure (DUP7 == "86")

||| REQ_EVM_119: DUP8 opcode (0x87)
export
test_REQ_EVM_119_dup8 : IO Bool
test_REQ_EVM_119_dup8 = pure (DUP8 == "87")

||| REQ_EVM_120: DUP9 opcode (0x88)
export
test_REQ_EVM_120_dup9 : IO Bool
test_REQ_EVM_120_dup9 = pure (DUP9 == "88")

||| REQ_EVM_121: DUP10 opcode (0x89)
export
test_REQ_EVM_121_dup10 : IO Bool
test_REQ_EVM_121_dup10 = pure (DUP10 == "89")

||| REQ_EVM_122: DUP11 opcode (0x8a)
export
test_REQ_EVM_122_dup11 : IO Bool
test_REQ_EVM_122_dup11 = pure (DUP11 == "8a")

||| REQ_EVM_123: DUP12 opcode (0x8b)
export
test_REQ_EVM_123_dup12 : IO Bool
test_REQ_EVM_123_dup12 = pure (DUP12 == "8b")

||| REQ_EVM_124: DUP13 opcode (0x8c)
export
test_REQ_EVM_124_dup13 : IO Bool
test_REQ_EVM_124_dup13 = pure (DUP13 == "8c")

||| REQ_EVM_125: DUP14 opcode (0x8d)
export
test_REQ_EVM_125_dup14 : IO Bool
test_REQ_EVM_125_dup14 = pure (DUP14 == "8d")

||| REQ_EVM_126: DUP15 opcode (0x8e)
export
test_REQ_EVM_126_dup15 : IO Bool
test_REQ_EVM_126_dup15 = pure (DUP15 == "8e")

||| REQ_EVM_127: DUP16 opcode (0x8f)
export
test_REQ_EVM_127_dup16 : IO Bool
test_REQ_EVM_127_dup16 = pure (DUP16 == "8f")

||| REQ_EVM_128: SWAP1 opcode (0x90)
export
test_REQ_EVM_128_swap1 : IO Bool
test_REQ_EVM_128_swap1 = pure (SWAP1 == "90")

||| REQ_EVM_129: SWAP2 opcode (0x91)
export
test_REQ_EVM_129_swap2 : IO Bool
test_REQ_EVM_129_swap2 = pure (SWAP2 == "91")

||| REQ_EVM_130: SWAP3 opcode (0x92)
export
test_REQ_EVM_130_swap3 : IO Bool
test_REQ_EVM_130_swap3 = pure (SWAP3 == "92")

||| REQ_EVM_131: SWAP4 opcode (0x93)
export
test_REQ_EVM_131_swap4 : IO Bool
test_REQ_EVM_131_swap4 = pure (SWAP4 == "93")

||| REQ_EVM_132: SWAP5 opcode (0x94)
export
test_REQ_EVM_132_swap5 : IO Bool
test_REQ_EVM_132_swap5 = pure (SWAP5 == "94")

||| REQ_EVM_133: SWAP6 opcode (0x95)
export
test_REQ_EVM_133_swap6 : IO Bool
test_REQ_EVM_133_swap6 = pure (SWAP6 == "95")

||| REQ_EVM_134: SWAP7 opcode (0x96)
export
test_REQ_EVM_134_swap7 : IO Bool
test_REQ_EVM_134_swap7 = pure (SWAP7 == "96")

||| REQ_EVM_135: SWAP8 opcode (0x97)
export
test_REQ_EVM_135_swap8 : IO Bool
test_REQ_EVM_135_swap8 = pure (SWAP8 == "97")

||| REQ_EVM_136: SWAP9 opcode (0x98)
export
test_REQ_EVM_136_swap9 : IO Bool
test_REQ_EVM_136_swap9 = pure (SWAP9 == "98")

||| REQ_EVM_137: SWAP10 opcode (0x99)
export
test_REQ_EVM_137_swap10 : IO Bool
test_REQ_EVM_137_swap10 = pure (SWAP10 == "99")

||| REQ_EVM_138: SWAP11 opcode (0x9a)
export
test_REQ_EVM_138_swap11 : IO Bool
test_REQ_EVM_138_swap11 = pure (SWAP11 == "9a")

||| REQ_EVM_139: SWAP12 opcode (0x9b)
export
test_REQ_EVM_139_swap12 : IO Bool
test_REQ_EVM_139_swap12 = pure (SWAP12 == "9b")

||| REQ_EVM_140: SWAP13 opcode (0x9c)
export
test_REQ_EVM_140_swap13 : IO Bool
test_REQ_EVM_140_swap13 = pure (SWAP13 == "9c")

||| REQ_EVM_141: SWAP14 opcode (0x9d)
export
test_REQ_EVM_141_swap14 : IO Bool
test_REQ_EVM_141_swap14 = pure (SWAP14 == "9d")

||| REQ_EVM_142: SWAP15 opcode (0x9e)
export
test_REQ_EVM_142_swap15 : IO Bool
test_REQ_EVM_142_swap15 = pure (SWAP15 == "9e")

||| REQ_EVM_143: SWAP16 opcode (0x9f)
export
test_REQ_EVM_143_swap16 : IO Bool
test_REQ_EVM_143_swap16 = pure (SWAP16 == "9f")

||| REQ_EVM_144: LOG0 opcode (0xa0)
export
test_REQ_EVM_144_log0 : IO Bool
test_REQ_EVM_144_log0 = pure (LOG0 == "a0")

||| REQ_EVM_145: LOG1 opcode (0xa1)
export
test_REQ_EVM_145_log1 : IO Bool
test_REQ_EVM_145_log1 = pure (LOG1 == "a1")

||| REQ_EVM_146: LOG2 opcode (0xa2)
export
test_REQ_EVM_146_log2 : IO Bool
test_REQ_EVM_146_log2 = pure (LOG2 == "a2")

||| REQ_EVM_147: LOG3 opcode (0xa3)
export
test_REQ_EVM_147_log3 : IO Bool
test_REQ_EVM_147_log3 = pure (LOG3 == "a3")

||| REQ_EVM_148: LOG4 opcode (0xa4)
export
test_REQ_EVM_148_log4 : IO Bool
test_REQ_EVM_148_log4 = pure (LOG4 == "a4")

||| REQ_EVM_149: CREATE opcode (0xf0)
export
test_REQ_EVM_149_create : IO Bool
test_REQ_EVM_149_create = pure (CREATE == "f0")

||| REQ_EVM_150: CALL opcode (0xf1)
export
test_REQ_EVM_150_call : IO Bool
test_REQ_EVM_150_call = pure (CALL == "f1")

||| REQ_EVM_151: CALLCODE opcode (0xf2)
export
test_REQ_EVM_151_callcode : IO Bool
test_REQ_EVM_151_callcode = pure (CALLCODE == "f2")

||| REQ_EVM_152: RETURN opcode (0xf3)
export
test_REQ_EVM_152_return : IO Bool
test_REQ_EVM_152_return = pure (RETURN == "f3")

||| REQ_EVM_153: DELEGATECALL opcode (0xf4)
export
test_REQ_EVM_153_delegatecall : IO Bool
test_REQ_EVM_153_delegatecall = pure (DELEGATECALL == "f4")

||| REQ_EVM_154: CREATE2 opcode (0xf5)
export
test_REQ_EVM_154_create2 : IO Bool
test_REQ_EVM_154_create2 = pure (CREATE2 == "f5")

||| REQ_EVM_155: STATICCALL opcode (0xfa)
export
test_REQ_EVM_155_staticcall : IO Bool
test_REQ_EVM_155_staticcall = pure (STATICCALL == "fa")

||| REQ_EVM_156: REVERT opcode (0xfd)
export
test_REQ_EVM_156_revert : IO Bool
test_REQ_EVM_156_revert = pure (REVERT == "fd")

||| REQ_EVM_157: INVALID opcode (0xfe)
export
test_REQ_EVM_157_invalid : IO Bool
test_REQ_EVM_157_invalid = pure (INVALID == "fe")

||| REQ_EVM_158: SELFDESTRUCT opcode (0xff)
export
test_REQ_EVM_158_selfdestruct : IO Bool
test_REQ_EVM_158_selfdestruct = pure (SELFDESTRUCT == "ff")

-- =============================================================================
-- REQ_YUL_*: YulIR Tests
-- =============================================================================

||| REQ_YUL_001: showExpr YLit number
export
test_REQ_YUL_001_showExprLitNum : IO Bool
test_REQ_YUL_001_showExprLitNum = pure $ showExpr (YLit (YulNum 42)) == "42"

||| REQ_YUL_002: showExpr YLit hex
export
test_REQ_YUL_002_showExprLitHex : IO Bool
test_REQ_YUL_002_showExprLitHex = pure $ showExpr (YLit (YulHex "ff")) == "0xff"

||| REQ_YUL_003: showExpr YLit bool true
export
test_REQ_YUL_003_showExprLitBoolTrue : IO Bool
test_REQ_YUL_003_showExprLitBoolTrue = pure $ showExpr (YLit (YulBool True)) == "true"

||| REQ_YUL_004: showExpr YLit bool false
export
test_REQ_YUL_004_showExprLitBoolFalse : IO Bool
test_REQ_YUL_004_showExprLitBoolFalse = pure $ showExpr (YLit (YulBool False)) == "false"

||| REQ_YUL_005: showExpr YVar
export
test_REQ_YUL_005_showExprVar : IO Bool
test_REQ_YUL_005_showExprVar = pure $ showExpr (YVar "x") == "x"

||| REQ_YUL_006: showExpr YCall no args
export
test_REQ_YUL_006_showExprCallNoArgs : IO Bool
test_REQ_YUL_006_showExprCallNoArgs = pure $ showExpr (YCall "foo" []) == "foo()"

||| REQ_YUL_007: showExpr YCall with args
export
test_REQ_YUL_007_showExprCallWithArgs : IO Bool
test_REQ_YUL_007_showExprCallWithArgs =
  pure $ showExpr (YCall "add" [YVar "a", YVar "b"]) == "add(a, b)"

||| REQ_YUL_008: yulNum builder
export
test_REQ_YUL_008_yulNum : IO Bool
test_REQ_YUL_008_yulNum = pure $ showExpr (yulNum 100) == "100"

||| REQ_YUL_009: yulVar builder
export
test_REQ_YUL_009_yulVar : IO Bool
test_REQ_YUL_009_yulVar = pure $ showExpr (yulVar "myVar") == "myVar"

||| REQ_YUL_010: yulCall builder
export
test_REQ_YUL_010_yulCall : IO Bool
test_REQ_YUL_010_yulCall = pure $ showExpr (yulCall "mstore" [yulNum 0, yulNum 64]) == "mstore(0, 64)"

||| REQ_YUL_011: sload builder
export
test_REQ_YUL_011_sloadBuilder : IO Bool
test_REQ_YUL_011_sloadBuilder = pure $ showExpr (YulIR.sload (yulNum 5)) == "sload(5)"

||| REQ_YUL_012: sstore builder
export
test_REQ_YUL_012_sstoreBuilder : IO Bool
test_REQ_YUL_012_sstoreBuilder = pure $ showExpr (YulIR.sstore (yulNum 0) (yulNum 42)) == "sstore(0, 42)"

||| REQ_YUL_013: mload builder
export
test_REQ_YUL_013_mloadBuilder : IO Bool
test_REQ_YUL_013_mloadBuilder = pure $ showExpr (YulIR.mload (yulNum 32)) == "mload(32)"

||| REQ_YUL_014: mstore builder
export
test_REQ_YUL_014_mstoreBuilder : IO Bool
test_REQ_YUL_014_mstoreBuilder = pure $ showExpr (YulIR.mstore (yulNum 0) (yulNum 1)) == "mstore(0, 1)"

||| REQ_YUL_015: calldataload builder
export
test_REQ_YUL_015_calldataloadBuilder : IO Bool
test_REQ_YUL_015_calldataloadBuilder = pure $ showExpr (YulIR.calldataload (yulNum 4)) == "calldataload(4)"

||| REQ_YUL_016: revert builder
export
test_REQ_YUL_016_revertBuilder : IO Bool
test_REQ_YUL_016_revertBuilder = pure $ showExpr (YulIR.revert (yulNum 0) (yulNum 0)) == "revert(0, 0)"

||| REQ_YUL_017: yulReturn builder
export
test_REQ_YUL_017_yulReturnBuilder : IO Bool
test_REQ_YUL_017_yulReturnBuilder = pure $ showExpr (YulIR.yulReturn (yulNum 0) (yulNum 32)) == "return(0, 32)"

||| REQ_YUL_018: collectCallsExpr YLit
export
test_REQ_YUL_018_collectCallsExprLit : IO Bool
test_REQ_YUL_018_collectCallsExprLit = pure $ null (SortedSet.toList (collectCallsExpr (YLit (YulNum 1))))

||| REQ_YUL_019: collectCallsExpr YVar
export
test_REQ_YUL_019_collectCallsExprVar : IO Bool
test_REQ_YUL_019_collectCallsExprVar = pure $ null (SortedSet.toList (collectCallsExpr (YVar "x")))

||| REQ_YUL_020: collectCallsExpr YCall
export
test_REQ_YUL_020_collectCallsExprCall : IO Bool
test_REQ_YUL_020_collectCallsExprCall =
  let calls = collectCallsExpr (YCall "foo" [YCall "bar" []])
  in pure $ SortedSet.contains "foo" calls && SortedSet.contains "bar" calls

||| REQ_YUL_021: SourceLoc show format
export
test_REQ_YUL_021_sourceLocShow : IO Bool
test_REQ_YUL_021_sourceLocShow =
  let loc = MkSourceLoc "Main" "test" 10 5 10 15
  in pure $ show loc == "Main:10:5--10:15"

||| REQ_YUL_022: sourceLocComment format
export
test_REQ_YUL_022_sourceLocComment : IO Bool
test_REQ_YUL_022_sourceLocComment =
  let loc = MkSourceLoc "Mod" "fn" 1 1 1 5
  in pure $ sourceLocComment loc == "/* Mod:1:1--1:5 */"

-- =============================================================================
-- REQ_ABI_*: ABI Types Tests
-- =============================================================================

||| REQ_ABI_001: abiTypeToString uint256
export
test_REQ_ABI_001_uint256 : IO Bool
test_REQ_ABI_001_uint256 = pure $ abiTypeToString ABI_Uint256 == "uint256"

||| REQ_ABI_002: abiTypeToString address
export
test_REQ_ABI_002_address : IO Bool
test_REQ_ABI_002_address = pure $ abiTypeToString ABI_Address == "address"

||| REQ_ABI_003: abiTypeToString bool
export
test_REQ_ABI_003_bool : IO Bool
test_REQ_ABI_003_bool = pure $ abiTypeToString ABI_Bool == "bool"

||| REQ_ABI_004: abiTypeToString bytes32
export
test_REQ_ABI_004_bytes32 : IO Bool
test_REQ_ABI_004_bytes32 = pure $ abiTypeToString ABI_Bytes32 == "bytes32"

||| REQ_ABI_005: abiTypeToString string
export
test_REQ_ABI_005_string : IO Bool
test_REQ_ABI_005_string = pure $ abiTypeToString ABI_String == "string"

||| REQ_ABI_006: abiTypeToString bytes
export
test_REQ_ABI_006_bytes : IO Bool
test_REQ_ABI_006_bytes = pure $ abiTypeToString ABI_Bytes == "bytes"

||| REQ_ABI_007: abiTypeToString array
export
test_REQ_ABI_007_array : IO Bool
test_REQ_ABI_007_array = pure $ abiTypeToString (ABI_Array ABI_Uint256) == "uint256[]"

||| REQ_ABI_008: abiTypeToString fixed array
export
test_REQ_ABI_008_fixedArray : IO Bool
test_REQ_ABI_008_fixedArray = pure $ abiTypeToString (ABI_FixedArray 3 ABI_Address) == "address[3]"

||| REQ_ABI_009: abiTypeToString tuple
export
test_REQ_ABI_009_tuple : IO Bool
test_REQ_ABI_009_tuple = pure $ abiTypeToString (ABI_Tuple [ABI_Uint256, ABI_Address]) == "(uint256,address)"

||| REQ_ABI_021: abiTypeToString uint128
export
test_REQ_ABI_021_uint128 : IO Bool
test_REQ_ABI_021_uint128 = pure $ abiTypeToString ABI_Uint128 == "uint128"

||| REQ_ABI_022: abiTypeToString uint64
export
test_REQ_ABI_022_uint64 : IO Bool
test_REQ_ABI_022_uint64 = pure $ abiTypeToString ABI_Uint64 == "uint64"

||| REQ_ABI_023: abiTypeToString uint32
export
test_REQ_ABI_023_uint32 : IO Bool
test_REQ_ABI_023_uint32 = pure $ abiTypeToString ABI_Uint32 == "uint32"

||| REQ_ABI_024: abiTypeToString uint16
export
test_REQ_ABI_024_uint16 : IO Bool
test_REQ_ABI_024_uint16 = pure $ abiTypeToString ABI_Uint16 == "uint16"

||| REQ_ABI_025: abiTypeToString uint8
export
test_REQ_ABI_025_uint8 : IO Bool
test_REQ_ABI_025_uint8 = pure $ abiTypeToString ABI_Uint8 == "uint8"

||| REQ_ABI_026: abiTypeToString int256
export
test_REQ_ABI_026_int256 : IO Bool
test_REQ_ABI_026_int256 = pure $ abiTypeToString ABI_Int256 == "int256"

||| REQ_ABI_027: abiTypeToString int128
export
test_REQ_ABI_027_int128 : IO Bool
test_REQ_ABI_027_int128 = pure $ abiTypeToString ABI_Int128 == "int128"

||| REQ_ABI_028: abiTypeToString int64
export
test_REQ_ABI_028_int64 : IO Bool
test_REQ_ABI_028_int64 = pure $ abiTypeToString ABI_Int64 == "int64"

||| REQ_ABI_029: abiTypeToString int32
export
test_REQ_ABI_029_int32 : IO Bool
test_REQ_ABI_029_int32 = pure $ abiTypeToString ABI_Int32 == "int32"

||| REQ_ABI_030: abiTypeToString int16
export
test_REQ_ABI_030_int16 : IO Bool
test_REQ_ABI_030_int16 = pure $ abiTypeToString ABI_Int16 == "int16"

||| REQ_ABI_031: abiTypeToString int8
export
test_REQ_ABI_031_int8 : IO Bool
test_REQ_ABI_031_int8 = pure $ abiTypeToString ABI_Int8 == "int8"

||| REQ_ABI_032: abiTypeToString bytes20
export
test_REQ_ABI_032_bytes20 : IO Bool
test_REQ_ABI_032_bytes20 = pure $ abiTypeToString ABI_Bytes20 == "bytes20"

||| REQ_ABI_033: abiTypeToString bytes4
export
test_REQ_ABI_033_bytes4 : IO Bool
test_REQ_ABI_033_bytes4 = pure $ abiTypeToString ABI_Bytes4 == "bytes4"

||| REQ_ABI_034: eventSignature
export
test_REQ_ABI_034_eventSig : IO Bool
test_REQ_ABI_034_eventSig =
  let e = mkEvent "Transfer" 0 [indexed "from" ABI_Address, indexed "to" ABI_Address, param "value" ABI_Uint256]
  in pure $ eventSignature e == "Transfer(address,address,uint256)"

||| REQ_ABI_035: addEvent
export
test_REQ_ABI_035_addEvent : IO Bool
test_REQ_ABI_035_addEvent =
  let e = mkEvent "Test" 0 []
      abi = addEvent e (emptyABI "Test")
  in pure $ length abi.events == 1

||| REQ_ABI_036: addError
export
test_REQ_ABI_036_addError : IO Bool
test_REQ_ABI_036_addError =
  let err = MkError "TestError" 0 []
      abi = addError err (emptyABI "Test")
  in pure $ length abi.errors == 1

||| REQ_ABI_037: tuple empty
export
test_REQ_ABI_037_tupleEmpty : IO Bool
test_REQ_ABI_037_tupleEmpty = pure $ abiTypeToString (ABI_Tuple []) == "()"

||| REQ_ABI_038: tuple single
export
test_REQ_ABI_038_tupleSingle : IO Bool
test_REQ_ABI_038_tupleSingle = pure $ abiTypeToString (ABI_Tuple [ABI_Bool]) == "(bool)"

||| REQ_ABI_010: mutabilityToString pure
export
test_REQ_ABI_010_mutPure : IO Bool
test_REQ_ABI_010_mutPure = pure $ mutabilityToString Pure == "pure"

||| REQ_ABI_011: mutabilityToString view
export
test_REQ_ABI_011_mutView : IO Bool
test_REQ_ABI_011_mutView = pure $ mutabilityToString View == "view"

||| REQ_ABI_012: mutabilityToString nonpayable
export
test_REQ_ABI_012_mutNonpayable : IO Bool
test_REQ_ABI_012_mutNonpayable = pure $ mutabilityToString Nonpayable == "nonpayable"

||| REQ_ABI_013: mutabilityToString payable
export
test_REQ_ABI_013_mutPayable : IO Bool
test_REQ_ABI_013_mutPayable = pure $ mutabilityToString Payable == "payable"

||| REQ_ABI_014: functionSignature simple
export
test_REQ_ABI_014_funcSig : IO Bool
test_REQ_ABI_014_funcSig =
  let f = mkFunc "transfer" 0 [ABI_Address, ABI_Uint256] [ABI_Bool] Nonpayable
  in pure $ functionSignature f == "transfer(address,uint256)"

||| REQ_ABI_015: functionSignature no params
export
test_REQ_ABI_015_funcSigNoParams : IO Bool
test_REQ_ABI_015_funcSigNoParams =
  let f = mkFunc "totalSupply" 0 [] [ABI_Uint256] View
  in pure $ functionSignature f == "totalSupply()"

||| REQ_ABI_016: param constructor
export
test_REQ_ABI_016_param : IO Bool
test_REQ_ABI_016_param =
  let p = param "amount" ABI_Uint256
  in pure $ p.paramName == "amount" && not p.indexed

||| REQ_ABI_017: anon constructor
export
test_REQ_ABI_017_anon : IO Bool
test_REQ_ABI_017_anon =
  let p = anon ABI_Address
  in pure $ p.paramName == "" && not p.indexed

||| REQ_ABI_018: indexed constructor
export
test_REQ_ABI_018_indexed : IO Bool
test_REQ_ABI_018_indexed =
  let p = indexed "from" ABI_Address
  in pure $ p.paramName == "from" && p.indexed

||| REQ_ABI_019: emptyABI
export
test_REQ_ABI_019_emptyABI : IO Bool
test_REQ_ABI_019_emptyABI =
  let abi = emptyABI "Test"
  in pure $ abi.contractName == "Test" && null abi.functions

||| REQ_ABI_020: addFunction
export
test_REQ_ABI_020_addFunction : IO Bool
test_REQ_ABI_020_addFunction =
  let f = mkFunc "test" 0 [] [] Pure
      abi = addFunction f (emptyABI "Test")
  in pure $ length abi.functions == 1

-- =============================================================================
-- REQ_STOR_*: Storage Tests
-- =============================================================================

||| REQ_STOR_001: Bits256 Storable slotCount
export
test_REQ_STOR_001_bits256SlotCount : IO Bool
test_REQ_STOR_001_bits256SlotCount = pure $ slotCount {a=Bits256} == 1

||| REQ_STOR_002: Bool Storable slotCount
export
test_REQ_STOR_002_boolSlotCount : IO Bool
test_REQ_STOR_002_boolSlotCount = pure $ slotCount {a=Bool} == 1

||| REQ_STOR_003: Member Storable slotCount
export
test_REQ_STOR_003_memberSlotCount : IO Bool
test_REQ_STOR_003_memberSlotCount = pure $ slotCount {a=Member} == 2

||| REQ_STOR_004: Bits256 toSlots
export
test_REQ_STOR_004_bits256ToSlots : IO Bool
test_REQ_STOR_004_bits256ToSlots =
  let v : Bits256 = 42
      [x] = toSlots v
  in pure $ x == 42

||| REQ_STOR_005: Bool toSlots True
export
test_REQ_STOR_005_boolToSlotsTrue : IO Bool
test_REQ_STOR_005_boolToSlotsTrue =
  let [x] = toSlots True
  in pure $ x == 1

||| REQ_STOR_006: Bool toSlots False
export
test_REQ_STOR_006_boolToSlotsFalse : IO Bool
test_REQ_STOR_006_boolToSlotsFalse =
  let [x] = toSlots False
  in pure $ x == 0

||| REQ_STOR_007: refSlot extracts slot
export
test_REQ_STOR_007_refSlot : IO Bool
test_REQ_STOR_007_refSlot =
  let r : Ref Bits256 = MkRef 100
  in pure $ refSlot r == 100

||| REQ_STOR_008: offsetRef adds offset
export
test_REQ_STOR_008_offsetRef : IO Bool
test_REQ_STOR_008_offsetRef =
  let r : Ref Bits256 = MkRef 10
      r2 = offsetRef 5 r
  in pure $ refSlot r2 == 15

||| REQ_STOR_009: Pair Storable slotCount
export
test_REQ_STOR_009_pairSlotCount : IO Bool
test_REQ_STOR_009_pairSlotCount = pure $ slotCount {a=(Bits256, Bits256)} == 2

||| REQ_STOR_010: structFieldSlot calculation
export
test_REQ_STOR_010_structFieldSlot : IO Bool
test_REQ_STOR_010_structFieldSlot = pure $ structFieldSlot 100 2 == 102

-- =============================================================================
-- Test Infrastructure
-- =============================================================================

||| Test definition record
public export
record TestDef where
  constructor MkTestDef
  specId : String
  description : String
  testFn : IO Bool

||| Create a test definition
export
test : String -> String -> IO Bool -> TestDef
test = MkTestDef

||| Run a single test and report result
covering
runTest : TestDef -> IO Bool
runTest td = do
  result <- td.testFn
  let status = if result then "[PASS]" else "[FAIL]"
  putStrLn $ status ++ " " ++ td.specId ++ ": " ++ td.description
  pure result

||| Count passed tests
countPassed : List Bool -> Nat
countPassed = length . filter id

||| Run all tests and report summary
covering
runTestSuite : String -> List TestDef -> IO ()
runTestSuite name tests = do
  putStrLn $ "=== " ++ name ++ " Test Suite ==="
  results <- traverse runTest tests
  putStrLn $ "\nResults: " ++ show (countPassed results) ++ "/" ++ show (length results) ++ " passed"
  if countPassed results == length results
    then putStrLn "ALL TESTS PASSED"
    else do
      putStrLn "SOME TESTS FAILED"
      exitFailure

-- =============================================================================
-- Test Collection
-- =============================================================================

||| All EVM compatibility tests with Spec ID associations
public export
allTests : List TestDef
allTests =
  [ test "REQ_EVM_001" "PUSH0 opcode defined" test_REQ_EVM_001_push0Defined
  , test "REQ_EVM_002" "Cancun opcodes defined" test_REQ_EVM_002_cancunOpcodes
  , test "REQ_EVM_003" "London has no PUSH0" test_REQ_EVM_003_londonNoPush0
  , test "REQ_EVM_004" "Shanghai+ has PUSH0" test_REQ_EVM_004_shanghaiHasPush0
  , test "REQ_EVM_005" "Cancun+ transient storage" test_REQ_EVM_005_cancunTransientStorage
  , test "REQ_EVM_006" "All versions enumerated" test_REQ_EVM_006_allVersions
  -- Arithmetic opcodes (0x00-0x0B)
  , test "REQ_EVM_010" "STOP (0x00)" test_REQ_EVM_010_stop
  , test "REQ_EVM_011" "ADD (0x01)" test_REQ_EVM_011_add
  , test "REQ_EVM_012" "MUL (0x02)" test_REQ_EVM_012_mul
  , test "REQ_EVM_013" "SUB (0x03)" test_REQ_EVM_013_sub
  , test "REQ_EVM_014" "DIV (0x04)" test_REQ_EVM_014_div
  , test "REQ_EVM_015" "SDIV (0x05)" test_REQ_EVM_015_sdiv
  , test "REQ_EVM_016" "MOD (0x06)" test_REQ_EVM_016_mod
  , test "REQ_EVM_017" "SMOD (0x07)" test_REQ_EVM_017_smod
  , test "REQ_EVM_018" "ADDMOD (0x08)" test_REQ_EVM_018_addmod
  , test "REQ_EVM_019" "MULMOD (0x09)" test_REQ_EVM_019_mulmod
  , test "REQ_EVM_020" "EXP (0x0a)" test_REQ_EVM_020_exp
  , test "REQ_EVM_021" "SIGNEXTEND (0x0b)" test_REQ_EVM_021_signextend
  -- Comparison opcodes (0x10-0x1D)
  , test "REQ_EVM_022" "LT (0x10)" test_REQ_EVM_022_lt
  , test "REQ_EVM_023" "GT (0x11)" test_REQ_EVM_023_gt
  , test "REQ_EVM_024" "SLT (0x12)" test_REQ_EVM_024_slt
  , test "REQ_EVM_025" "SGT (0x13)" test_REQ_EVM_025_sgt
  , test "REQ_EVM_026" "EQ (0x14)" test_REQ_EVM_026_eq
  , test "REQ_EVM_027" "ISZERO (0x15)" test_REQ_EVM_027_iszero
  , test "REQ_EVM_028" "AND (0x16)" test_REQ_EVM_028_and
  , test "REQ_EVM_029" "OR (0x17)" test_REQ_EVM_029_or
  , test "REQ_EVM_030" "XOR (0x18)" test_REQ_EVM_030_xor
  , test "REQ_EVM_031" "NOT (0x19)" test_REQ_EVM_031_not
  , test "REQ_EVM_032" "BYTE (0x1a)" test_REQ_EVM_032_byte
  , test "REQ_EVM_033" "SHL (0x1b)" test_REQ_EVM_033_shl
  , test "REQ_EVM_034" "SHR (0x1c)" test_REQ_EVM_034_shr
  , test "REQ_EVM_035" "SAR (0x1d)" test_REQ_EVM_035_sar
  -- SHA3 (0x20)
  , test "REQ_EVM_036" "KECCAK256 (0x20)" test_REQ_EVM_036_keccak256
  -- Environmental opcodes (0x30-0x3F)
  , test "REQ_EVM_037" "ADDRESS (0x30)" test_REQ_EVM_037_address
  , test "REQ_EVM_038" "BALANCE (0x31)" test_REQ_EVM_038_balance
  , test "REQ_EVM_039" "ORIGIN (0x32)" test_REQ_EVM_039_origin
  , test "REQ_EVM_040" "CALLER (0x33)" test_REQ_EVM_040_caller
  , test "REQ_EVM_041" "CALLVALUE (0x34)" test_REQ_EVM_041_callvalue
  , test "REQ_EVM_042" "CALLDATALOAD (0x35)" test_REQ_EVM_042_calldataload
  , test "REQ_EVM_043" "CALLDATASIZE (0x36)" test_REQ_EVM_043_calldatasize
  , test "REQ_EVM_044" "CALLDATACOPY (0x37)" test_REQ_EVM_044_calldatacopy
  , test "REQ_EVM_045" "CODESIZE (0x38)" test_REQ_EVM_045_codesize
  , test "REQ_EVM_046" "CODECOPY (0x39)" test_REQ_EVM_046_codecopy
  , test "REQ_EVM_047" "GASPRICE (0x3a)" test_REQ_EVM_047_gasprice
  , test "REQ_EVM_048" "EXTCODESIZE (0x3b)" test_REQ_EVM_048_extcodesize
  , test "REQ_EVM_049" "EXTCODECOPY (0x3c)" test_REQ_EVM_049_extcodecopy
  , test "REQ_EVM_050" "RETURNDATASIZE (0x3d)" test_REQ_EVM_050_returndatasize
  , test "REQ_EVM_051" "RETURNDATACOPY (0x3e)" test_REQ_EVM_051_returndatacopy
  , test "REQ_EVM_052" "EXTCODEHASH (0x3f)" test_REQ_EVM_052_extcodehash
  -- Block information opcodes (0x40-0x4A)
  , test "REQ_EVM_053" "BLOCKHASH (0x40)" test_REQ_EVM_053_blockhash
  , test "REQ_EVM_054" "COINBASE (0x41)" test_REQ_EVM_054_coinbase
  , test "REQ_EVM_055" "TIMESTAMP (0x42)" test_REQ_EVM_055_timestamp
  , test "REQ_EVM_056" "NUMBER (0x43)" test_REQ_EVM_056_number
  , test "REQ_EVM_057" "PREVRANDAO (0x44)" test_REQ_EVM_057_prevrandao
  , test "REQ_EVM_058" "GASLIMIT (0x45)" test_REQ_EVM_058_gaslimit
  , test "REQ_EVM_059" "CHAINID (0x46)" test_REQ_EVM_059_chainid
  , test "REQ_EVM_060" "SELFBALANCE (0x47)" test_REQ_EVM_060_selfbalance
  , test "REQ_EVM_061" "BASEFEE (0x48)" test_REQ_EVM_061_basefee
  , test "REQ_EVM_062" "BLOBHASH (0x49)" test_REQ_EVM_062_blobhash
  , test "REQ_EVM_063" "BLOBBASEFEE (0x4a)" test_REQ_EVM_063_blobbasefee
  -- Stack/Memory/Storage opcodes (0x50-0x5F)
  , test "REQ_EVM_064" "POP (0x50)" test_REQ_EVM_064_pop
  , test "REQ_EVM_065" "MLOAD (0x51)" test_REQ_EVM_065_mload
  , test "REQ_EVM_066" "MSTORE (0x52)" test_REQ_EVM_066_mstore
  , test "REQ_EVM_067" "MSTORE8 (0x53)" test_REQ_EVM_067_mstore8
  , test "REQ_EVM_068" "SLOAD (0x54)" test_REQ_EVM_068_sload
  , test "REQ_EVM_069" "SSTORE (0x55)" test_REQ_EVM_069_sstore
  , test "REQ_EVM_070" "JUMP (0x56)" test_REQ_EVM_070_jump
  , test "REQ_EVM_071" "JUMPI (0x57)" test_REQ_EVM_071_jumpi
  , test "REQ_EVM_072" "PC (0x58)" test_REQ_EVM_072_pc
  , test "REQ_EVM_073" "MSIZE (0x59)" test_REQ_EVM_073_msize
  , test "REQ_EVM_074" "GAS (0x5a)" test_REQ_EVM_074_gas
  , test "REQ_EVM_075" "JUMPDEST (0x5b)" test_REQ_EVM_075_jumpdest
  , test "REQ_EVM_076" "TLOAD (0x5c)" test_REQ_EVM_076_tload
  , test "REQ_EVM_077" "TSTORE (0x5d)" test_REQ_EVM_077_tstore
  , test "REQ_EVM_078" "MCOPY (0x5e)" test_REQ_EVM_078_mcopy
  , test "REQ_EVM_079" "PUSH0 (0x5f)" test_REQ_EVM_079_push0
  -- PUSH opcodes (0x60-0x7F)
  , test "REQ_EVM_080" "PUSH1 (0x60)" test_REQ_EVM_080_push1
  , test "REQ_EVM_081" "PUSH2 (0x61)" test_REQ_EVM_081_push2
  , test "REQ_EVM_082" "PUSH3 (0x62)" test_REQ_EVM_082_push3
  , test "REQ_EVM_083" "PUSH4 (0x63)" test_REQ_EVM_083_push4
  , test "REQ_EVM_084" "PUSH5 (0x64)" test_REQ_EVM_084_push5
  , test "REQ_EVM_085" "PUSH6 (0x65)" test_REQ_EVM_085_push6
  , test "REQ_EVM_086" "PUSH7 (0x66)" test_REQ_EVM_086_push7
  , test "REQ_EVM_087" "PUSH8 (0x67)" test_REQ_EVM_087_push8
  , test "REQ_EVM_088" "PUSH9 (0x68)" test_REQ_EVM_088_push9
  , test "REQ_EVM_089" "PUSH10 (0x69)" test_REQ_EVM_089_push10
  , test "REQ_EVM_090" "PUSH11 (0x6a)" test_REQ_EVM_090_push11
  , test "REQ_EVM_091" "PUSH12 (0x6b)" test_REQ_EVM_091_push12
  , test "REQ_EVM_092" "PUSH13 (0x6c)" test_REQ_EVM_092_push13
  , test "REQ_EVM_093" "PUSH14 (0x6d)" test_REQ_EVM_093_push14
  , test "REQ_EVM_094" "PUSH15 (0x6e)" test_REQ_EVM_094_push15
  , test "REQ_EVM_095" "PUSH16 (0x6f)" test_REQ_EVM_095_push16
  , test "REQ_EVM_096" "PUSH17 (0x70)" test_REQ_EVM_096_push17
  , test "REQ_EVM_097" "PUSH18 (0x71)" test_REQ_EVM_097_push18
  , test "REQ_EVM_098" "PUSH19 (0x72)" test_REQ_EVM_098_push19
  , test "REQ_EVM_099" "PUSH20 (0x73)" test_REQ_EVM_099_push20
  , test "REQ_EVM_100" "PUSH21 (0x74)" test_REQ_EVM_100_push21
  , test "REQ_EVM_101" "PUSH22 (0x75)" test_REQ_EVM_101_push22
  , test "REQ_EVM_102" "PUSH23 (0x76)" test_REQ_EVM_102_push23
  , test "REQ_EVM_103" "PUSH24 (0x77)" test_REQ_EVM_103_push24
  , test "REQ_EVM_104" "PUSH25 (0x78)" test_REQ_EVM_104_push25
  , test "REQ_EVM_105" "PUSH26 (0x79)" test_REQ_EVM_105_push26
  , test "REQ_EVM_106" "PUSH27 (0x7a)" test_REQ_EVM_106_push27
  , test "REQ_EVM_107" "PUSH28 (0x7b)" test_REQ_EVM_107_push28
  , test "REQ_EVM_108" "PUSH29 (0x7c)" test_REQ_EVM_108_push29
  , test "REQ_EVM_109" "PUSH30 (0x7d)" test_REQ_EVM_109_push30
  , test "REQ_EVM_110" "PUSH31 (0x7e)" test_REQ_EVM_110_push31
  , test "REQ_EVM_111" "PUSH32 (0x7f)" test_REQ_EVM_111_push32
  -- DUP opcodes (0x80-0x8F)
  , test "REQ_EVM_112" "DUP1 (0x80)" test_REQ_EVM_112_dup1
  , test "REQ_EVM_113" "DUP2 (0x81)" test_REQ_EVM_113_dup2
  , test "REQ_EVM_114" "DUP3 (0x82)" test_REQ_EVM_114_dup3
  , test "REQ_EVM_115" "DUP4 (0x83)" test_REQ_EVM_115_dup4
  , test "REQ_EVM_116" "DUP5 (0x84)" test_REQ_EVM_116_dup5
  , test "REQ_EVM_117" "DUP6 (0x85)" test_REQ_EVM_117_dup6
  , test "REQ_EVM_118" "DUP7 (0x86)" test_REQ_EVM_118_dup7
  , test "REQ_EVM_119" "DUP8 (0x87)" test_REQ_EVM_119_dup8
  , test "REQ_EVM_120" "DUP9 (0x88)" test_REQ_EVM_120_dup9
  , test "REQ_EVM_121" "DUP10 (0x89)" test_REQ_EVM_121_dup10
  , test "REQ_EVM_122" "DUP11 (0x8a)" test_REQ_EVM_122_dup11
  , test "REQ_EVM_123" "DUP12 (0x8b)" test_REQ_EVM_123_dup12
  , test "REQ_EVM_124" "DUP13 (0x8c)" test_REQ_EVM_124_dup13
  , test "REQ_EVM_125" "DUP14 (0x8d)" test_REQ_EVM_125_dup14
  , test "REQ_EVM_126" "DUP15 (0x8e)" test_REQ_EVM_126_dup15
  , test "REQ_EVM_127" "DUP16 (0x8f)" test_REQ_EVM_127_dup16
  -- SWAP opcodes (0x90-0x9F)
  , test "REQ_EVM_128" "SWAP1 (0x90)" test_REQ_EVM_128_swap1
  , test "REQ_EVM_129" "SWAP2 (0x91)" test_REQ_EVM_129_swap2
  , test "REQ_EVM_130" "SWAP3 (0x92)" test_REQ_EVM_130_swap3
  , test "REQ_EVM_131" "SWAP4 (0x93)" test_REQ_EVM_131_swap4
  , test "REQ_EVM_132" "SWAP5 (0x94)" test_REQ_EVM_132_swap5
  , test "REQ_EVM_133" "SWAP6 (0x95)" test_REQ_EVM_133_swap6
  , test "REQ_EVM_134" "SWAP7 (0x96)" test_REQ_EVM_134_swap7
  , test "REQ_EVM_135" "SWAP8 (0x97)" test_REQ_EVM_135_swap8
  , test "REQ_EVM_136" "SWAP9 (0x98)" test_REQ_EVM_136_swap9
  , test "REQ_EVM_137" "SWAP10 (0x99)" test_REQ_EVM_137_swap10
  , test "REQ_EVM_138" "SWAP11 (0x9a)" test_REQ_EVM_138_swap11
  , test "REQ_EVM_139" "SWAP12 (0x9b)" test_REQ_EVM_139_swap12
  , test "REQ_EVM_140" "SWAP13 (0x9c)" test_REQ_EVM_140_swap13
  , test "REQ_EVM_141" "SWAP14 (0x9d)" test_REQ_EVM_141_swap14
  , test "REQ_EVM_142" "SWAP15 (0x9e)" test_REQ_EVM_142_swap15
  , test "REQ_EVM_143" "SWAP16 (0x9f)" test_REQ_EVM_143_swap16
  -- LOG opcodes (0xA0-0xA4)
  , test "REQ_EVM_144" "LOG0 (0xa0)" test_REQ_EVM_144_log0
  , test "REQ_EVM_145" "LOG1 (0xa1)" test_REQ_EVM_145_log1
  , test "REQ_EVM_146" "LOG2 (0xa2)" test_REQ_EVM_146_log2
  , test "REQ_EVM_147" "LOG3 (0xa3)" test_REQ_EVM_147_log3
  , test "REQ_EVM_148" "LOG4 (0xa4)" test_REQ_EVM_148_log4
  -- System opcodes (0xF0-0xFF)
  , test "REQ_EVM_149" "CREATE (0xf0)" test_REQ_EVM_149_create
  , test "REQ_EVM_150" "CALL (0xf1)" test_REQ_EVM_150_call
  , test "REQ_EVM_151" "CALLCODE (0xf2)" test_REQ_EVM_151_callcode
  , test "REQ_EVM_152" "RETURN (0xf3)" test_REQ_EVM_152_return
  , test "REQ_EVM_153" "DELEGATECALL (0xf4)" test_REQ_EVM_153_delegatecall
  , test "REQ_EVM_154" "CREATE2 (0xf5)" test_REQ_EVM_154_create2
  , test "REQ_EVM_155" "STATICCALL (0xfa)" test_REQ_EVM_155_staticcall
  , test "REQ_EVM_156" "REVERT (0xfd)" test_REQ_EVM_156_revert
  , test "REQ_EVM_157" "INVALID (0xfe)" test_REQ_EVM_157_invalid
  , test "REQ_EVM_158" "SELFDESTRUCT (0xff)" test_REQ_EVM_158_selfdestruct
  -- YulIR tests
  , test "REQ_YUL_001" "showExpr YLit number" test_REQ_YUL_001_showExprLitNum
  , test "REQ_YUL_002" "showExpr YLit hex" test_REQ_YUL_002_showExprLitHex
  , test "REQ_YUL_003" "showExpr YLit bool true" test_REQ_YUL_003_showExprLitBoolTrue
  , test "REQ_YUL_004" "showExpr YLit bool false" test_REQ_YUL_004_showExprLitBoolFalse
  , test "REQ_YUL_005" "showExpr YVar" test_REQ_YUL_005_showExprVar
  , test "REQ_YUL_006" "showExpr YCall no args" test_REQ_YUL_006_showExprCallNoArgs
  , test "REQ_YUL_007" "showExpr YCall with args" test_REQ_YUL_007_showExprCallWithArgs
  , test "REQ_YUL_008" "yulNum builder" test_REQ_YUL_008_yulNum
  , test "REQ_YUL_009" "yulVar builder" test_REQ_YUL_009_yulVar
  , test "REQ_YUL_010" "yulCall builder" test_REQ_YUL_010_yulCall
  , test "REQ_YUL_011" "sload builder" test_REQ_YUL_011_sloadBuilder
  , test "REQ_YUL_012" "sstore builder" test_REQ_YUL_012_sstoreBuilder
  , test "REQ_YUL_013" "mload builder" test_REQ_YUL_013_mloadBuilder
  , test "REQ_YUL_014" "mstore builder" test_REQ_YUL_014_mstoreBuilder
  , test "REQ_YUL_015" "calldataload builder" test_REQ_YUL_015_calldataloadBuilder
  , test "REQ_YUL_016" "revert builder" test_REQ_YUL_016_revertBuilder
  , test "REQ_YUL_017" "yulReturn builder" test_REQ_YUL_017_yulReturnBuilder
  , test "REQ_YUL_018" "collectCallsExpr YLit" test_REQ_YUL_018_collectCallsExprLit
  , test "REQ_YUL_019" "collectCallsExpr YVar" test_REQ_YUL_019_collectCallsExprVar
  , test "REQ_YUL_020" "collectCallsExpr YCall" test_REQ_YUL_020_collectCallsExprCall
  , test "REQ_YUL_021" "SourceLoc show format" test_REQ_YUL_021_sourceLocShow
  , test "REQ_YUL_022" "sourceLocComment format" test_REQ_YUL_022_sourceLocComment
  -- ABI Types tests
  , test "REQ_ABI_001" "abiTypeToString uint256" test_REQ_ABI_001_uint256
  , test "REQ_ABI_002" "abiTypeToString address" test_REQ_ABI_002_address
  , test "REQ_ABI_003" "abiTypeToString bool" test_REQ_ABI_003_bool
  , test "REQ_ABI_004" "abiTypeToString bytes32" test_REQ_ABI_004_bytes32
  , test "REQ_ABI_005" "abiTypeToString string" test_REQ_ABI_005_string
  , test "REQ_ABI_006" "abiTypeToString bytes" test_REQ_ABI_006_bytes
  , test "REQ_ABI_007" "abiTypeToString array" test_REQ_ABI_007_array
  , test "REQ_ABI_008" "abiTypeToString fixed array" test_REQ_ABI_008_fixedArray
  , test "REQ_ABI_009" "abiTypeToString tuple" test_REQ_ABI_009_tuple
  , test "REQ_ABI_010" "mutabilityToString pure" test_REQ_ABI_010_mutPure
  , test "REQ_ABI_011" "mutabilityToString view" test_REQ_ABI_011_mutView
  , test "REQ_ABI_012" "mutabilityToString nonpayable" test_REQ_ABI_012_mutNonpayable
  , test "REQ_ABI_013" "mutabilityToString payable" test_REQ_ABI_013_mutPayable
  , test "REQ_ABI_014" "functionSignature simple" test_REQ_ABI_014_funcSig
  , test "REQ_ABI_015" "functionSignature no params" test_REQ_ABI_015_funcSigNoParams
  , test "REQ_ABI_016" "param constructor" test_REQ_ABI_016_param
  , test "REQ_ABI_017" "anon constructor" test_REQ_ABI_017_anon
  , test "REQ_ABI_018" "indexed constructor" test_REQ_ABI_018_indexed
  , test "REQ_ABI_019" "emptyABI" test_REQ_ABI_019_emptyABI
  , test "REQ_ABI_020" "addFunction" test_REQ_ABI_020_addFunction
  , test "REQ_ABI_021" "uint128" test_REQ_ABI_021_uint128
  , test "REQ_ABI_022" "uint64" test_REQ_ABI_022_uint64
  , test "REQ_ABI_023" "uint32" test_REQ_ABI_023_uint32
  , test "REQ_ABI_024" "uint16" test_REQ_ABI_024_uint16
  , test "REQ_ABI_025" "uint8" test_REQ_ABI_025_uint8
  , test "REQ_ABI_026" "int256" test_REQ_ABI_026_int256
  , test "REQ_ABI_027" "int128" test_REQ_ABI_027_int128
  , test "REQ_ABI_028" "int64" test_REQ_ABI_028_int64
  , test "REQ_ABI_029" "int32" test_REQ_ABI_029_int32
  , test "REQ_ABI_030" "int16" test_REQ_ABI_030_int16
  , test "REQ_ABI_031" "int8" test_REQ_ABI_031_int8
  , test "REQ_ABI_032" "bytes20" test_REQ_ABI_032_bytes20
  , test "REQ_ABI_033" "bytes4" test_REQ_ABI_033_bytes4
  , test "REQ_ABI_034" "eventSignature" test_REQ_ABI_034_eventSig
  , test "REQ_ABI_035" "addEvent" test_REQ_ABI_035_addEvent
  , test "REQ_ABI_036" "addError" test_REQ_ABI_036_addError
  , test "REQ_ABI_037" "tuple empty" test_REQ_ABI_037_tupleEmpty
  , test "REQ_ABI_038" "tuple single" test_REQ_ABI_038_tupleSingle
  -- Storage tests
  , test "REQ_STOR_001" "Bits256 slotCount" test_REQ_STOR_001_bits256SlotCount
  , test "REQ_STOR_002" "Bool slotCount" test_REQ_STOR_002_boolSlotCount
  , test "REQ_STOR_003" "Member slotCount" test_REQ_STOR_003_memberSlotCount
  , test "REQ_STOR_004" "Bits256 toSlots" test_REQ_STOR_004_bits256ToSlots
  , test "REQ_STOR_005" "Bool toSlots True" test_REQ_STOR_005_boolToSlotsTrue
  , test "REQ_STOR_006" "Bool toSlots False" test_REQ_STOR_006_boolToSlotsFalse
  , test "REQ_STOR_007" "refSlot extracts slot" test_REQ_STOR_007_refSlot
  , test "REQ_STOR_008" "offsetRef adds offset" test_REQ_STOR_008_offsetRef
  , test "REQ_STOR_009" "Pair slotCount" test_REQ_STOR_009_pairSlotCount
  , test "REQ_STOR_010" "structFieldSlot" test_REQ_STOR_010_structFieldSlot
  ]

-- =============================================================================
-- Main Entry Point
-- =============================================================================

||| Run all EVM compatibility tests
public export
covering
runAllTests : IO ()
runAllTests = runTestSuite "EVM Compatibility" allTests

covering
main : IO ()
main = runAllTests
