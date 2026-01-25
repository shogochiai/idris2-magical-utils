||| EVM.Opcodes - EVM Opcode definitions
|||
||| Defines all EVM opcodes with their byte values.
||| Reference: https://www.evm.codes/
module EVM.Opcodes

%default covering

||| EVM Opcode enumeration
public export
data Opcode : Type where
  -- Stop and Arithmetic (0x00-0x0b)
  STOP : Opcode
  ADD : Opcode
  MUL : Opcode
  SUB : Opcode
  DIV : Opcode
  SDIV : Opcode
  MOD : Opcode
  SMOD : Opcode
  ADDMOD : Opcode
  MULMOD : Opcode
  EXP : Opcode
  SIGNEXTEND : Opcode

  -- Comparison & Bitwise (0x10-0x1d)
  LT : Opcode
  GT : Opcode
  SLT : Opcode
  SGT : Opcode
  EQ : Opcode
  ISZERO : Opcode
  AND : Opcode
  OR : Opcode
  XOR : Opcode
  NOT : Opcode
  BYTE : Opcode
  SHL : Opcode
  SHR : Opcode
  SAR : Opcode

  -- Keccak256 (0x20)
  KECCAK256 : Opcode

  -- Environmental (0x30-0x3f)
  ADDRESS : Opcode
  BALANCE : Opcode
  ORIGIN : Opcode
  CALLER : Opcode
  CALLVALUE : Opcode
  CALLDATALOAD : Opcode
  CALLDATASIZE : Opcode
  CALLDATACOPY : Opcode
  CODESIZE : Opcode
  CODECOPY : Opcode
  GASPRICE : Opcode
  EXTCODESIZE : Opcode
  EXTCODECOPY : Opcode
  RETURNDATASIZE : Opcode
  RETURNDATACOPY : Opcode
  EXTCODEHASH : Opcode

  -- Block Information (0x40-0x4a)
  BLOCKHASH : Opcode
  COINBASE : Opcode
  TIMESTAMP : Opcode
  NUMBER : Opcode
  PREVRANDAO : Opcode
  GASLIMIT : Opcode
  CHAINID : Opcode
  SELFBALANCE : Opcode
  BASEFEE : Opcode
  BLOBHASH : Opcode
  BLOBBASEFEE : Opcode

  -- Stack, Memory, Storage, Flow (0x50-0x5f)
  POP : Opcode
  MLOAD : Opcode
  MSTORE : Opcode
  MSTORE8 : Opcode
  SLOAD : Opcode
  SSTORE : Opcode
  JUMP : Opcode
  JUMPI : Opcode
  PC : Opcode
  MSIZE : Opcode
  GAS : Opcode
  JUMPDEST : Opcode
  TLOAD : Opcode
  TSTORE : Opcode
  MCOPY : Opcode

  -- Push operations (0x5f-0x7f)
  PUSH0 : Opcode
  PUSH1 : Opcode
  PUSH2 : Opcode
  PUSH3 : Opcode
  PUSH4 : Opcode
  PUSH5 : Opcode
  PUSH6 : Opcode
  PUSH7 : Opcode
  PUSH8 : Opcode
  PUSH9 : Opcode
  PUSH10 : Opcode
  PUSH11 : Opcode
  PUSH12 : Opcode
  PUSH13 : Opcode
  PUSH14 : Opcode
  PUSH15 : Opcode
  PUSH16 : Opcode
  PUSH17 : Opcode
  PUSH18 : Opcode
  PUSH19 : Opcode
  PUSH20 : Opcode
  PUSH21 : Opcode
  PUSH22 : Opcode
  PUSH23 : Opcode
  PUSH24 : Opcode
  PUSH25 : Opcode
  PUSH26 : Opcode
  PUSH27 : Opcode
  PUSH28 : Opcode
  PUSH29 : Opcode
  PUSH30 : Opcode
  PUSH31 : Opcode
  PUSH32 : Opcode

  -- Dup operations (0x80-0x8f)
  DUP1 : Opcode
  DUP2 : Opcode
  DUP3 : Opcode
  DUP4 : Opcode
  DUP5 : Opcode
  DUP6 : Opcode
  DUP7 : Opcode
  DUP8 : Opcode
  DUP9 : Opcode
  DUP10 : Opcode
  DUP11 : Opcode
  DUP12 : Opcode
  DUP13 : Opcode
  DUP14 : Opcode
  DUP15 : Opcode
  DUP16 : Opcode

  -- Swap operations (0x90-0x9f)
  SWAP1 : Opcode
  SWAP2 : Opcode
  SWAP3 : Opcode
  SWAP4 : Opcode
  SWAP5 : Opcode
  SWAP6 : Opcode
  SWAP7 : Opcode
  SWAP8 : Opcode
  SWAP9 : Opcode
  SWAP10 : Opcode
  SWAP11 : Opcode
  SWAP12 : Opcode
  SWAP13 : Opcode
  SWAP14 : Opcode
  SWAP15 : Opcode
  SWAP16 : Opcode

  -- Log operations (0xa0-0xa4)
  LOG0 : Opcode
  LOG1 : Opcode
  LOG2 : Opcode
  LOG3 : Opcode
  LOG4 : Opcode

  -- System operations (0xf0-0xff)
  CREATE : Opcode
  CALL : Opcode
  CALLCODE : Opcode
  RETURN : Opcode
  DELEGATECALL : Opcode
  CREATE2 : Opcode
  STATICCALL : Opcode
  REVERT : Opcode
  INVALID : Opcode
  SELFDESTRUCT : Opcode

  -- Unknown/Invalid byte
  UNKNOWN : Bits8 -> Opcode

||| Decode byte to opcode
public export
fromByte : Bits8 -> Opcode
fromByte 0x00 = STOP
fromByte 0x01 = ADD
fromByte 0x02 = MUL
fromByte 0x03 = SUB
fromByte 0x04 = DIV
fromByte 0x05 = SDIV
fromByte 0x06 = MOD
fromByte 0x07 = SMOD
fromByte 0x08 = ADDMOD
fromByte 0x09 = MULMOD
fromByte 0x0a = EXP
fromByte 0x0b = SIGNEXTEND
fromByte 0x10 = LT
fromByte 0x11 = GT
fromByte 0x12 = SLT
fromByte 0x13 = SGT
fromByte 0x14 = EQ
fromByte 0x15 = ISZERO
fromByte 0x16 = AND
fromByte 0x17 = OR
fromByte 0x18 = XOR
fromByte 0x19 = NOT
fromByte 0x1a = BYTE
fromByte 0x1b = SHL
fromByte 0x1c = SHR
fromByte 0x1d = SAR
fromByte 0x20 = KECCAK256
fromByte 0x30 = ADDRESS
fromByte 0x31 = BALANCE
fromByte 0x32 = ORIGIN
fromByte 0x33 = CALLER
fromByte 0x34 = CALLVALUE
fromByte 0x35 = CALLDATALOAD
fromByte 0x36 = CALLDATASIZE
fromByte 0x37 = CALLDATACOPY
fromByte 0x38 = CODESIZE
fromByte 0x39 = CODECOPY
fromByte 0x3a = GASPRICE
fromByte 0x3b = EXTCODESIZE
fromByte 0x3c = EXTCODECOPY
fromByte 0x3d = RETURNDATASIZE
fromByte 0x3e = RETURNDATACOPY
fromByte 0x3f = EXTCODEHASH
fromByte 0x40 = BLOCKHASH
fromByte 0x41 = COINBASE
fromByte 0x42 = TIMESTAMP
fromByte 0x43 = NUMBER
fromByte 0x44 = PREVRANDAO
fromByte 0x45 = GASLIMIT
fromByte 0x46 = CHAINID
fromByte 0x47 = SELFBALANCE
fromByte 0x48 = BASEFEE
fromByte 0x49 = BLOBHASH
fromByte 0x4a = BLOBBASEFEE
fromByte 0x50 = POP
fromByte 0x51 = MLOAD
fromByte 0x52 = MSTORE
fromByte 0x53 = MSTORE8
fromByte 0x54 = SLOAD
fromByte 0x55 = SSTORE
fromByte 0x56 = JUMP
fromByte 0x57 = JUMPI
fromByte 0x58 = PC
fromByte 0x59 = MSIZE
fromByte 0x5a = GAS
fromByte 0x5b = JUMPDEST
fromByte 0x5c = TLOAD
fromByte 0x5d = TSTORE
fromByte 0x5e = MCOPY
fromByte 0x5f = PUSH0
fromByte 0x60 = PUSH1
fromByte 0x61 = PUSH2
fromByte 0x62 = PUSH3
fromByte 0x63 = PUSH4
fromByte 0x64 = PUSH5
fromByte 0x65 = PUSH6
fromByte 0x66 = PUSH7
fromByte 0x67 = PUSH8
fromByte 0x68 = PUSH9
fromByte 0x69 = PUSH10
fromByte 0x6a = PUSH11
fromByte 0x6b = PUSH12
fromByte 0x6c = PUSH13
fromByte 0x6d = PUSH14
fromByte 0x6e = PUSH15
fromByte 0x6f = PUSH16
fromByte 0x70 = PUSH17
fromByte 0x71 = PUSH18
fromByte 0x72 = PUSH19
fromByte 0x73 = PUSH20
fromByte 0x74 = PUSH21
fromByte 0x75 = PUSH22
fromByte 0x76 = PUSH23
fromByte 0x77 = PUSH24
fromByte 0x78 = PUSH25
fromByte 0x79 = PUSH26
fromByte 0x7a = PUSH27
fromByte 0x7b = PUSH28
fromByte 0x7c = PUSH29
fromByte 0x7d = PUSH30
fromByte 0x7e = PUSH31
fromByte 0x7f = PUSH32
fromByte 0x80 = DUP1
fromByte 0x81 = DUP2
fromByte 0x82 = DUP3
fromByte 0x83 = DUP4
fromByte 0x84 = DUP5
fromByte 0x85 = DUP6
fromByte 0x86 = DUP7
fromByte 0x87 = DUP8
fromByte 0x88 = DUP9
fromByte 0x89 = DUP10
fromByte 0x8a = DUP11
fromByte 0x8b = DUP12
fromByte 0x8c = DUP13
fromByte 0x8d = DUP14
fromByte 0x8e = DUP15
fromByte 0x8f = DUP16
fromByte 0x90 = SWAP1
fromByte 0x91 = SWAP2
fromByte 0x92 = SWAP3
fromByte 0x93 = SWAP4
fromByte 0x94 = SWAP5
fromByte 0x95 = SWAP6
fromByte 0x96 = SWAP7
fromByte 0x97 = SWAP8
fromByte 0x98 = SWAP9
fromByte 0x99 = SWAP10
fromByte 0x9a = SWAP11
fromByte 0x9b = SWAP12
fromByte 0x9c = SWAP13
fromByte 0x9d = SWAP14
fromByte 0x9e = SWAP15
fromByte 0x9f = SWAP16
fromByte 0xa0 = LOG0
fromByte 0xa1 = LOG1
fromByte 0xa2 = LOG2
fromByte 0xa3 = LOG3
fromByte 0xa4 = LOG4
fromByte 0xf0 = CREATE
fromByte 0xf1 = CALL
fromByte 0xf2 = CALLCODE
fromByte 0xf3 = RETURN
fromByte 0xf4 = DELEGATECALL
fromByte 0xf5 = CREATE2
fromByte 0xfa = STATICCALL
fromByte 0xfd = REVERT
fromByte 0xfe = INVALID
fromByte 0xff = SELFDESTRUCT
fromByte b = UNKNOWN b

||| Get push size (number of bytes to read after PUSH opcode)
public export
pushSize : Opcode -> Nat
pushSize PUSH0 = 0
pushSize PUSH1 = 1
pushSize PUSH2 = 2
pushSize PUSH3 = 3
pushSize PUSH4 = 4
pushSize PUSH5 = 5
pushSize PUSH6 = 6
pushSize PUSH7 = 7
pushSize PUSH8 = 8
pushSize PUSH9 = 9
pushSize PUSH10 = 10
pushSize PUSH11 = 11
pushSize PUSH12 = 12
pushSize PUSH13 = 13
pushSize PUSH14 = 14
pushSize PUSH15 = 15
pushSize PUSH16 = 16
pushSize PUSH17 = 17
pushSize PUSH18 = 18
pushSize PUSH19 = 19
pushSize PUSH20 = 20
pushSize PUSH21 = 21
pushSize PUSH22 = 22
pushSize PUSH23 = 23
pushSize PUSH24 = 24
pushSize PUSH25 = 25
pushSize PUSH26 = 26
pushSize PUSH27 = 27
pushSize PUSH28 = 28
pushSize PUSH29 = 29
pushSize PUSH30 = 30
pushSize PUSH31 = 31
pushSize PUSH32 = 32
pushSize _ = 0

||| Check if opcode is a PUSH
public export
isPush : Opcode -> Bool
isPush PUSH0 = True
isPush PUSH1 = True
isPush PUSH2 = True
isPush PUSH3 = True
isPush PUSH4 = True
isPush PUSH5 = True
isPush PUSH6 = True
isPush PUSH7 = True
isPush PUSH8 = True
isPush PUSH9 = True
isPush PUSH10 = True
isPush PUSH11 = True
isPush PUSH12 = True
isPush PUSH13 = True
isPush PUSH14 = True
isPush PUSH15 = True
isPush PUSH16 = True
isPush PUSH17 = True
isPush PUSH18 = True
isPush PUSH19 = True
isPush PUSH20 = True
isPush PUSH21 = True
isPush PUSH22 = True
isPush PUSH23 = True
isPush PUSH24 = True
isPush PUSH25 = True
isPush PUSH26 = True
isPush PUSH27 = True
isPush PUSH28 = True
isPush PUSH29 = True
isPush PUSH30 = True
isPush PUSH31 = True
isPush PUSH32 = True
isPush _ = False

||| Check if opcode terminates execution
public export
isTerminating : Opcode -> Bool
isTerminating STOP = True
isTerminating RETURN = True
isTerminating REVERT = True
isTerminating INVALID = True
isTerminating SELFDESTRUCT = True
isTerminating _ = False

||| Show instance
public export
Show Opcode where
  show STOP = "STOP"
  show ADD = "ADD"
  show MUL = "MUL"
  show SUB = "SUB"
  show DIV = "DIV"
  show SDIV = "SDIV"
  show MOD = "MOD"
  show SMOD = "SMOD"
  show ADDMOD = "ADDMOD"
  show MULMOD = "MULMOD"
  show EXP = "EXP"
  show SIGNEXTEND = "SIGNEXTEND"
  show LT = "LT"
  show GT = "GT"
  show SLT = "SLT"
  show SGT = "SGT"
  show EQ = "EQ"
  show ISZERO = "ISZERO"
  show AND = "AND"
  show OR = "OR"
  show XOR = "XOR"
  show NOT = "NOT"
  show BYTE = "BYTE"
  show SHL = "SHL"
  show SHR = "SHR"
  show SAR = "SAR"
  show KECCAK256 = "KECCAK256"
  show ADDRESS = "ADDRESS"
  show BALANCE = "BALANCE"
  show ORIGIN = "ORIGIN"
  show CALLER = "CALLER"
  show CALLVALUE = "CALLVALUE"
  show CALLDATALOAD = "CALLDATALOAD"
  show CALLDATASIZE = "CALLDATASIZE"
  show CALLDATACOPY = "CALLDATACOPY"
  show CODESIZE = "CODESIZE"
  show CODECOPY = "CODECOPY"
  show GASPRICE = "GASPRICE"
  show EXTCODESIZE = "EXTCODESIZE"
  show EXTCODECOPY = "EXTCODECOPY"
  show RETURNDATASIZE = "RETURNDATASIZE"
  show RETURNDATACOPY = "RETURNDATACOPY"
  show EXTCODEHASH = "EXTCODEHASH"
  show BLOCKHASH = "BLOCKHASH"
  show COINBASE = "COINBASE"
  show TIMESTAMP = "TIMESTAMP"
  show NUMBER = "NUMBER"
  show PREVRANDAO = "PREVRANDAO"
  show GASLIMIT = "GASLIMIT"
  show CHAINID = "CHAINID"
  show SELFBALANCE = "SELFBALANCE"
  show BASEFEE = "BASEFEE"
  show BLOBHASH = "BLOBHASH"
  show BLOBBASEFEE = "BLOBBASEFEE"
  show POP = "POP"
  show MLOAD = "MLOAD"
  show MSTORE = "MSTORE"
  show MSTORE8 = "MSTORE8"
  show SLOAD = "SLOAD"
  show SSTORE = "SSTORE"
  show JUMP = "JUMP"
  show JUMPI = "JUMPI"
  show PC = "PC"
  show MSIZE = "MSIZE"
  show GAS = "GAS"
  show JUMPDEST = "JUMPDEST"
  show TLOAD = "TLOAD"
  show TSTORE = "TSTORE"
  show MCOPY = "MCOPY"
  show PUSH0 = "PUSH0"
  show PUSH1 = "PUSH1"
  show PUSH2 = "PUSH2"
  show PUSH3 = "PUSH3"
  show PUSH4 = "PUSH4"
  show PUSH5 = "PUSH5"
  show PUSH6 = "PUSH6"
  show PUSH7 = "PUSH7"
  show PUSH8 = "PUSH8"
  show PUSH9 = "PUSH9"
  show PUSH10 = "PUSH10"
  show PUSH11 = "PUSH11"
  show PUSH12 = "PUSH12"
  show PUSH13 = "PUSH13"
  show PUSH14 = "PUSH14"
  show PUSH15 = "PUSH15"
  show PUSH16 = "PUSH16"
  show PUSH17 = "PUSH17"
  show PUSH18 = "PUSH18"
  show PUSH19 = "PUSH19"
  show PUSH20 = "PUSH20"
  show PUSH21 = "PUSH21"
  show PUSH22 = "PUSH22"
  show PUSH23 = "PUSH23"
  show PUSH24 = "PUSH24"
  show PUSH25 = "PUSH25"
  show PUSH26 = "PUSH26"
  show PUSH27 = "PUSH27"
  show PUSH28 = "PUSH28"
  show PUSH29 = "PUSH29"
  show PUSH30 = "PUSH30"
  show PUSH31 = "PUSH31"
  show PUSH32 = "PUSH32"
  show DUP1 = "DUP1"
  show DUP2 = "DUP2"
  show DUP3 = "DUP3"
  show DUP4 = "DUP4"
  show DUP5 = "DUP5"
  show DUP6 = "DUP6"
  show DUP7 = "DUP7"
  show DUP8 = "DUP8"
  show DUP9 = "DUP9"
  show DUP10 = "DUP10"
  show DUP11 = "DUP11"
  show DUP12 = "DUP12"
  show DUP13 = "DUP13"
  show DUP14 = "DUP14"
  show DUP15 = "DUP15"
  show DUP16 = "DUP16"
  show SWAP1 = "SWAP1"
  show SWAP2 = "SWAP2"
  show SWAP3 = "SWAP3"
  show SWAP4 = "SWAP4"
  show SWAP5 = "SWAP5"
  show SWAP6 = "SWAP6"
  show SWAP7 = "SWAP7"
  show SWAP8 = "SWAP8"
  show SWAP9 = "SWAP9"
  show SWAP10 = "SWAP10"
  show SWAP11 = "SWAP11"
  show SWAP12 = "SWAP12"
  show SWAP13 = "SWAP13"
  show SWAP14 = "SWAP14"
  show SWAP15 = "SWAP15"
  show SWAP16 = "SWAP16"
  show LOG0 = "LOG0"
  show LOG1 = "LOG1"
  show LOG2 = "LOG2"
  show LOG3 = "LOG3"
  show LOG4 = "LOG4"
  show CREATE = "CREATE"
  show CALL = "CALL"
  show CALLCODE = "CALLCODE"
  show RETURN = "RETURN"
  show DELEGATECALL = "DELEGATECALL"
  show CREATE2 = "CREATE2"
  show STATICCALL = "STATICCALL"
  show REVERT = "REVERT"
  show INVALID = "INVALID"
  show SELFDESTRUCT = "SELFDESTRUCT"
  show (UNKNOWN b) = "UNKNOWN(" ++ show b ++ ")"

||| Eq instance for Opcode (derived via Show)
public export
Eq Opcode where
  op1 == op2 = show op1 == show op2
