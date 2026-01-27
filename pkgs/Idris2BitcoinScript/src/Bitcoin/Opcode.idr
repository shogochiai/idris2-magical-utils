||| Bitcoin Opcodes
|||
||| Complete Bitcoin Script opcode definitions.
module Bitcoin.Opcode

%default total

-- =============================================================================
-- Opcode Definitions
-- =============================================================================

||| Bitcoin Script opcodes
public export
data Opcode : Type where
  -- Constants
  OP_0           : Opcode  -- 0x00
  OP_FALSE       : Opcode  -- 0x00 (alias)
  OP_PUSHDATA1   : Opcode  -- 0x4c
  OP_PUSHDATA2   : Opcode  -- 0x4d
  OP_PUSHDATA4   : Opcode  -- 0x4e
  OP_1NEGATE     : Opcode  -- 0x4f
  OP_1           : Opcode  -- 0x51
  OP_TRUE        : Opcode  -- 0x51 (alias)
  OP_2           : Opcode  -- 0x52
  OP_3           : Opcode  -- 0x53
  OP_4           : Opcode  -- 0x54
  OP_5           : Opcode  -- 0x55
  OP_6           : Opcode  -- 0x56
  OP_7           : Opcode  -- 0x57
  OP_8           : Opcode  -- 0x58
  OP_9           : Opcode  -- 0x59
  OP_10          : Opcode  -- 0x5a
  OP_11          : Opcode  -- 0x5b
  OP_12          : Opcode  -- 0x5c
  OP_13          : Opcode  -- 0x5d
  OP_14          : Opcode  -- 0x5e
  OP_15          : Opcode  -- 0x5f
  OP_16          : Opcode  -- 0x60

  -- Flow control
  OP_NOP         : Opcode  -- 0x61
  OP_IF          : Opcode  -- 0x63
  OP_NOTIF       : Opcode  -- 0x64
  OP_ELSE        : Opcode  -- 0x67
  OP_ENDIF       : Opcode  -- 0x68
  OP_VERIFY      : Opcode  -- 0x69
  OP_RETURN      : Opcode  -- 0x6a

  -- Stack
  OP_TOALTSTACK  : Opcode  -- 0x6b
  OP_FROMALTSTACK: Opcode  -- 0x6c
  OP_IFDUP       : Opcode  -- 0x73
  OP_DEPTH       : Opcode  -- 0x74
  OP_DROP        : Opcode  -- 0x75
  OP_DUP         : Opcode  -- 0x76
  OP_NIP         : Opcode  -- 0x77
  OP_OVER        : Opcode  -- 0x78
  OP_PICK        : Opcode  -- 0x79
  OP_ROLL        : Opcode  -- 0x7a
  OP_ROT         : Opcode  -- 0x7b
  OP_SWAP        : Opcode  -- 0x7c
  OP_TUCK        : Opcode  -- 0x7d
  OP_2DROP       : Opcode  -- 0x6d
  OP_2DUP        : Opcode  -- 0x6e
  OP_3DUP        : Opcode  -- 0x6f
  OP_2OVER       : Opcode  -- 0x70
  OP_2ROT        : Opcode  -- 0x71
  OP_2SWAP       : Opcode  -- 0x72

  -- Splice (OP_CAT enabled with OP_CAT activation)
  OP_CAT         : Opcode  -- 0x7e (requires OP_CAT soft fork)
  OP_SPLIT       : Opcode  -- 0x7f
  OP_NUM2BIN     : Opcode  -- 0x80 (BCH: convert int to n-byte LE)
  OP_BIN2NUM     : Opcode  -- 0x81 (BCH: convert LE bytes to int)
  OP_SIZE        : Opcode  -- 0x82

  -- Bitwise logic
  OP_INVERT      : Opcode  -- 0x83
  OP_AND         : Opcode  -- 0x84
  OP_OR          : Opcode  -- 0x85
  OP_XOR         : Opcode  -- 0x86
  OP_EQUAL       : Opcode  -- 0x87
  OP_EQUALVERIFY : Opcode  -- 0x88

  -- Arithmetic
  OP_1ADD        : Opcode  -- 0x8b
  OP_1SUB        : Opcode  -- 0x8c
  OP_NEGATE      : Opcode  -- 0x8f
  OP_ABS         : Opcode  -- 0x90
  OP_NOT         : Opcode  -- 0x91
  OP_0NOTEQUAL   : Opcode  -- 0x92
  OP_ADD         : Opcode  -- 0x93
  OP_SUB         : Opcode  -- 0x94
  OP_MUL         : Opcode  -- 0x95
  OP_DIV         : Opcode  -- 0x96
  OP_MOD         : Opcode  -- 0x97
  OP_BOOLAND     : Opcode  -- 0x9a
  OP_BOOLOR      : Opcode  -- 0x9b
  OP_NUMEQUAL    : Opcode  -- 0x9c
  OP_NUMEQUALVERIFY : Opcode -- 0x9d
  OP_NUMNOTEQUAL : Opcode  -- 0x9e
  OP_LESSTHAN    : Opcode  -- 0x9f
  OP_GREATERTHAN : Opcode  -- 0xa0
  OP_LESSTHANOREQUAL : Opcode -- 0xa1
  OP_GREATERTHANOREQUAL : Opcode -- 0xa2
  OP_MIN         : Opcode  -- 0xa3
  OP_MAX         : Opcode  -- 0xa4
  OP_WITHIN      : Opcode  -- 0xa5

  -- Crypto
  OP_RIPEMD160   : Opcode  -- 0xa6
  OP_SHA1        : Opcode  -- 0xa7
  OP_SHA256      : Opcode  -- 0xa8
  OP_HASH160     : Opcode  -- 0xa9
  OP_HASH256     : Opcode  -- 0xaa
  OP_CODESEPARATOR : Opcode -- 0xab
  OP_CHECKSIG    : Opcode  -- 0xac
  OP_CHECKSIGVERIFY : Opcode -- 0xad
  OP_CHECKMULTISIG : Opcode -- 0xae
  OP_CHECKMULTISIGVERIFY : Opcode -- 0xaf

  -- Locktime
  OP_CHECKLOCKTIMEVERIFY : Opcode -- 0xb1
  OP_CHECKSEQUENCEVERIFY : Opcode -- 0xb2

  -- Taproot
  OP_CHECKSIGADD : Opcode  -- 0xba

-- =============================================================================
-- Opcode to Byte
-- =============================================================================

||| Convert opcode to byte value
public export
opcodeByte : Opcode -> Int
opcodeByte OP_0 = 0x00
opcodeByte OP_FALSE = 0x00
opcodeByte OP_PUSHDATA1 = 0x4c
opcodeByte OP_PUSHDATA2 = 0x4d
opcodeByte OP_PUSHDATA4 = 0x4e
opcodeByte OP_1NEGATE = 0x4f
opcodeByte OP_1 = 0x51
opcodeByte OP_TRUE = 0x51
opcodeByte OP_2 = 0x52
opcodeByte OP_3 = 0x53
opcodeByte OP_4 = 0x54
opcodeByte OP_5 = 0x55
opcodeByte OP_6 = 0x56
opcodeByte OP_7 = 0x57
opcodeByte OP_8 = 0x58
opcodeByte OP_9 = 0x59
opcodeByte OP_10 = 0x5a
opcodeByte OP_11 = 0x5b
opcodeByte OP_12 = 0x5c
opcodeByte OP_13 = 0x5d
opcodeByte OP_14 = 0x5e
opcodeByte OP_15 = 0x5f
opcodeByte OP_16 = 0x60
opcodeByte OP_NOP = 0x61
opcodeByte OP_IF = 0x63
opcodeByte OP_NOTIF = 0x64
opcodeByte OP_ELSE = 0x67
opcodeByte OP_ENDIF = 0x68
opcodeByte OP_VERIFY = 0x69
opcodeByte OP_RETURN = 0x6a
opcodeByte OP_TOALTSTACK = 0x6b
opcodeByte OP_FROMALTSTACK = 0x6c
opcodeByte OP_2DROP = 0x6d
opcodeByte OP_2DUP = 0x6e
opcodeByte OP_3DUP = 0x6f
opcodeByte OP_2OVER = 0x70
opcodeByte OP_2ROT = 0x71
opcodeByte OP_2SWAP = 0x72
opcodeByte OP_IFDUP = 0x73
opcodeByte OP_DEPTH = 0x74
opcodeByte OP_DROP = 0x75
opcodeByte OP_DUP = 0x76
opcodeByte OP_NIP = 0x77
opcodeByte OP_OVER = 0x78
opcodeByte OP_PICK = 0x79
opcodeByte OP_ROLL = 0x7a
opcodeByte OP_ROT = 0x7b
opcodeByte OP_SWAP = 0x7c
opcodeByte OP_TUCK = 0x7d
opcodeByte OP_CAT = 0x7e
opcodeByte OP_SPLIT = 0x7f
opcodeByte OP_NUM2BIN = 0x80
opcodeByte OP_BIN2NUM = 0x81
opcodeByte OP_SIZE = 0x82
opcodeByte OP_INVERT = 0x83
opcodeByte OP_AND = 0x84
opcodeByte OP_OR = 0x85
opcodeByte OP_XOR = 0x86
opcodeByte OP_EQUAL = 0x87
opcodeByte OP_EQUALVERIFY = 0x88
opcodeByte OP_1ADD = 0x8b
opcodeByte OP_1SUB = 0x8c
opcodeByte OP_NEGATE = 0x8f
opcodeByte OP_ABS = 0x90
opcodeByte OP_NOT = 0x91
opcodeByte OP_0NOTEQUAL = 0x92
opcodeByte OP_ADD = 0x93
opcodeByte OP_SUB = 0x94
opcodeByte OP_MUL = 0x95
opcodeByte OP_DIV = 0x96
opcodeByte OP_MOD = 0x97
opcodeByte OP_BOOLAND = 0x9a
opcodeByte OP_BOOLOR = 0x9b
opcodeByte OP_NUMEQUAL = 0x9c
opcodeByte OP_NUMEQUALVERIFY = 0x9d
opcodeByte OP_NUMNOTEQUAL = 0x9e
opcodeByte OP_LESSTHAN = 0x9f
opcodeByte OP_GREATERTHAN = 0xa0
opcodeByte OP_LESSTHANOREQUAL = 0xa1
opcodeByte OP_GREATERTHANOREQUAL = 0xa2
opcodeByte OP_MIN = 0xa3
opcodeByte OP_MAX = 0xa4
opcodeByte OP_WITHIN = 0xa5
opcodeByte OP_RIPEMD160 = 0xa6
opcodeByte OP_SHA1 = 0xa7
opcodeByte OP_SHA256 = 0xa8
opcodeByte OP_HASH160 = 0xa9
opcodeByte OP_HASH256 = 0xaa
opcodeByte OP_CODESEPARATOR = 0xab
opcodeByte OP_CHECKSIG = 0xac
opcodeByte OP_CHECKSIGVERIFY = 0xad
opcodeByte OP_CHECKMULTISIG = 0xae
opcodeByte OP_CHECKMULTISIGVERIFY = 0xaf
opcodeByte OP_CHECKLOCKTIMEVERIFY = 0xb1
opcodeByte OP_CHECKSEQUENCEVERIFY = 0xb2
opcodeByte OP_CHECKSIGADD = 0xba
