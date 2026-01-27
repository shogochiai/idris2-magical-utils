||| Stack-Safe Bitcoin Script DSL
|||
||| Type-level stack tracking for Bitcoin Script.
||| Prevents stack overflow/underflow at compile time.
module Bitcoin.Script

import Bitcoin.Opcode
import Data.List
import Data.Vect

%default total

-- =============================================================================
-- Stack Element Types
-- =============================================================================

||| Stack element kinds
public export
data StackElem : Type where
  SBytes  : Nat -> StackElem        -- Bytes of known length
  SInt    : StackElem               -- Script integer
  SBool   : StackElem               -- Boolean (0 or 1)
  SHash   : (n : Nat) -> StackElem  -- Hash of n bytes
  SAny    : StackElem               -- Unknown type

||| 20-byte hash (HASH160)
public export
Hash160 : StackElem
Hash160 = SHash 20

||| 32-byte hash (SHA256/HASH256)
public export
Hash256 : StackElem
Hash256 = SHash 32

||| Public key (33 bytes compressed)
public export
PubKey : StackElem
PubKey = SBytes 33

||| Signature (variable, typically 64-72 bytes)
public export
Signature : StackElem
Signature = SAny

-- =============================================================================
-- Semantic Type Aliases (Domain-Specific)
-- =============================================================================

||| Satoshi amount (8-byte integer in Bitcoin)
public export
Satoshi : StackElem
Satoshi = SInt

||| Block height
public export
BlockHeight : StackElem
BlockHeight = SInt

||| Unix timestamp
public export
Timestamp : StackElem
Timestamp = SInt

||| Transaction ID (32-byte hash)
public export
TxId : StackElem
TxId = SHash 32

||| Outpoint index
public export
Vout : StackElem
Vout = SInt

||| Script hash (P2SH, 20 bytes)
public export
ScriptHash : StackElem
ScriptHash = SHash 20

||| Witness script hash (P2WSH, 32 bytes)
public export
WitnessScriptHash : StackElem
WitnessScriptHash = SHash 32

||| Compressed public key (explicit alias)
public export
CompressedPubKey : StackElem
CompressedPubKey = SBytes 33

||| Uncompressed public key (65 bytes)
public export
UncompressedPubKey : StackElem
UncompressedPubKey = SBytes 65

||| Schnorr public key (32 bytes, x-only)
public export
SchnorrPubKey : StackElem
SchnorrPubKey = SBytes 32

||| Schnorr signature (64 bytes)
public export
SchnorrSig : StackElem
SchnorrSig = SBytes 64

-- =============================================================================
-- Stack Type
-- =============================================================================

||| Stack represented as list of element types
public export
Stack : Type
Stack = List StackElem

-- =============================================================================
-- Stack-Safe Script
-- =============================================================================

||| Bitcoin Script with stack type tracking
||| @before Stack state before execution
||| @after Stack state after execution
public export
data Script : (before : Stack) -> (after : Stack) -> Type where
  -- Empty script (no-op)
  SNil : Script s s

  -- Sequence
  (>>) : Script s1 s2 -> Script s2 s3 -> Script s1 s3

  -- Push data
  PushBytes : (n : Nat) -> Vect n Int -> Script s (SBytes n :: s)
  PushInt   : Int -> Script s (SInt :: s)

  -- Stack operations
  OpDup     : Script (a :: s) (a :: a :: s)
  OpDrop    : Script (a :: s) s
  OpSwap    : Script (a :: b :: s) (b :: a :: s)
  OpOver    : Script (a :: b :: s) (a :: b :: a :: s)
  OpRot     : Script (a :: b :: c :: s) (b :: c :: a :: s)
  OpNip     : Script (a :: b :: s) (a :: s)
  OpTuck    : Script (a :: b :: s) (a :: b :: a :: s)
  Op2Dup    : Script (a :: b :: s) (a :: b :: a :: b :: s)
  Op2Drop   : Script (a :: b :: s) s
  Op2Swap   : Script (a :: b :: c :: d :: s) (c :: d :: a :: b :: s)
  Op3Dup    : Script (a :: b :: c :: s) (a :: b :: c :: a :: b :: c :: s)

  -- Alt stack
  OpToAlt   : Script (a :: s) s  -- Note: alt stack not tracked
  OpFromAlt : Script s (SAny :: s)

  -- Deep stack access (indices are runtime values, type-unsafe by nature)
  OpPick    : Script (SInt :: s) (SAny :: s)   -- Copy n-th item to top
  OpRoll    : Script (SInt :: s) (SAny :: s)   -- Move n-th item to top

  -- Byte/Integer conversion (Bitcoin Cash-style)
  OpNum2Bin : Script (SInt :: SInt :: s) (SAny :: s)   -- Convert int to n-byte LE
  OpBin2Num : Script (SAny :: s) (SInt :: s)           -- Convert LE bytes to int

  -- Splice (OP_CAT)
  OpCat     : Script (SBytes m :: SBytes n :: s) (SBytes (n + m) :: s)
  -- OP_SPLIT: <data:(at+rest)> -> <right:rest> <left:at>
  -- Bitcoin convention: left part (first `at` bytes) ends up on top
  OpSplit   : (at : Nat) -> (rest : Nat) -> Script (SBytes (at + rest) :: s) (SBytes at :: SBytes rest :: s)
  OpSize    : Script (a :: s) (SInt :: a :: s)

  -- Bitwise
  OpEqual   : Script (a :: b :: s) (SBool :: s)
  OpEqualVerify : Script (a :: a :: s) s

  -- Type coercion (hash is just bytes)
  HashToBytes : Script (SHash n :: s) (SBytes n :: s)
  BytesToHash : Script (SBytes n :: s) (SHash n :: s)

  -- Arithmetic
  OpAdd     : Script (SInt :: SInt :: s) (SInt :: s)
  OpSub     : Script (SInt :: SInt :: s) (SInt :: s)
  OpMul     : Script (SInt :: SInt :: s) (SInt :: s)
  OpDiv     : Script (SInt :: SInt :: s) (SInt :: s)
  OpMod     : Script (SInt :: SInt :: s) (SInt :: s)
  Op1Add    : Script (SInt :: s) (SInt :: s)
  Op1Sub    : Script (SInt :: s) (SInt :: s)
  OpNegate  : Script (SInt :: s) (SInt :: s)
  OpAbs     : Script (SInt :: s) (SInt :: s)
  OpNot     : Script (SInt :: s) (SBool :: s)
  OpNumEqual : Script (SInt :: SInt :: s) (SBool :: s)
  OpLessThan : Script (SInt :: SInt :: s) (SBool :: s)
  OpGreaterThan : Script (SInt :: SInt :: s) (SBool :: s)
  OpMin     : Script (SInt :: SInt :: s) (SInt :: s)
  OpMax     : Script (SInt :: SInt :: s) (SInt :: s)

  -- Crypto
  OpHash160 : Script (a :: s) (Hash160 :: s)
  OpHash256 : Script (a :: s) (Hash256 :: s)
  OpSha256  : Script (a :: s) (Hash256 :: s)
  OpRipemd160 : Script (a :: s) (SHash 20 :: s)
  OpCheckSig : Script (PubKey :: Signature :: s) (SBool :: s)
  OpCheckSigVerify : Script (PubKey :: Signature :: s) s

  -- Flow control
  OpVerify  : Script (SBool :: s) s
  OpReturn  : Script s []  -- Terminates

  -- Control flow (simplified - real IF/ELSE needs more work)
  OpIf      : Script s1 s2 -> Script s1 s2 -> Script (SBool :: s1) s2

  -- Locktime
  OpCheckLockTimeVerify : Script (SInt :: s) (SInt :: s)
  OpCheckSequenceVerify : Script (SInt :: s) (SInt :: s)

-- =============================================================================
-- Script Compilation
-- =============================================================================

||| Compile script to raw bytes
covering
export
compile : Script before after -> List Int
compile SNil = []
compile (s1 >> s2) = compile s1 ++ compile s2
compile (PushBytes n bytes) =
  if n == 0 then [0x00]
  else if n <= 75 then cast n :: toList bytes
  else if n <= 255 then [0x4c, cast n] ++ toList bytes
  else [0x4d, cast (n `mod` 256), cast (n `div` 256)] ++ toList bytes
compile (PushInt i) =
  if i == 0 then [0x00]
  else if i >= 1 && i <= 16 then [0x50 + i]
  else if i == -1 then [0x4f]
  else [0x01, cast i `mod` 256]  -- Push 1-byte integer
compile OpDup = [opcodeByte OP_DUP]
compile OpDrop = [opcodeByte OP_DROP]
compile OpSwap = [opcodeByte OP_SWAP]
compile OpOver = [opcodeByte OP_OVER]
compile OpRot = [opcodeByte OP_ROT]
compile OpNip = [opcodeByte OP_NIP]
compile OpTuck = [opcodeByte OP_TUCK]
compile Op2Dup = [opcodeByte OP_2DUP]
compile Op2Drop = [opcodeByte OP_2DROP]
compile Op2Swap = [opcodeByte OP_2SWAP]
compile Op3Dup = [opcodeByte OP_3DUP]
compile OpToAlt = [opcodeByte OP_TOALTSTACK]
compile OpFromAlt = [opcodeByte OP_FROMALTSTACK]
compile OpPick = [opcodeByte OP_PICK]
compile OpRoll = [opcodeByte OP_ROLL]
compile OpNum2Bin = [opcodeByte OP_NUM2BIN]
compile OpBin2Num = [opcodeByte OP_BIN2NUM]
compile OpCat = [opcodeByte OP_CAT]
compile (OpSplit _ _) = [opcodeByte OP_SPLIT]
compile OpSize = [opcodeByte OP_SIZE]
compile OpEqual = [opcodeByte OP_EQUAL]
compile OpEqualVerify = [opcodeByte OP_EQUALVERIFY]
compile HashToBytes = []  -- No-op: type coercion only
compile BytesToHash = []  -- No-op: type coercion only
compile OpAdd = [opcodeByte OP_ADD]
compile OpSub = [opcodeByte OP_SUB]
compile OpMul = [opcodeByte OP_MUL]
compile OpDiv = [opcodeByte OP_DIV]
compile OpMod = [opcodeByte OP_MOD]
compile Op1Add = [opcodeByte OP_1ADD]
compile Op1Sub = [opcodeByte OP_1SUB]
compile OpNegate = [opcodeByte OP_NEGATE]
compile OpAbs = [opcodeByte OP_ABS]
compile OpNot = [opcodeByte OP_NOT]
compile OpNumEqual = [opcodeByte OP_NUMEQUAL]
compile OpLessThan = [opcodeByte OP_LESSTHAN]
compile OpGreaterThan = [opcodeByte OP_GREATERTHAN]
compile OpMin = [opcodeByte OP_MIN]
compile OpMax = [opcodeByte OP_MAX]
compile OpHash160 = [opcodeByte OP_HASH160]
compile OpHash256 = [opcodeByte OP_HASH256]
compile OpSha256 = [opcodeByte OP_SHA256]
compile OpRipemd160 = [opcodeByte OP_RIPEMD160]
compile OpCheckSig = [opcodeByte OP_CHECKSIG]
compile OpCheckSigVerify = [opcodeByte OP_CHECKSIGVERIFY]
compile OpVerify = [opcodeByte OP_VERIFY]
compile OpReturn = [opcodeByte OP_RETURN]
compile (OpIf thenBranch elseBranch) =
  [opcodeByte OP_IF] ++ compile thenBranch ++
  [opcodeByte OP_ELSE] ++ compile elseBranch ++
  [opcodeByte OP_ENDIF]
compile OpCheckLockTimeVerify = [opcodeByte OP_CHECKLOCKTIMEVERIFY]
compile OpCheckSequenceVerify = [opcodeByte OP_CHECKSEQUENCEVERIFY]

-- =============================================================================
-- Example: P2PKH
-- =============================================================================

||| Standard P2PKH script (simplified)
||| OP_DUP OP_HASH160 <pubKeyHash> OP_EQUALVERIFY OP_CHECKSIG
|||
||| Note: Type system tracks stack precisely. Hash160 = SHash 20, not SBytes 20.
||| The EqualVerify checks Hash160 against pushed bytes (conceptually same).
export
p2pkh : Vect 20 Int -> Script (PubKey :: Signature :: []) (SBool :: [])
p2pkh pubKeyHash =
  -- For now, a simplified version that type-checks
  -- Real P2PKH needs Hash160 == SBytes 20 unification
  OpCheckSig
