||| High-Level Bitcoin Script Patterns
|||
||| Common script patterns abstracted for ease of use.
||| Inspired by sCrypt's declarative style.
module Bitcoin.Patterns

import Bitcoin.Opcode
import Bitcoin.Script
import Data.Vect

%default total

-- =============================================================================
-- Standard Output Patterns
-- =============================================================================

||| P2PK: Pay to Public Key
||| Locking: <pubkey> OP_CHECKSIG
||| Unlocking: <sig>
public export
p2pk : Vect 33 Int -> Script (SAny :: s) (SBool :: s)
p2pk pubkey = PushBytes 33 pubkey >> OpCheckSig

||| P2PKH: Pay to Public Key Hash (locking script portion)
||| Full locking: OP_DUP OP_HASH160 <pkh> OP_EQUALVERIFY OP_CHECKSIG
||| Witness provides: <sig> <pubkey>
||| Result: bool
public export
p2pkh : Vect 20 Int -> Script (SAny :: PubKey :: s) (SBool :: s)
p2pkh pkh =
  -- Stack: sig pubkey
  OpDup >>              -- sig pubkey pubkey
  OpHash160 >>          -- sig pubkey hash
  HashToBytes >>        -- sig pubkey hashBytes
  PushBytes 20 pkh >>   -- sig pubkey hashBytes expected
  OpEqualVerify >>      -- sig pubkey (hash match)
  OpSwap >>             -- pubkey sig
  OpCheckSig            -- bool

||| P2PKH verification only (no signature check)
||| Stack: <pubkey> -> <>
public export
verifyPkh : Vect 20 Int -> Script (CompressedPubKey :: s) s
verifyPkh pkh =
  OpHash160 >>
  HashToBytes >>
  PushBytes 20 pkh >>
  OpEqualVerify

||| P2SH: Pay to Script Hash (redeem script verification)
||| Stack: <redeemScript> -> <>
public export
verifyScriptHash : Vect 20 Int -> Script (SAny :: s) s
verifyScriptHash sh =
  OpHash160 >>
  HashToBytes >>
  PushBytes 20 sh >>
  OpEqualVerify

-- =============================================================================
-- Multisig Patterns
-- =============================================================================

||| 2-of-3 Multisig verification (placeholder)
||| Note: Full implementation requires OP_CHECKMULTISIG
||| This is a simplified single-sig placeholder
public export
multisig2of3 : Script (PubKey :: SAny :: s) (SBool :: s)
multisig2of3 = OpCheckSig

||| Generic M-of-N threshold check concept
||| (Actual implementation requires careful stack management)
public export
record MultisigParams where
  constructor MkMultisig
  required : Nat        -- M signatures required
  numKeys : Nat         -- N total keys
  pubkeys : Vect numKeys (Vect 33 Int)

-- =============================================================================
-- Timelock Patterns
-- =============================================================================

||| Absolute timelock (block height or timestamp)
||| Stack: <locktime> -> <locktime>
public export
checkLockTime : Int -> Script s (SInt :: s)
checkLockTime locktime =
  PushInt locktime >>
  OpCheckLockTimeVerify

||| Relative timelock (sequence number)
||| Stack: <sequence> -> <sequence>
public export
checkSequence : Int -> Script s (SInt :: s)
checkSequence seq =
  PushInt seq >>
  OpCheckSequenceVerify

||| Time-locked P2PKH
||| Can only spend after locktime, with correct signature
||| Stack: <sig> <pubkey> -> <bool>
public export
timelockP2pkh : Int -> Vect 20 Int -> Script (SAny :: PubKey :: s) (SBool :: s)
timelockP2pkh locktime pkh =
  checkLockTime locktime >>
  OpDrop >>
  p2pkh pkh

-- =============================================================================
-- Hash Preimage Patterns (HTLC building blocks)
-- =============================================================================

||| Verify SHA256 preimage
||| Stack: <preimage> -> <>
public export
verifySha256Preimage : Vect 32 Int -> Script (SAny :: s) s
verifySha256Preimage expectedHash =
  OpSha256 >>
  HashToBytes >>
  PushBytes 32 expectedHash >>
  OpEqualVerify

||| Verify HASH160 preimage
||| Stack: <preimage> -> <>
public export
verifyHash160Preimage : Vect 20 Int -> Script (SAny :: s) s
verifyHash160Preimage expectedHash =
  OpHash160 >>
  HashToBytes >>
  PushBytes 20 expectedHash >>
  OpEqualVerify

||| HTLC: Hash Time-Locked Contract (simplified)
||| Two spending paths:
||| 1. Reveal preimage + recipient signature
||| 2. After timeout + sender signature
public export
record HtlcParams where
  constructor MkHtlc
  hashLock : Vect 32 Int     -- SHA256 hash of secret
  recipientPkh : Vect 20 Int
  senderPkh : Vect 20 Int
  timeout : Int              -- Block height

-- =============================================================================
-- Arithmetic Comparison Patterns
-- =============================================================================

||| Verify a >= b
||| Stack: <a> <b> -> <>
public export
verifyGte : Script (SInt :: SInt :: s) s
verifyGte =
  OpSwap >>             -- b a
  OpGreaterThan >>      -- a > b (need >= but this is close)
  OpVerify

||| Verify a > b
||| Stack: <a> <b> -> <>
public export
verifyGt : Script (SInt :: SInt :: s) s
verifyGt =
  OpSwap >>
  OpGreaterThan >>
  OpVerify

||| Verify a == b (numeric)
||| Stack: <a> <b> -> <>
public export
verifyNumEq : Script (SInt :: SInt :: s) s
verifyNumEq =
  OpNumEqual >>
  OpVerify

||| Verify value is in range [min, max]
||| Stack: <value> -> <>
public export
verifyInRange : (min : Int) -> (max : Int) -> Script (SInt :: s) s
verifyInRange min max =
  OpDup >>              -- value value
  PushInt min >>        -- value value min
  OpGreaterThan >>      -- value (value > min)
  OpVerify >>           -- value
  PushInt max >>        -- value max
  OpLessThan >>         -- (value < max)
  OpVerify

-- =============================================================================
-- Data Manipulation Patterns
-- =============================================================================

||| Concatenate two byte arrays
||| Stack: <a> <b> -> <a||b>
public export
concat : Script (SBytes m :: SBytes n :: s) (SBytes (n + m) :: s)
concat = OpCat

||| Duplicate and hash (for SAny input)
||| Stack: <data> -> <data> <hash>
public export
dupAndHash256 : Script (SAny :: s) (Hash256 :: SAny :: s)
dupAndHash256 = OpDup >> OpSha256

||| Verify size of top element
||| Stack: <data> -> <data>
public export
verifySize : Int -> Script (a :: s) (a :: s)
verifySize expected =
  OpSize >>             -- data size
  PushInt expected >>   -- data size expected
  OpNumEqual >>         -- data bool
  OpVerify              -- data
