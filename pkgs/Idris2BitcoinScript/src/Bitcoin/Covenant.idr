||| OP_CAT Covenant Patterns
|||
||| Common covenant patterns using OP_CAT for transaction introspection.
|||
||| NOTE: This is a design skeleton. Full implementation requires
||| careful stack type tracking and sighash preimage reconstruction.
module Bitcoin.Covenant

import Bitcoin.Opcode
import Bitcoin.Script
import Data.Vect

%default total

-- =============================================================================
-- Covenant Concepts
-- =============================================================================

-- OP_CAT Covenants work by:
-- 1. Reconstructing parts of the transaction on the stack
-- 2. Hashing them to get the sighash
-- 3. Verifying signature against that sighash
--
-- This allows constraining:
-- - Output amounts
-- - Output scripts
-- - Input indices
-- - etc.

-- =============================================================================
-- SIGHASH Flags
-- =============================================================================

||| SIGHASH flags
public export
data SigHashFlag : Type where
  SIGHASH_ALL          : SigHashFlag  -- 0x01
  SIGHASH_NONE         : SigHashFlag  -- 0x02
  SIGHASH_SINGLE       : SigHashFlag  -- 0x03
  SIGHASH_ANYONECANPAY : SigHashFlag  -- 0x80

||| Sighash flag to byte
public export
sigHashByte : SigHashFlag -> Int
sigHashByte SIGHASH_ALL = 0x01
sigHashByte SIGHASH_NONE = 0x02
sigHashByte SIGHASH_SINGLE = 0x03
sigHashByte SIGHASH_ANYONECANPAY = 0x80

-- =============================================================================
-- Covenant Building Blocks
-- =============================================================================

||| Simple hash verification (push expected, compare)
||| Stack: <actualHash> -> <>
||| Coerces hash to bytes for comparison with pushed bytes
public export
verifyHash : (n : Nat) -> Vect n Int -> Script (SHash n :: s) s
verifyHash n expected = HashToBytes >> PushBytes n expected >> OpEqualVerify

-- =============================================================================
-- BitGold Covenant Parameters
-- =============================================================================

||| Minimum collateral ratio (130% = 13000 bps)
public export
minRatioBps : Int
minRatioBps = 13000

||| Liquidation bonus (5% = 500 bps)
public export
liquidationBonusBps : Int
liquidationBonusBps = 500

-- =============================================================================
-- Covenant Types (for documentation)
-- =============================================================================

||| Pledge covenant constraints
||| - Must preserve state hash continuity
||| - Collateral ratio must stay >= 130%
||| - Only owner can withdraw (unless liquidated)
public export
record PledgeCovenant where
  constructor MkPledgeCovenant
  stateHash   : Vect 32 Int
  ownerPubKey : Vect 33 Int
  collateral  : Int
  debt        : Int

||| Oracle covenant constraints
||| - Must be signed by authorized oracle
||| - Price must be within deviation bounds
public export
record OracleCovenant where
  constructor MkOracleCovenant
  oraclePubKey : Vect 33 Int
  maxDeviation : Int  -- basis points

-- =============================================================================
-- Covenant Script Patterns (Conceptual)
-- =============================================================================

-- These are conceptual patterns. Real implementation requires:
-- 1. Full sighash preimage format for Taproot/SegWit
-- 2. Proper OP_CAT concatenation of transaction fields
-- 3. Stack manipulation to extract and verify fields

||| Pattern: State continuation
||| Ensures output script matches expected (recursive covenant)
|||
||| Conceptually:
|||   <sig> <pubkey> <newState>
|||   OP_SHA256 <expectedScriptHash> OP_EQUALVERIFY
|||   OP_CHECKSIG
public export
stateContinuationPattern : String
stateContinuationPattern = """
  # State continuation covenant
  # Verifies output[0] script matches expected hash
  OP_SHA256
  <expectedScriptHash>
  OP_EQUALVERIFY
  OP_CHECKSIG
"""

||| Pattern: Amount preservation
||| Ensures output amount >= input amount
|||
||| Requires introspection opcodes or OP_CAT reconstruction
public export
amountPreservationPattern : String
amountPreservationPattern = """
  # Amount preservation covenant
  # output_amount >= input_amount
  <output_amount>
  <input_amount>
  OP_GREATERTHANOREQUAL
  OP_VERIFY
"""

||| Pattern: Ratio check
||| Verifies collateral ratio meets minimum
public export
ratioCheckPattern : String
ratioCheckPattern = """
  # Ratio check covenant
  # (collateral * btcPrice * 10000) / (debt * xauPrice) >= minRatio
  <collateral> <btcPrice> OP_MUL
  10000 OP_MUL
  <debt> <xauPrice> OP_MUL
  OP_DIV
  13000  # minRatio
  OP_GREATERTHANOREQUAL
  OP_VERIFY
"""

-- =============================================================================
-- Covenant Composition
-- =============================================================================

||| Compose two script fragments
public export
andThen : Script s1 s2 -> Script s2 s3 -> Script s1 s3
andThen = (>>)

||| Require both conditions (verify first, then second returns bool)
public export
requireBoth : Script s1 (SBool :: s2) -> Script s2 (SBool :: s3) -> Script s1 (SBool :: s3)
requireBoth c1 c2 = c1 >> OpVerify >> c2
