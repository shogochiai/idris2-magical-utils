||| Resumable secp256k1 public-key recovery — IC-friendly.
|||
||| A single on-chain `ecrecover` (3 scalar multiplications over the group)
||| exceeds the IC per-message instruction limit (IC0522). The group law itself
||| is correct (IcWasm.Secp256k1); the only problem is doing ~768 point ops in
||| ONE message. This module makes the computation RESUMABLE: each `advance`
||| call runs a bounded number of bit-steps and returns the carried state, so the
||| full recovery is spread across several update calls — staying under budget
||| while keeping ALL crypto in pure Idris (no C, no precompile).
|||
||| The recovery is Q = r⁻¹·(s·R − e·G) for recId∈{0,1}; we compute it bit-serial
||| (double-and-add, one scalar bit per step) through a small phase machine and
||| compare Q against the known signer pubkey to pick v.
module IcWasm.Secp256k1.Resumable

import IcWasm.Secp256k1

%default total

-- =============================================================================
-- Bit-serial scalar multiplication (resumable)
-- =============================================================================

||| In-progress scalar multiply k·B:  acc + (remaining bits of k)·addend.
||| Invariant: k·B = acc + scalar·addend, processed LSB-first.
public export
record MulState where
  constructor MkMulState
  acc     : Pt        -- accumulated result so far
  addend  : Pt        -- 2^i · B for the current bit position i
  scalar  : Integer   -- remaining (unprocessed high) bits of k
  done    : Bool      -- scalar exhausted → acc is the final k·B

||| Begin a scalar multiply k·B.
public export
mulStart : (k : Integer) -> (base : Pt) -> MulState
mulStart k base = MkMulState PInf base k (k <= 0)

||| Advance a scalar multiply by up to `budget` bit-steps. Each step folds one
||| bit of `scalar` into `acc` and doubles `addend`. Stops early when complete.
public export
mulAdvance : (budget : Nat) -> MulState -> MulState
mulAdvance Z st = st
mulAdvance (S k) st =
  if st.done then st
  else if st.scalar <= 0 then { done := True } st
  else
    let acc'    = if mmod st.scalar 2 == 1 then ptAdd st.acc st.addend else st.acc
        addend' = ptDbl st.addend
        scalar' = st.scalar `div` 2
        st'     = MkMulState acc' addend' scalar' (scalar' <= 0)
    in mulAdvance k st'

-- =============================================================================
-- Recovery phase machine
-- =============================================================================

||| Which scalar-mult of the recovery we are currently grinding.
||| Phase order: compute sR = s·R, then eG = e·G, then Q = rInv·(sR − eG).
public export
data Phase = PhaseSR | PhaseEG | PhaseQ | PhaseDone

public export
Eq Phase where
  PhaseSR == PhaseSR = True
  PhaseEG == PhaseEG = True
  PhaseQ  == PhaseQ  = True
  PhaseDone == PhaseDone = True
  _ == _ = False

||| Full resumable recovery state for one recId. Carries the active scalar-mult
||| plus the rolling intermediate points and the fixed scalars.
public export
record RecoverState where
  constructor MkRecoverState
  phase  : Phase
  mul    : MulState     -- the active scalar-mult
  sR     : Pt           -- filled after PhaseSR
  eG     : Pt           -- filled after PhaseEG
  rInvN  : Integer      -- r⁻¹ mod n (precomputed; one cheap egcd)
  result : Maybe Pt     -- the recovered Q (when PhaseDone)

||| Start recovering Q for the given (r, s, e, recId). `Nothing` if r doesn't lift
||| to a curve point (invalid signature). The first scalar-mult (s·R) is primed.
public export
recoverStart : (r, s, e : Integer) -> (recId : Bool) -> Maybe RecoverState
recoverStart r s e recId =
  case liftX r recId of
    Nothing => Nothing
    Just yR =>
      let bigR = PXY r yR
      in Just $ MkRecoverState PhaseSR
                  (mulStart (mmod s secpN) bigR)
                  PInf PInf (modInv r secpN) Nothing

||| −P = (x, p − y).
negPt : Pt -> Pt
negPt PInf = PInf
negPt (PXY x y) = PXY x (mmod (negate y) secpP)

||| Advance the recovery by up to `budget` group-law steps. Transitions phases as
||| each scalar-mult completes; when the final Q is computed, sets `result` and
||| `phase := PhaseDone`. Idempotent once done.
public export
recoverAdvance : (budget : Nat) -> (r, s, e : Integer) -> RecoverState -> RecoverState
recoverAdvance budget r s e st =
  case st.phase of
    PhaseDone => st
    PhaseSR =>
      let m = mulAdvance budget st.mul
      in if m.done
           then { phase := PhaseEG, sR := m.acc,
                  mul := mulStart (mmod e secpN) (PXY gX gY) } st
           else { mul := m } st
    PhaseEG =>
      let m = mulAdvance budget st.mul
      in if m.done
           then let eGv = m.acc
                    sumP = ptAdd st.sR (negPt eGv)   -- sR − eG
                in { phase := PhaseQ, eG := eGv,
                     mul := mulStart st.rInvN sumP } st
           else { mul := m } st
    PhaseQ =>
      let m = mulAdvance budget st.mul
      in if m.done
           then { phase := PhaseDone, result := Just m.acc } st
           else { mul := m } st

||| Has the recovery finished, and if so what is Q?
public export
recoverResult : RecoverState -> Maybe Pt
recoverResult st = if st.phase == PhaseDone then st.result else Nothing

-- =============================================================================
-- Serialization (client-carried continuation)
-- =============================================================================
--
-- The recovery state is round-tripped through the client across messages, so the
-- canister needs no storage and stays concurrency-safe. We serialize to a compact
-- '|'-delimited string of fixed-width hex fields. Integers are encoded as
-- 0-padded 80-hex-char big-endian (≥256-bit, with sign folded out: all values
-- here are non-negative field/scalar residues or the small phase/flag ints).

||| Integer → fixed 80-hex-char (40-byte) big-endian. 40 bytes > 32 so it holds
||| any field element / scalar without overflow.
intToHex80 : Integer -> String
intToHex80 v = pack (reverse (go 80 (if v < 0 then 0 else v)))
  where
    hd : Integer -> Char
    hd n = let d = n `mod` 16
           in if d < 10 then chr (cast d + 48) else chr (cast d + 87)
    go : Nat -> Integer -> List Char
    go Z _ = []
    go (S k) w = hd w :: go k (w `div` 16)

||| 80-hex-char → Integer.
hexToInt : String -> Integer
hexToInt s = foldl step 0 (unpack s)
  where
    step : Integer -> Char -> Integer
    step acc c =
      let o = ord c
          d : Integer
          d = if o >= 48 && o <= 57 then cast (o - 48)
              else if o >= 97 && o <= 102 then cast (o - 87)
              else if o >= 65 && o <= 70 then cast (o - 55)
              else 0
      in acc * 16 + d

ptToHex : Pt -> String
ptToHex PInf        = "I" ++ intToHex80 0 ++ intToHex80 0
ptToHex (PXY x y)   = "P" ++ intToHex80 x ++ intToHex80 y

||| Parse "P<80x><80y>" / "I…" → Pt. Field width is fixed (80 each).
hexToPt : String -> Pt
hexToPt s =
  let tag = substr 0 1 s
      xS  = substr 1 80 s
      yS  = substr 81 80 s
  in if tag == "P" then PXY (hexToInt xS) (hexToInt yS) else PInf

phaseToHex : Phase -> String
phaseToHex PhaseSR   = "0"
phaseToHex PhaseEG   = "1"
phaseToHex PhaseQ    = "2"
phaseToHex PhaseDone = "3"

hexToPhase : String -> Phase
hexToPhase "0" = PhaseSR
hexToPhase "1" = PhaseEG
hexToPhase "2" = PhaseQ
hexToPhase _   = PhaseDone

||| Serialize a RecoverState to an opaque continuation token. Layout (fixed
||| offsets so parsing is index-based, '|'-free): everything concatenated.
|||   [0]      phase (1 char)
|||   [1]      mul.done (1 char: 0/1)
|||   [2..164] mul.acc Pt (1+160)
|||   ...      mul.addend Pt, mul.scalar(80), sR Pt, eG Pt, rInvN(80), hasResult(1), result Pt
public export
serializeRecover : RecoverState -> String
serializeRecover st =
  phaseToHex st.phase
    ++ (if st.mul.done then "1" else "0")
    ++ ptToHex st.mul.acc
    ++ ptToHex st.mul.addend
    ++ intToHex80 st.mul.scalar
    ++ ptToHex st.sR
    ++ ptToHex st.eG
    ++ intToHex80 st.rInvN
    ++ (case st.result of Just q => "1" ++ ptToHex q; Nothing => "0" ++ ptToHex PInf)

||| Inverse of serializeRecover. Index-based (Pt token = 161 chars, int = 80).
public export
deserializeRecover : String -> RecoverState
deserializeRecover s =
  let phase   = hexToPhase (substr 0 1 s)
      mdone   = substr 1 1 s == "1"
      accS    = substr 2 161 s
      addS    = substr 163 161 s
      sclS    = substr 324 80 s
      srS     = substr 404 161 s
      egS     = substr 565 161 s
      rinvS   = substr 726 80 s
      hasRes  = substr 806 1 s == "1"
      resS    = substr 807 161 s
  in MkRecoverState phase
       (MkMulState (hexToPt accS) (hexToPt addS) (hexToInt sclS) mdone)
       (hexToPt srS) (hexToPt egS) (hexToInt rinvS)
       (if hasRes then Just (hexToPt resS) else Nothing)
