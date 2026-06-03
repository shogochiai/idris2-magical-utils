||| secp256k1 field arithmetic for EVM signing — point decompression and
||| EIP-1559 recovery-id (v) derivation, in pure Idris2.
|||
||| Why this exists (project rule: idris2-icwasm projects stay in Idris2; the raw
||| runtime is a generated artifact, the crypto is NOT hand-written C):
|||
|||  * `IcWasm.EvmAddress.decompress` was a TODO that punted to web3.py, so
|||    `getEvmAddress` returned a wrong address (0.01 ETH was lost once to
|||    keccak256(compressed_x_only)).
|||  * The bundler-as-canister (AA-5) signs an EIP-1559 tx with ICP `sign_with_ecdsa`,
|||    which returns only (r, s). EVM needs the recovery id `v ∈ {0,1}`. Deriving it
|||    requires decompressing the point at x = r and comparing against the known
|||    signer public key — i.e. exactly the secp256k1 field math below.
|||
||| secp256k1: y² = x³ + 7 (mod p), with the field prime
|||   p = 2²⁵⁶ − 2³² − 977 ≡ 3 (mod 4)
||| so the modular square root is the single modular exponentiation
|||   √a = a^((p+1)/4) (mod p).
||| All arithmetic is on Idris2 `Integer` (arbitrary precision) — no Vect bit
||| juggling, no `{auto prf}`, no big Nat pattern matches (OOM-safe per guidelines).
module IcWasm.Secp256k1

%default total

-- =============================================================================
-- Curve constants (secp256k1)
-- =============================================================================

||| Field prime p = 2^256 - 2^32 - 977.
public export
secpP : Integer
secpP = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F

||| Group order n.
public export
secpN : Integer
secpN = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141

||| Curve constant b (a = 0, b = 7).
secpB : Integer
secpB = 7

-- =============================================================================
-- Modular arithmetic (Integer, arbitrary precision)
-- =============================================================================

||| Non-negative remainder (Idris `mod` can be negative for negative inputs).
public export
mmod : Integer -> Integer -> Integer
mmod a m = let r = a `mod` m in if r < 0 then r + m else r

||| Modular exponentiation base^exp mod m, square-and-multiply.
||| `fuel` bounds the recursion (256-bit exponents need ≤ 256 halvings; we pass
||| a generous fuel so the function is total without a termination proof on `exp`).
modPowFuel : (fuel : Nat) -> (base, expo, m : Integer) -> Integer
modPowFuel Z _ _ _ = 1
modPowFuel (S k) base expo m =
  if expo <= 0 then 1
  else
    let half = modPowFuel k base (expo `div` 2) m
        sq   = mmod (half * half) m
    in if mmod expo 2 == 1 then mmod (sq * base) m else sq

||| base^exp mod m for 256-bit exponents (fuel 320 ≫ 256 bits).
public export
modPow : (base, expo, m : Integer) -> Integer
modPow = modPowFuel 320

||| Modular inverse via the extended Euclidean algorithm (binary-friendly).
||| Far cheaper than Fermat's a^(p-2) (one modPow) — critical for staying under
||| the IC per-message instruction limit when called inside the group law.
||| `fuel` bounds the recursion (gcd steps ≤ ~2·bits ≈ 512 for 256-bit inputs).
egcdFuel : (fuel : Nat) -> (old_r, r, old_s, s : Integer) -> (Integer, Integer)
egcdFuel Z old_r _ old_s _ = (old_r, old_s)
egcdFuel (S k) old_r r old_s s =
  if r == 0 then (old_r, old_s)
  else let q = old_r `div` r
       in egcdFuel k r (old_r - q * r) s (old_s - q * s)

||| Modular inverse a⁻¹ mod m via extended GCD.
public export
modInv : Integer -> Integer -> Integer
modInv a m =
  let (g, x) = egcdFuel 600 (mmod a m) m 1 0
  in mmod x m

-- =============================================================================
-- Modular square root (p ≡ 3 mod 4)  →  √a = a^((p+1)/4) mod p
-- =============================================================================

||| Modular square root over the secp256k1 field. Returns `Just y` with the
||| root in [0, p) whose y² ≡ a, or `Nothing` if `a` is a non-residue (the
||| candidate fails the y² == a check — i.e. x is not on the curve).
public export
modSqrtP : Integer -> Maybe Integer
modSqrtP a =
  let a'  = mmod a secpP
      y   = modPow a' ((secpP + 1) `div` 4) secpP
  in if mmod (y * y) secpP == a' then Just y else Nothing

-- =============================================================================
-- Point decompression:  x  →  (x, y) with the requested y-parity
-- =============================================================================

||| Recover the y-coordinate of the curve point at `x` with the given parity
||| (`yIsOdd = True` ⇒ the odd root, as in SEC1 prefix 0x03 / EIP-1559 v=1).
||| `Nothing` if `x` is not a valid x-coordinate on secp256k1.
public export
liftX : (x : Integer) -> (yIsOdd : Bool) -> Maybe Integer
liftX x yIsOdd =
  let rhs = mmod (mmod (x * x) secpP * x + secpB) secpP   -- x³ + 7 (mod p)
  in case modSqrtP rhs of
       Nothing => Nothing
       Just y  =>
         let yOdd = mmod y 2 == 1
         in Just (if yOdd == yIsOdd then y else secpP - y)

||| Decompress a SEC1 prefix byte (0x02 even / 0x03 odd) + x → (x, y).
public export
decompressPoint : (pfx : Integer) -> (x : Integer) -> Maybe (Integer, Integer)
decompressPoint pfx x =
  case liftX x (pfx == 3) of
    Nothing => Nothing
    Just y  => Just (x, y)

-- =============================================================================
-- EIP-1559 recovery id (v)
-- =============================================================================

||| The point on the curve recovered from an ECDSA signature for recovery id
||| `recId ∈ {0,1}` over message hash `e`:
|||   R   = liftX(r, recId odd)          (r assumed < p; recId≥2 unsupported)
|||   Q   = r⁻¹ · (s·R − e·G)
||| We only need Q's coordinates to compare against the known signer key, so we
||| return them; G-multiples use the standard secp256k1 generator.
||| Returns the recovered public-key point, or Nothing if R doesn't lift.
|||
||| Generator G.
gX : Integer
gX = 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
gY : Integer
gY = 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8

-- --- secp256k1 group law (Jacobian-free affine; n is prime so no point at
-- --- infinity arises for the scalars we use) -------------------------------

||| Affine point or the identity (∞).
public export
data Pt = PInf | PXY Integer Integer

ptDbl : Pt -> Pt
ptDbl PInf = PInf
ptDbl (PXY x y) =
  if mmod y secpP == 0 then PInf
  else
    let l = mmod (3 * mmod (x * x) secpP * modInv (2 * y) secpP) secpP
        x3 = mmod (l * l - 2 * x) secpP
        y3 = mmod (l * (x - x3) - y) secpP
    in PXY x3 y3

ptAdd : Pt -> Pt -> Pt
ptAdd PInf q = q
ptAdd p PInf = p
ptAdd (PXY x1 y1) (PXY x2 y2) =
  if mmod x1 secpP == mmod x2 secpP
    then (if mmod (y1 + y2) secpP == 0 then PInf else ptDbl (PXY x1 y1))
    else
      let l = mmod ((y2 - y1) * modInv (mmod (x2 - x1) secpP) secpP) secpP
          x3 = mmod (l * l - x1 - x2) secpP
          y3 = mmod (l * (x1 - x3) - y1) secpP
      in PXY x3 y3

||| Scalar multiply k·P, double-and-add with fuel. Stops as soon as k reaches 0
||| (so cost tracks the actual bit length, ~256 doublings worst case, not a fixed
||| 320). fuel 260 ≥ 256-bit scalars.
scalarMulFuel : Nat -> Integer -> Pt -> Pt
scalarMulFuel Z _ _ = PInf
scalarMulFuel (S f) k pt =
  if k <= 0 then PInf
  else
    let rest = scalarMulFuel f (k `div` 2) (ptDbl pt)
    in if mmod k 2 == 1 then ptAdd pt rest else rest

public export
scalarMul : Integer -> Pt -> Pt
scalarMul = scalarMulFuel 260

||| Recover the public-key point from (r, s, e) for a given recovery id.
||| Q = r⁻¹ (s·R − e·G).
public export
recoverPubPoint : (r, s, e : Integer) -> (recId : Bool) -> Maybe Pt
recoverPubPoint r s e recId =
  case liftX r recId of
    Nothing => Nothing
    Just yR =>
      let bigR = PXY r yR
          rInv = modInv r secpN
          sR   = scalarMul (mmod s secpN) bigR
          eG   = scalarMul (mmod e secpN) (PXY gX gY)
          -- −eG = (x, p − y)
          negEG = case eG of PInf => PInf; PXY x y => PXY x (mmod (negate y) secpP)
          sum  = ptAdd sR negEG
      in Just (scalarMul (mmod rInv secpN) sum)

||| Derive the EIP-1559 recovery id v ∈ {0,1}: the recId whose recovered public
||| key equals the known signer key (kx, ky). Returns `Just 0/1` or `Nothing`
||| if neither matches (signature not from this key over this hash).
public export
recoveryId : (r, s, e, kx, ky : Integer) -> Maybe Integer
recoveryId r s e kx ky =
  let try : Bool -> Bool
      try b = case recoverPubPoint r s e b of
                Just (PXY x y) => mmod x secpP == mmod kx secpP
                                  && mmod y secpP == mmod ky secpP
                _            => False
  in if try False then Just 0
     else if try True then Just 1
     else Nothing
