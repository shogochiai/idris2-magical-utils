||| Tests for IcWasm.Secp256k1 + .Resumable — the pure-Idris secp256k1 used for
||| EVM recovery-id and point decompression. Golden values are from a REAL ICP
||| t-ECDSA signature (recovers the funded deployer 0x57d979…), so a regression in
||| the field math fails here rather than on-chain with funds at stake.
module IcWasm.Tests.Secp256k1Tests

import IcWasm.Secp256k1
import IcWasm.Secp256k1.Resumable

%default covering

-- Real signature golden (test_key_1 over hash 0x11…11; yParity=0 → 0x57d979…).
gr : Integer
gr = 0xd19a73a6aa79e968006a2ff08e55c9f246bed8f012cf91e20fa23e76c835d9ad
gs : Integer
gs = 0x37553b1d8f6b5ce7f264b3ad59897362fd7f902691b012d6635f47a955a1f2c6
ge : Integer
ge = 0x1111111111111111111111111111111111111111111111111111111111111111
gkx : Integer
gkx = 0xd3135905e3e6130d7a5310f199269a2471af1946a93bbe9d5082c87e22c0c7ce
gky : Integer
gky = 0x6a9f3a229a524d2d396684038f42f0dff3debe97a3e3835385e57636d897dacf

||| REQ_SECP_SQRT_001: modular sqrt round-trips (y² ≡ a).
export
test_REQ_SECP_SQRT_001 : Bool
test_REQ_SECP_SQRT_001 =
  case modSqrtP 4 of
    Just y  => mmod (y * y) secpP == 4
    Nothing => False

||| REQ_SECP_INV_001: modInv (binary egcd) — 3⁻¹ mod 7 == 5.
export
test_REQ_SECP_INV_001 : Bool
test_REQ_SECP_INV_001 = modInv 3 7 == 5 && mmod (modInv gr secpN * gr) secpN == 1

||| REQ_SECP_DECOMP_001: decompressing the signer pubkey x (odd y, prefix 03)
||| recovers exactly ky.
export
test_REQ_SECP_DECOMP_001 : Bool
test_REQ_SECP_DECOMP_001 =
  case decompressPoint 3 gkx of
    Just (_, y) => y == gky
    Nothing     => False

||| REQ_SECP_RECID_001: recoveryId on the real signature returns Just 0 (the
||| on-chain yParity that recovered the funded deployer).
export
test_REQ_SECP_RECID_001 : Bool
test_REQ_SECP_RECID_001 = recoveryId gr gs ge gkx gky == Just 0

-- Drive the resumable recovery to completion in small budget chunks.
driveResume : Nat -> RecoverState -> Maybe Pt
driveResume Z _ = Nothing
driveResume (S fuel) st =
  case recoverResult st of
    Just q  => Just q
    Nothing => driveResume fuel (recoverAdvance 20 gr gs ge st)

ptEq : Pt -> Integer -> Integer -> Bool
ptEq (PXY x y) ax ay = mmod x secpP == mmod ax secpP && mmod y secpP == mmod ay secpP
ptEq _ _ _ = False

||| REQ_SECP_RESUME_001: the resumable recovery reaches the SAME Q as the
||| non-resumable path (driven in budget-20 chunks).
export
test_REQ_SECP_RESUME_001 : Bool
test_REQ_SECP_RESUME_001 =
  case recoverStart gr gs ge False of
    Nothing  => False
    Just st0 => case driveResume 200 st0 of
      Just q  => ptEq q gkx gky
      Nothing => False

||| REQ_SECP_RESUME_002: serialize/deserialize round-trips the recovery state
||| (the client-carried continuation reconstructs Q identically).
export
test_REQ_SECP_RESUME_002 : Bool
test_REQ_SECP_RESUME_002 =
  case recoverStart gr gs ge False of
    Nothing  => False
    Just st0 => go 200 (serializeRecover st0)
  where
    go : Nat -> String -> Bool
    go Z _ = False
    go (S fuel) tok =
      let st = deserializeRecover tok in
      case recoverResult st of
        Just q  => ptEq q gkx gky
        Nothing => go fuel (serializeRecover (recoverAdvance 20 gr gs ge st))

export
allTests : List (String, Bool)
allTests =
  [ ("REQ_SECP_SQRT_001",   test_REQ_SECP_SQRT_001)
  , ("REQ_SECP_INV_001",    test_REQ_SECP_INV_001)
  , ("REQ_SECP_DECOMP_001", test_REQ_SECP_DECOMP_001)
  , ("REQ_SECP_RECID_001",  test_REQ_SECP_RECID_001)
  , ("REQ_SECP_RESUME_001", test_REQ_SECP_RESUME_001)
  , ("REQ_SECP_RESUME_002", test_REQ_SECP_RESUME_002)
  ]

export
secp256k1TestsPass : Bool
secp256k1TestsPass = all snd allTests
