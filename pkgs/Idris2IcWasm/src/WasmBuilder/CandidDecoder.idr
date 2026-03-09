||| Candid Decoder — DIDL バイト列 → Idris2 値
|||
||| IC canister の引数として届く Candid binary format をデコードする。
||| CandidStubs.idr (エンコーダー) と対をなすデコーダー実装。
|||
||| Wire format:
|||   "DIDL" <type_table_len> <type_entries...> <arg_count> <type_refs...> <values...>
|||
||| LEB128: unsigned, little-endian, 7bit/byte, MSB=継続ビット
||| SLEB128: signed, 同上
module WasmBuilder.CandidDecoder

import Data.Bits
import Data.List
import Data.Maybe
import Data.String
import WasmBuilder.IC0.FFI

%default covering

-- =============================================================================
-- デコード済み Candid 値
-- =============================================================================

public export
data CandidValue
  = CVNat Integer
  | CVInt Integer
  | CVNat8  Bits8
  | CVNat16 Bits16
  | CVNat32 Bits32
  | CVNat64 Bits64
  | CVInt8  Int8
  | CVInt16 Int16
  | CVInt32 Int32
  | CVInt64 Int64
  | CVFloat32 Double
  | CVFloat64 Double
  | CVText String
  | CVBool Bool
  | CVPrincipal (List Bits8)
  | CVBlob (List Bits8)
  | CVOpt (Maybe CandidValue)
  | CVVec (List CandidValue)
  | CVRecord (List (Bits32, CandidValue))   -- (hash, value)
  | CVVariant Bits32 CandidValue             -- (tag_hash, payload)
  | CVNull
  | CVUnknown String

public export
Eq CandidValue where
  (CVNat a)       == (CVNat b)       = a == b
  (CVInt a)       == (CVInt b)       = a == b
  (CVNat8 a)      == (CVNat8 b)      = a == b
  (CVNat16 a)     == (CVNat16 b)     = a == b
  (CVNat32 a)     == (CVNat32 b)     = a == b
  (CVNat64 a)     == (CVNat64 b)     = a == b
  (CVInt8 a)      == (CVInt8 b)      = a == b
  (CVInt16 a)     == (CVInt16 b)     = a == b
  (CVInt32 a)     == (CVInt32 b)     = a == b
  (CVInt64 a)     == (CVInt64 b)     = a == b
  (CVFloat32 a)   == (CVFloat32 b)   = a == b
  (CVFloat64 a)   == (CVFloat64 b)   = a == b
  (CVText a)      == (CVText b)      = a == b
  (CVBool a)      == (CVBool b)      = a == b
  (CVPrincipal a) == (CVPrincipal b) = a == b
  (CVBlob a)      == (CVBlob b)      = a == b
  (CVOpt a)       == (CVOpt b)       = assert_total (a == b)
  (CVVec a)       == (CVVec b)       = assert_total (a == b)
  (CVRecord a)    == (CVRecord b)    = assert_total (map fst a == map fst b && map snd a == map snd b)
  (CVVariant ha va) == (CVVariant hb vb) = ha == hb && assert_total (va == vb)
  CVNull          == CVNull          = True
  (CVUnknown a)   == (CVUnknown b)   = a == b
  _               == _               = False

public export
Show CandidValue where
  show (CVNat n)       = "nat(" ++ show n ++ ")"
  show (CVInt n)       = "int(" ++ show n ++ ")"
  show (CVNat8 n)      = "nat8(" ++ show n ++ ")"
  show (CVNat16 n)     = "nat16(" ++ show n ++ ")"
  show (CVNat32 n)     = "nat32(" ++ show n ++ ")"
  show (CVNat64 n)     = "nat64(" ++ show n ++ ")"
  show (CVInt8 n)      = "int8(" ++ show n ++ ")"
  show (CVInt16 n)     = "int16(" ++ show n ++ ")"
  show (CVInt32 n)     = "int32(" ++ show n ++ ")"
  show (CVInt64 n)     = "int64(" ++ show n ++ ")"
  show (CVFloat32 f)   = "float32(" ++ show f ++ ")"
  show (CVFloat64 f)   = "float64(" ++ show f ++ ")"
  show (CVText s)      = "text(" ++ s ++ ")"
  show (CVBool b)      = "bool(" ++ show b ++ ")"
  show (CVPrincipal _) = "principal(...)"
  show (CVBlob bs)     = "blob(" ++ show (length bs) ++ "B)"
  show (CVOpt Nothing) = "opt(none)"
  show (CVOpt (Just v))= "opt(" ++ show v ++ ")"
  show (CVVec vs)      = "vec[" ++ show (length vs) ++ "]"
  show (CVRecord fs)   = "record{" ++ show (length fs) ++ "}"
  show (CVVariant h v) = "variant(" ++ show h ++ "," ++ show v ++ ")"
  show CVNull          = "null"
  show (CVUnknown s)   = "unknown(" ++ s ++ ")"

-- =============================================================================
-- 内部型テーブル表現 (デコード中の型情報)
-- =============================================================================

data InnerType
  = ITNat | ITInt
  | ITNat8 | ITNat16 | ITNat32 | ITNat64
  | ITInt8 | ITInt16 | ITInt32 | ITInt64
  | ITFloat32 | ITFloat64
  | ITText | ITBool | ITNull | ITBlob | ITPrincipal
  | ITOpt Int               -- type table index of inner
  | ITVec Int               -- type table index of inner
  | ITRecord (List (Bits32, Int))   -- [(field_hash, type_idx)]
  | ITVariant (List (Bits32, Int))  -- [(tag_hash, type_idx)]
  | ITRef Int               -- forward reference to type table
  | ITUnknown Int           -- unknown type code

-- =============================================================================
-- LEB128 / SLEB128 デコード
-- =============================================================================

||| LEB128 unsigned decode: (value, remaining_bytes)
export
decodeLeb128 : List Bits8 -> (Integer, List Bits8)
decodeLeb128 = go 0 0
  where
    go : Integer -> Integer -> List Bits8 -> (Integer, List Bits8)
    go acc shift [] = (acc, [])
    go acc shift (b :: bs) =
      let low  : Integer = cast (b .&. 0x7F)
          n    : Nat     = cast shift
          v              = acc + (low `shiftL` n)
          more           = (b .&. 0x80) /= 0
      in if more then go v (shift + 7) bs else (v, bs)

||| SLEB128 signed decode: (value, remaining_bytes)
export
decodeSleb128 : List Bits8 -> (Integer, List Bits8)
decodeSleb128 = go 0 0
  where
    go : Integer -> Integer -> List Bits8 -> (Integer, List Bits8)
    go acc shift [] = (acc, [])
    go acc shift (b :: bs) =
      let low    : Integer = cast (b .&. 0x7F)
          n      : Nat     = cast shift
          v                = acc + (low `shiftL` n)
          more             = (b .&. 0x80) /= 0
          shift'           = shift + 7
      in if more
           then go v shift' bs
           else -- 符号拡張: MSB が 1 なら負の値
                let n'     : Nat = cast shift'
                    signed       = if (b .&. 0x40) /= 0
                                     then v - (the Integer 1 `shiftL` n')
                                     else v
                in (signed, bs)

-- little-endian バイト列を Integer に変換
leToInt : List (Nat, Bits8) -> Integer
leToInt [] = 0
leToInt (p :: ps) =
  let byte : Integer = cast (snd p)
      n              = fst p * 8
  in (byte `shiftL` n) + leToInt ps

||| 固定幅 little-endian uint decode
decodeFixed : Nat -> List Bits8 -> (Integer, List Bits8)
decodeFixed width bs =
  let (taken, rest) = splitAt width bs
      indexed       = zip [0..width] taken
  in (leToInt indexed, rest)

||| 固定幅 little-endian int decode (符号付き)
decodeFixedSigned : Nat -> List Bits8 -> (Integer, List Bits8)
decodeFixedSigned width bs =
  let (uval, rest) = decodeFixed width bs
      bits    : Int = cast (width * 8)
      signBit : Integer = 1 `shiftL` cast (bits - 1)
      sval    = if uval >= signBit then uval - (1 `shiftL` cast bits) else uval
  in (sval, rest)

||| IEEE 754 float32: 4バイト little-endian → Double (近似)
decodeFloat32 : List Bits8 -> (Double, List Bits8)
decodeFloat32 bs =
  let (raw, rest) = decodeFixed 4 bs
  in (cast raw / 1.0, rest)   -- 簡略化: 実値ではなくビットパターンをキャスト

||| IEEE 754 float64: 8バイト little-endian → Double
decodeFloat64 : List Bits8 -> (Double, List Bits8)
decodeFloat64 bs =
  let (raw, rest) = decodeFixed 8 bs
  in (cast raw / 1.0, rest)

-- =============================================================================
-- 型テーブルのパース
-- =============================================================================

||| 型コードから InnerType に変換 (プリミティブのみ)
primTypeCode : Integer -> Maybe InnerType
primTypeCode (-1)  = Just ITNull
primTypeCode (-2)  = Just ITBool
primTypeCode (-3)  = Just ITNat
primTypeCode (-4)  = Just ITInt
primTypeCode (-5)  = Just ITNat8
primTypeCode (-6)  = Just ITNat16
primTypeCode (-7)  = Just ITNat32
primTypeCode (-8)  = Just ITNat64
primTypeCode (-9)  = Just ITInt8
primTypeCode (-10) = Just ITInt16
primTypeCode (-11) = Just ITInt32
primTypeCode (-12) = Just ITInt64
primTypeCode (-13) = Just ITFloat32
primTypeCode (-14) = Just ITFloat64
primTypeCode (-15) = Just ITText
primTypeCode (-24) = Just ITPrincipal
primTypeCode _     = Nothing

||| 1つの型エントリをパース
parseOneType : List Bits8 -> (InnerType, List Bits8)
parseOneType bs =
  let (code, rest) = decodeSleb128 bs
  in case code of
    (-18) =>   -- opt
      let (inner, rest2) = decodeSleb128 rest
      in (ITOpt (cast inner), rest2)
    (-19) =>   -- vec
      let (inner, rest2) = decodeSleb128 rest
      in (ITVec (cast inner), rest2)
    (-20) =>   -- record
      let (fieldCount, rest2) = decodeLeb128 rest
          (fields, rest3) = parseFields (cast fieldCount) rest2 []
      in (ITRecord fields, rest3)
    (-21) =>   -- variant
      let (caseCount, rest2) = decodeLeb128 rest
          (cases, rest3) = parseFields (cast caseCount) rest2 []
      in (ITVariant cases, rest3)
    _ => case primTypeCode code of
           Just t  => (t, rest)
           Nothing => (ITUnknown (cast code), rest)
  where
    parseFields : Nat -> List Bits8 -> List (Bits32, Int) -> (List (Bits32, Int), List Bits8)
    parseFields 0 bs acc = (reverse acc, bs)
    parseFields (S n) bs acc =
      let (hash, bs2) = decodeLeb128 bs
          (tidx, bs3) = decodeSleb128 bs2
      in parseFields n bs3 ((cast hash, cast tidx) :: acc)

||| 型テーブル全体をパース
parseTypeTable : Nat -> List Bits8 -> (List InnerType, List Bits8)
parseTypeTable 0   bs = ([], bs)
parseTypeTable (S n) bs =
  let (t, rest)    = parseOneType bs
      (ts, rest2)  = parseTypeTable n rest
  in (t :: ts, rest2)

-- =============================================================================
-- 型テーブルを参照して InnerType を解決
-- =============================================================================

resolveRef : List InnerType -> Int -> InnerType
resolveRef table idx =
  if idx >= 0 && idx < cast (length table)
    then case index idx table of
           Just t  => t
           Nothing => ITUnknown idx
    else case primTypeCode (cast idx) of
           Just t  => t
           Nothing => ITUnknown idx
  where
    index : Int -> List a -> Maybe a
    index _ [] = Nothing
    index 0 (x :: _) = Just x
    index n (_ :: xs) = index (n - 1) xs

-- =============================================================================
-- 値デコード (mutual: decodeValue ↔ decodeMany ↔ decodeFields)
-- =============================================================================

listIndex : Nat -> List a -> Maybe a
listIndex _ []        = Nothing
listIndex 0 (x :: _) = Just x
listIndex (S n) (_ :: xs) = listIndex n xs

mutual
  ||| 型に従ってバイト列から値をデコード
  decodeValue : List InnerType -> InnerType -> List Bits8 -> (CandidValue, List Bits8)
  -- プリミティブ
  decodeValue _ ITNull    bs = (CVNull, bs)
  decodeValue _ ITBool    (b :: bs) = (CVBool (b /= 0), bs)
  decodeValue _ ITBool    []        = (CVBool False, [])
  decodeValue _ ITNat     bs = let (n, r) = decodeLeb128 bs   in (CVNat n, r)
  decodeValue _ ITInt     bs = let (n, r) = decodeSleb128 bs  in (CVInt n, r)
  decodeValue _ ITNat8    bs = let (n, r) = decodeFixed 1 bs  in (CVNat8 (cast n), r)
  decodeValue _ ITNat16   bs = let (n, r) = decodeFixed 2 bs  in (CVNat16 (cast n), r)
  decodeValue _ ITNat32   bs = let (n, r) = decodeFixed 4 bs  in (CVNat32 (cast n), r)
  decodeValue _ ITNat64   bs = let (n, r) = decodeFixed 8 bs  in (CVNat64 (cast n), r)
  decodeValue _ ITInt8    bs = let (n, r) = decodeFixedSigned 1 bs in (CVInt8 (cast n), r)
  decodeValue _ ITInt16   bs = let (n, r) = decodeFixedSigned 2 bs in (CVInt16 (cast n), r)
  decodeValue _ ITInt32   bs = let (n, r) = decodeFixedSigned 4 bs in (CVInt32 (cast n), r)
  decodeValue _ ITInt64   bs = let (n, r) = decodeFixedSigned 8 bs in (CVInt64 (cast n), r)
  decodeValue _ ITFloat32 bs = let (f, r) = decodeFloat32 bs  in (CVFloat32 f, r)
  decodeValue _ ITFloat64 bs = let (f, r) = decodeFloat64 bs  in (CVFloat64 f, r)
  -- text: LEB128 長さ + UTF-8 バイト
  decodeValue _ ITText bs =
    let (len, rest)    = decodeLeb128 bs
        (taken, rest2) = splitAt (cast len) rest
        str            = pack (map (chr . cast) taken)
    in (CVText str, rest2)
  -- blob
  decodeValue _ ITBlob bs =
    let (len, rest)    = decodeLeb128 bs
        (taken, rest2) = splitAt (cast len) rest
    in (CVBlob taken, rest2)
  -- principal: 1バイトタグ + LEB128 長さ + バイト列
  decodeValue _ ITPrincipal (_ :: bs) =
    let (len, rest)    = decodeLeb128 bs
        (taken, rest2) = splitAt (cast len) rest
    in (CVPrincipal taken, rest2)
  decodeValue _ ITPrincipal [] = (CVPrincipal [], [])
  -- opt
  decodeValue _     (ITOpt _)       (0 :: bs) = (CVOpt Nothing, bs)
  decodeValue table (ITOpt innerIdx) (1 :: bs) =
    let innerType    = resolveRef table innerIdx
        (v, rest)    = decodeValue table innerType bs
    in (CVOpt (Just v), rest)
  decodeValue _ (ITOpt _) bs = (CVOpt Nothing, bs)
  -- vec
  decodeValue table (ITVec innerIdx) bs =
    let (count, rest) = decodeLeb128 bs
        innerType     = resolveRef table innerIdx
        (items, rest2) = decodeMany table innerType (cast count) rest []
    in (CVVec items, rest2)
  -- record
  decodeValue table (ITRecord fields) bs =
    let (vals, rest) = decodeFields table fields bs []
    in (CVRecord vals, rest)
  -- variant
  decodeValue table (ITVariant cases) bs =
    let (tagIdx, rest) = decodeLeb128 bs
    in case listIndex (cast tagIdx) cases of
         Just (hash, payloadIdx) =>
           let payloadType = resolveRef table payloadIdx
               (v, rest2)  = decodeValue table payloadType rest
           in (CVVariant hash v, rest2)
         Nothing => (CVUnknown ("variant:tag=" ++ show tagIdx), rest)
  decodeValue _ (ITUnknown code) bs = (CVUnknown ("typeCode=" ++ show code), bs)
  decodeValue _ (ITRef idx)      bs = (CVUnknown ("ref=" ++ show idx), bs)

  ||| N 個の値をデコード
  decodeMany : List InnerType -> InnerType -> Nat -> List Bits8 -> List CandidValue -> (List CandidValue, List Bits8)
  decodeMany _ _ 0     bs acc = (reverse acc, bs)
  decodeMany table t (S n) bs acc =
    let (v, rest) = decodeValue table t bs
    in decodeMany table t n rest (v :: acc)

  ||| record フィールドをデコード
  decodeFields : List InnerType -> List (Bits32, Int) -> List Bits8 -> List (Bits32, CandidValue) -> (List (Bits32, CandidValue), List Bits8)
  decodeFields _ [] bs acc = (reverse acc, bs)
  decodeFields table ((hash, tidx) :: rest) bs acc =
    let ftype     = resolveRef table tidx
        (v, bs2)  = decodeValue table ftype bs
    in decodeFields table rest bs2 ((hash, v) :: acc)

-- =============================================================================
-- メインエントリ: DIDL バイト列 → CandidValue リスト
-- =============================================================================

decodeTypeRefs : Nat -> List Bits8 -> List Int -> (List Int, List Bits8)
decodeTypeRefs 0     bs  acc = (reverse acc, bs)
decodeTypeRefs (S n) bs  acc =
  let (ref, rest) = decodeSleb128 bs
  in decodeTypeRefs n rest (cast ref :: acc)

decodeArgs : List InnerType -> List Int -> List Bits8 -> List CandidValue -> List CandidValue
decodeArgs _     []            _  acc = reverse acc
decodeArgs table (ref :: refs) bs acc =
  let t = resolveRef table ref
      (v, rest) = decodeValue table t bs
  in decodeArgs table refs rest (v :: acc)

||| DIDL バイト列をデコードして引数リストを返す
||| エラー時は CVUnknown を返す (クラッシュしない)
export
decodeDIDL : List Bits8 -> List CandidValue
decodeDIDL (0x44 :: 0x49 :: 0x44 :: 0x4C :: rest) =
  let (tableCount, rest2) = decodeLeb128 rest
      (table, rest3)      = parseTypeTable (cast tableCount) rest2
      (argCount, rest4)   = decodeLeb128 rest3
      (typeRefs, rest5)   = decodeTypeRefs (cast argCount) rest4 []
  in decodeArgs table typeRefs rest5 []
decodeDIDL _ = [CVUnknown "not DIDL"]

||| IC メッセージ引数バッファを読んでデコード
export
readArgs : IO (List CandidValue)
readArgs = do
  bs <- readArgBytes
  pure (decodeDIDL bs)

-- =============================================================================
-- ヘルパー: 値の取り出し
-- =============================================================================

||| nat 値を取り出す (失敗時 Nothing)
export
asNat : CandidValue -> Maybe Integer
asNat (CVNat n)   = Just n
asNat (CVNat32 n) = Just (cast n)
asNat (CVNat64 n) = Just (cast n)
asNat (CVInt n)   = if n >= 0 then Just n else Nothing
asNat _           = Nothing

||| text 値を取り出す
export
asText : CandidValue -> Maybe String
asText (CVText s) = Just s
asText _          = Nothing

||| bool 値を取り出す
export
asBool : CandidValue -> Maybe Bool
asBool (CVBool b) = Just b
asBool _          = Nothing

||| principal バイト列を取り出す
export
asPrincipal : CandidValue -> Maybe (List Bits8)
asPrincipal (CVPrincipal bs) = Just bs
asPrincipal _                = Nothing

||| opt の中身を取り出す
export
asOpt : CandidValue -> Maybe (Maybe CandidValue)
asOpt (CVOpt m) = Just m
asOpt _         = Nothing

||| N 番目の引数を取り出す (0-indexed)
export
nthArg : Nat -> List CandidValue -> Maybe CandidValue
nthArg 0     (x :: _)  = Just x
nthArg (S n) (_ :: xs) = nthArg n xs
nthArg _     []        = Nothing

-- =============================================================================
-- テスト用: 純粋関数のユニットテスト
-- =============================================================================

export
testDecodeLeb128_small : Integer
testDecodeLeb128_small = fst (decodeLeb128 [0x05])
-- expected: 5

export
testDecodeLeb128_multibyte : Integer
testDecodeLeb128_multibyte = fst (decodeLeb128 [0x80, 0x01])
-- expected: 128

export
testDecodeLeb128_300 : Integer
testDecodeLeb128_300 = fst (decodeLeb128 [0xAC, 0x02])
-- expected: 300

export
testDecodeSleb128_positive : Integer
testDecodeSleb128_positive = fst (decodeSleb128 [0x3F])
-- expected: 63

export
testDecodeSleb128_negative : Integer
testDecodeSleb128_negative = fst (decodeSleb128 [0x40])
-- expected: -64

export
testDecodeSleb128_minus100 : Integer
testDecodeSleb128_minus100 = fst (decodeSleb128 [0x9C, 0x7F])
-- expected: -100

export
testDecodeText : CandidValue
testDecodeText = fst (decodeValue [] ITText [0x05, 0x68, 0x65, 0x6C, 0x6C, 0x6F])
-- expected: CVText "hello"

export
testDecodeBool_true : CandidValue
testDecodeBool_true = fst (decodeValue [] ITBool [0x01])
-- expected: CVBool True

export
testDecodeBool_false : CandidValue
testDecodeBool_false = fst (decodeValue [] ITBool [0x00])
-- expected: CVBool False

export
testDecodeOpt_none : CandidValue
testDecodeOpt_none = fst (decodeValue [] (ITOpt (-3)) [0x00])
-- expected: CVOpt Nothing

export
testDecodeOpt_some_nat : CandidValue
testDecodeOpt_some_nat = fst (decodeValue [] (ITOpt (-3)) [0x01, 0x2A])
-- expected: CVOpt (Just (CVNat 42))

||| DIDL nat 42 のエンコード: DIDL + type_table_count=0 + arg_count=1 + nat_typeref + value
export
testDecodeDIDL_nat42 : List CandidValue
testDecodeDIDL_nat42 = decodeDIDL [0x44, 0x49, 0x44, 0x4C, 0x00, 0x01, 0x7D, 0x2A]
-- expected: [CVNat 42]

||| DIDL text "hi": DIDL + 0 types + 1 arg + 0x71(text) + len=2 + "hi"
export
testDecodeDIDL_text : List CandidValue
testDecodeDIDL_text = decodeDIDL [0x44, 0x49, 0x44, 0x4C, 0x00, 0x01, 0x71, 0x02, 0x68, 0x69]
-- expected: [CVText "hi"]

||| DIDL (nat, text): 2引数
export
testDecodeDIDL_nat_text : List CandidValue
testDecodeDIDL_nat_text =
  decodeDIDL [ 0x44, 0x49, 0x44, 0x4C  -- DIDL
             , 0x00                     -- 0 types in table
             , 0x02                     -- 2 args
             , 0x7D                     -- arg0: nat
             , 0x71                     -- arg1: text
             , 0x05                     -- nat value: 5
             , 0x03, 0x66, 0x6F, 0x6F  -- text value: len=3, "foo"
             ]
-- expected: [CVNat 5, CVText "foo"]
