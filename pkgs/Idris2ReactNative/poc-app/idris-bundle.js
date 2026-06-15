class IdrisError extends Error { }

function __prim_js2idris_array(x){
  let acc = { h:0 };

  for (let i = x.length-1; i>=0; i--) {
      acc = { a1:x[i], a2:acc };
  }
  return acc;
}

function __prim_idris2js_array(x){
  const result = Array();
  while (x.h === undefined) {
    result.push(x.a1); x = x.a2;
  }
  return result;
}

function __lazy(thunk) {
  let res;
  return function () {
    if (thunk === undefined) return res;
    res = thunk();
    thunk = undefined;
    return res;
  };
};

function __prim_stringIteratorNew(_str) {
  return 0
}

function __prim_stringIteratorToString(_, str, it, f) {
  return f(str.slice(it))
}

function __prim_stringIteratorNext(str, it) {
  if (it >= str.length)
    return {h: 0};
  else
    return {a1: str.charAt(it), a2: it + 1};
}

function __tailRec(f,ini) {
  let obj = ini;
  while(true){
    switch(obj.h){
      case 0: return obj.a1;
      default: obj = f(obj);
    }
  }
}

const _idrisworld = Symbol('idrisworld')

const _crashExp = x=>{throw new IdrisError(x)}

const _bigIntOfString = s=> {
  try {
    const idx = s.indexOf('.')
    return idx === -1 ? BigInt(s) : BigInt(s.slice(0, idx))
  } catch (e) { return 0n }
}

const _numberOfString = s=> {
  try {
    const res = Number(s);
    return isNaN(res) ? 0 : res;
  } catch (e) { return 0 }
}

const _intOfString = s=> Math.trunc(_numberOfString(s))

const _truncToChar = x=> String.fromCodePoint(
  (x >= 0 && x <= 55295) || (x >= 57344 && x <= 1114111) ? x : 0
)

// Int8
const _truncInt8 = x => {
  const res = x & 0xff;
  return res >= 0x80 ? res - 0x100 : res;
}

const _truncBigInt8 = x => Number(BigInt.asIntN(8, x))

// Euclidian Division
const _div = (a,b) => {
  const q = Math.trunc(a / b)
  const r = a % b
  return r < 0 ? (b > 0 ? q - 1 : q + 1) : q
}

const _divBigInt = (a,b) => {
  const q = a / b
  const r = a % b
  return r < 0n ? (b > 0n ? q - 1n : q + 1n) : q
}

// Euclidian Modulo
const _mod = (a,b) => {
  const r = a % b
  return r < 0 ? (b > 0 ? r + b : r - b) : r
}

const _modBigInt = (a,b) => {
  const r = a % b
  return r < 0n ? (b > 0n ? r + b : r - b) : r
}

const _add8s = (a,b) => _truncInt8(a + b)
const _sub8s = (a,b) => _truncInt8(a - b)
const _mul8s = (a,b) => _truncInt8(a * b)
const _div8s = (a,b) => _truncInt8(_div(a,b))
const _shl8s = (a,b) => _truncInt8(a << b)
const _shr8s = (a,b) => _truncInt8(a >> b)

// Int16
const _truncInt16 = x => {
  const res = x & 0xffff;
  return res >= 0x8000 ? res - 0x10000 : res;
}

const _truncBigInt16 = x => Number(BigInt.asIntN(16, x))

const _add16s = (a,b) => _truncInt16(a + b)
const _sub16s = (a,b) => _truncInt16(a - b)
const _mul16s = (a,b) => _truncInt16(a * b)
const _div16s = (a,b) => _truncInt16(_div(a,b))
const _shl16s = (a,b) => _truncInt16(a << b)
const _shr16s = (a,b) => _truncInt16(a >> b)

//Int32
const _truncInt32 = x => x & 0xffffffff

const _truncBigInt32 = x => Number(BigInt.asIntN(32, x))

const _add32s = (a,b) => _truncInt32(a + b)
const _sub32s = (a,b) => _truncInt32(a - b)
const _div32s = (a,b) => _truncInt32(_div(a,b))

const _mul32s = (a,b) => {
  const res = a * b;
  if (res <= Number.MIN_SAFE_INTEGER || res >= Number.MAX_SAFE_INTEGER) {
    return _truncInt32((a & 0xffff) * b + (b & 0xffff) * (a & 0xffff0000))
  } else {
    return _truncInt32(res)
  }
}

//Int64
const _truncBigInt64 = x => BigInt.asIntN(64, x)

const _add64s = (a,b) => _truncBigInt64(a + b)
const _sub64s = (a,b) => _truncBigInt64(a - b)
const _mul64s = (a,b) => _truncBigInt64(a * b)
const _shl64s = (a,b) => _truncBigInt64(a << b)
const _div64s = (a,b) => _truncBigInt64(_divBigInt(a,b))
const _shr64s = (a,b) => _truncBigInt64(a >> b)

//Bits8
const _truncUInt8 = x => x & 0xff

const _truncUBigInt8 = x => Number(BigInt.asUintN(8, x))

const _add8u = (a,b) => (a + b) & 0xff
const _sub8u = (a,b) => (a - b) & 0xff
const _mul8u = (a,b) => (a * b) & 0xff
const _div8u = (a,b) => Math.trunc(a / b)
const _shl8u = (a,b) => (a << b) & 0xff
const _shr8u = (a,b) => (a >> b) & 0xff

//Bits16
const _truncUInt16 = x => x & 0xffff

const _truncUBigInt16 = x => Number(BigInt.asUintN(16, x))

const _add16u = (a,b) => (a + b) & 0xffff
const _sub16u = (a,b) => (a - b) & 0xffff
const _mul16u = (a,b) => (a * b) & 0xffff
const _div16u = (a,b) => Math.trunc(a / b)
const _shl16u = (a,b) => (a << b) & 0xffff
const _shr16u = (a,b) => (a >> b) & 0xffff

//Bits32
const _truncUBigInt32 = x => Number(BigInt.asUintN(32, x))

const _truncUInt32 = x => {
  const res = x & -1;
  return res < 0 ? res + 0x100000000 : res;
}

const _add32u = (a,b) => _truncUInt32(a + b)
const _sub32u = (a,b) => _truncUInt32(a - b)
const _mul32u = (a,b) => _truncUInt32(_mul32s(a,b))
const _div32u = (a,b) => Math.trunc(a / b)

const _shl32u = (a,b) => _truncUInt32(a << b)
const _shr32u = (a,b) => _truncUInt32(a <= 0x7fffffff ? a >> b : (b == 0 ? a : (a >> b) ^ ((-0x80000000) >> (b-1))))
const _and32u = (a,b) => _truncUInt32(a & b)
const _or32u = (a,b)  => _truncUInt32(a | b)
const _xor32u = (a,b) => _truncUInt32(a ^ b)

//Bits64
const _truncUBigInt64 = x => BigInt.asUintN(64, x)

const _add64u = (a,b) => BigInt.asUintN(64, a + b)
const _mul64u = (a,b) => BigInt.asUintN(64, a * b)
const _div64u = (a,b) => a / b
const _shl64u = (a,b) => BigInt.asUintN(64, a << b)
const _shr64u = (a,b) => BigInt.asUintN(64, a >> b)
const _sub64u = (a,b) => BigInt.asUintN(64, a - b)

//String
const _strReverse = x => x.split('').reverse().join('')

const _substr = (o,l,x) => x.slice(o, o + l)

const RN_Runtime_prim__textNode = ( (s) => __idrisRN.textNode(s));
const RN_Runtime_prim__setProp = ( (p, k, v) => __idrisRN.setProp(p, k, v));
const RN_Runtime_prim__setHandler = ( (p, k, cb) => __idrisRN.setHandler(p, k, cb));
const RN_Runtime_prim__run = ( (render) => __idrisRN.run(render));
const RN_Runtime_prim__requestRerender = ( () => __idrisRN.requestRerender());
const RN_Runtime_prim__pushChild = ( (arr, c) => __idrisRN.pushChild(arr, c));
const RN_Runtime_prim__newProps = ( () => __idrisRN.newProps());
const RN_Runtime_prim__newChildren = ( () => __idrisRN.newChildren());
const RN_Runtime_prim__createElement = ( (comp, p, kids) => __idrisRN.createElement(comp, p, kids));
/* {$tcOpt:1} */
function x24tcOpt_1($0) {
 switch($0.a6.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:1} */, a1: undefined};
  case undefined: /* cons */ {
   const $4 = RN_Runtime_renderNode($0.a4, $0.a6.a1, $0.a7);
   const $9 = RN_Runtime_prim__pushChild($0.a5, $4, $0.a7);
   return {h: 1 /* {TcContinue1:1} */, a1: $0.a1, a2: $0.a2, a3: $0.a3, a4: $0.a4, a5: $0.a5, a6: $0.a6.a2, a7: $0.a7};
  }
 }
}

/* RN.Runtime.2870:1384:renderChildren */
function RN_Runtime_n__2870_1384_renderChildren($0, $1, $2, $3, $4, $5, $6) {
 return __tailRec(x24tcOpt_1, {h: 1 /* {TcContinue1:1} */, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4, a6: $5, a7: $6});
}

/* {$tcOpt:2} */
function x24tcOpt_2($0) {
 switch($0.a4.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:2} */, a1: undefined};
  case undefined: /* cons */ {
   switch($0.a4.a1.h) {
    case 0: /* Prop */ {
     const $5 = RN_Runtime_prim__setProp($0.a3, $0.a4.a1.a1, $0.a4.a1.a2, $0.a5);
     return {h: 1 /* {TcContinue2:1} */, a1: $0.a1, a2: $0.a2, a3: $0.a3, a4: $0.a4.a2, a5: $0.a5};
    }
    case 1: /* Handler */ {
     const $10 = RN_Runtime_prim__setHandler($0.a3, $0.a4.a1.a1, $15 => $0.a2($0.a4.a1.a2), $0.a5);
     return {h: 1 /* {TcContinue2:1} */, a1: $0.a1, a2: $0.a2, a3: $0.a3, a4: $0.a4.a2, a5: $0.a5};
    }
   }
  }
 }
}

/* RN.Runtime.2754:1248:go */
function RN_Runtime_n__2754_1248_go($0, $1, $2, $3, $4) {
 return __tailRec(x24tcOpt_2, {h: 1 /* {TcContinue2:1} */, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4});
}

/* {__mainExpression:0} */
function __mainExpression_0() {
 return PrimIO_unsafePerformIO($2 => Main_main($2));
}

/* prim__sub_Integer : Integer -> Integer -> Integer */
function prim__sub_Integer($0, $1) {
 return ($0-$1);
}

/* Main.view : Model -> Node Msg */
function Main_view($0) {
 return RN_Node_view({a1: RN_Node_testID('root'), a2: {h: 0}}, {a1: RN_Node_text({h: 0}, 'Hello from Idris2 on React Native'), a2: {a1: RN_Node_text({h: 0}, ('tapped: '+Prelude_Show_show_Show_Nat($0))), a2: {a1: RN_Node_button({a1: RN_Node_onPress(undefined), a2: {h: 0}}, 'Tap me'), a2: {h: 0}}}});
}

/* Main.update : Msg -> Model -> Model */
function Main_update($0, $1) {
 return ($1+1n);
}

/* Main.main : IO () */
function Main_main($0) {
 return RN_Runtime_runMVU($3 => $4 => Main_update($3, $4), $9 => Main_view($9), undefined, 0n, $0);
}

/* RN.Runtime.2980:1539:render */
function RN_Runtime_n__2980_1539_render($0, $1, $2, $3, $4, $5, $6) {
 const $7 = ($4.value);
 return RN_Runtime_renderNode($d => $e => RN_Runtime_n__2980_1507_dispatch($0, $1, $2, $3, $4, $d, $e), $2($7), $6);
}

/* RN.Runtime.2980:1507:dispatch */
function RN_Runtime_n__2980_1507_dispatch($0, $1, $2, $3, $4, $5, $6) {
 const $7 = ($4.value);
 const $b = ($4.value=$3($5)($7));
 return RN_Runtime_prim__requestRerender($6);
}

/* RN.Runtime.runMVU : (e -> s -> s) -> (s -> Node e) -> e -> s -> IO () */
function RN_Runtime_runMVU($0, $1, $2, $3, $4) {
 const $15 = b => a => $16 => $17 => $18 => {
  const $19 = $16($18);
  const $1c = $17($18);
  return $19($1c);
 };
 const $a = {a1: b => a => func => $c => $d => Prelude_IO_map_Functor_IO(func, $c, $d), a2: a => $13 => $14 => $13, a3: $15};
 const $21 = b => a => $22 => $23 => $24 => {
  const $25 = $22($24);
  return $23($25)($24);
 };
 const $2c = a => $2d => $2e => {
  const $2f = $2d($2e);
  return $2f($2e);
 };
 const $9 = {a1: $a, a2: $21, a3: $2c};
 const $8 = {a1: $9, a2: a => $35 => $35};
 const $6 = Data_IORef_newIORef($8, $0($2)($3));
 const $5 = $6($4);
 return RN_Runtime_prim__run($3e => $3f => RN_Runtime_n__2980_1539_render($2, $3, $1, $0, $5, $3e, $3f), $4);
}

/* RN.Runtime.renderNode : (e -> IO ()) -> Node e -> IO RElem */
function RN_Runtime_renderNode($0, $1, $2) {
 switch($1.h) {
  case 1: /* Str */ return RN_Runtime_prim__textNode($1.a1, $2);
  case 2: /* Empty */ return RN_Runtime_prim__textNode('', $2);
  case 0: /* El */ {
   const $a = RN_Runtime_buildProps($0, $1.a2, $2);
   const $f = RN_Runtime_prim__newChildren($2);
   const $12 = RN_Runtime_n__2870_1384_renderChildren($1.a3, $1.a2, $1.a1, $0, $f, $1.a3, $2);
   return RN_Runtime_prim__createElement($1.a1, $a, $f, $2);
  }
 }
}

/* RN.Runtime.buildProps : (e -> IO ()) -> List (Attr e) -> IO RProps */
function RN_Runtime_buildProps($0, $1, $2) {
 const $3 = RN_Runtime_prim__newProps($2);
 const $6 = RN_Runtime_n__2754_1248_go($1, $0, $3, $1, $2);
 return $3;
}

/* Data.IORef.newIORef : HasIO io => a -> io (IORef a) */
function Data_IORef_newIORef($0, $1) {
 return $0.a1.a2(undefined)(undefined)($0.a2(undefined)($10 => ({value:$1})))(m => $0.a1.a1.a2(undefined)(m));
}

/* Prelude.Types.prim__integerToNat : Integer -> Nat */
function Prelude_Types_prim__integerToNat($0) {
 switch(((0n<=$0)?1:0)) {
  case 0: return 0n;
  default: return $0;
 }
}

/* Prelude.EqOrd.compare */
function Prelude_EqOrd_compare_Ord_Integer($0, $1) {
 switch(Prelude_EqOrd_x3c_Ord_Integer($0, $1)) {
  case 1: return 0;
  case 0: {
   switch(Prelude_EqOrd_x3dx3d_Eq_Integer($0, $1)) {
    case 1: return 1;
    case 0: return 2;
   }
  }
 }
}

/* Prelude.EqOrd.== */
function Prelude_EqOrd_x3dx3d_Eq_Ordering($0, $1) {
 switch($0) {
  case 0: {
   switch($1) {
    case 0: return 1;
    default: return 0;
   }
  }
  case 1: {
   switch($1) {
    case 1: return 1;
    default: return 0;
   }
  }
  case 2: {
   switch($1) {
    case 2: return 1;
    default: return 0;
   }
  }
  default: return 0;
 }
}

/* Prelude.EqOrd.== */
function Prelude_EqOrd_x3dx3d_Eq_Integer($0, $1) {
 switch((($0===$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.== */
function Prelude_EqOrd_x3dx3d_Eq_Char($0, $1) {
 switch((($0===$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.< */
function Prelude_EqOrd_x3c_Ord_Integer($0, $1) {
 switch((($0<$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd./= */
function Prelude_EqOrd_x2fx3d_Eq_Ordering($0, $1) {
 switch(Prelude_EqOrd_x3dx3d_Eq_Ordering($0, $1)) {
  case 1: return 0;
  case 0: return 1;
 }
}

/* Prelude.EqOrd.compareInteger : Integer -> Integer -> Ordering */
function Prelude_EqOrd_compareInteger($0, $1) {
 return Prelude_EqOrd_compare_Ord_Integer($0, $1);
}

/* Prelude.Show.show */
function Prelude_Show_show_Show_Nat($0) {
 return Prelude_Show_show_Show_Integer($0);
}

/* Prelude.Show.show */
function Prelude_Show_show_Show_Integer($0) {
 return Prelude_Show_showPrec_Show_Integer({h: 0 /* Open */}, $0);
}

/* Prelude.Show.showPrec */
function Prelude_Show_showPrec_Show_Integer($0, $1) {
 return Prelude_Show_primNumShow($4 => (''+$4), $0, $1);
}

/* Prelude.Show.compare */
function Prelude_Show_compare_Ord_Prec($0, $1) {
 switch($0.h) {
  case 4: /* User */ {
   switch($1.h) {
    case 4: /* User */ return Prelude_EqOrd_compare_Ord_Integer($0.a1, $1.a1);
    default: return Prelude_EqOrd_compare_Ord_Integer(Prelude_Show_precCon($0), Prelude_Show_precCon($1));
   }
  }
  default: return Prelude_EqOrd_compare_Ord_Integer(Prelude_Show_precCon($0), Prelude_Show_precCon($1));
 }
}

/* Prelude.Show.>= */
function Prelude_Show_x3ex3d_Ord_Prec($0, $1) {
 return Prelude_EqOrd_x2fx3d_Eq_Ordering(Prelude_Show_compare_Ord_Prec($0, $1), 0);
}

/* Prelude.Show.showParens : Bool -> String -> String */
function Prelude_Show_showParens($0, $1) {
 switch($0) {
  case 0: return $1;
  case 1: return ('('+($1+')'));
 }
}

/* Prelude.Show.primNumShow : (a -> String) -> Prec -> a -> String */
function Prelude_Show_primNumShow($0, $1, $2) {
 const $3 = $0($2);
 let $7;
 switch(Prelude_Show_x3ex3d_Ord_Prec($1, {h: 5 /* PrefixMinus */})) {
  case 1: {
   $7 = Prelude_Show_firstCharIs($e => Prelude_EqOrd_x3dx3d_Eq_Char($e, '-'), $3);
   break;
  }
  case 0: {
   $7 = 0;
   break;
  }
 }
 return Prelude_Show_showParens($7, $3);
}

/* Prelude.Show.precCon : Prec -> Integer */
function Prelude_Show_precCon($0) {
 switch($0.h) {
  case 0: /* Open */ return 0n;
  case 1: /* Equal */ return 1n;
  case 2: /* Dollar */ return 2n;
  case 3: /* Backtick */ return 3n;
  case 4: /* User */ return 4n;
  case 5: /* PrefixMinus */ return 5n;
  case 6: /* App */ return 6n;
 }
}

/* Prelude.Show.firstCharIs : (Char -> Bool) -> String -> Bool */
function Prelude_Show_firstCharIs($0, $1) {
 switch($1) {
  case '': return 0;
  default: return $0(($1.charAt(0)));
 }
}

/* Prelude.IO.map */
function Prelude_IO_map_Functor_IO($0, $1, $2) {
 const $3 = $1($2);
 return $0($3);
}

/* PrimIO.unsafePerformIO : IO a -> a */
function PrimIO_unsafePerformIO($0) {
 return PrimIO_unsafeCreateWorld(w => $0(w));
}

/* PrimIO.unsafeCreateWorld : (1 _ : ((1 _ : %World) -> a)) -> a */
function PrimIO_unsafeCreateWorld($0) {
 return $0(_idrisworld);
}

/* RN.Node.view : List (Attr e) -> List (Node e) -> Node e */
function RN_Node_view($0, $1) {
 return {h: 0 /* El */, a1: 'View', a2: $0, a3: $1};
}

/* RN.Node.text : List (Attr e) -> String -> Node e */
function RN_Node_text($0, $1) {
 return {h: 0 /* El */, a1: 'Text', a2: $0, a3: {a1: {h: 1 /* Str */, a1: $1}, a2: {h: 0}}};
}

/* RN.Node.testID : String -> Attr e */
function RN_Node_testID($0) {
 return {h: 0 /* Prop */, a1: 'testID', a2: $0};
}

/* RN.Node.onPress : e -> Attr e */
function RN_Node_onPress($0) {
 return {h: 1 /* Handler */, a1: 'onPress', a2: $0};
}

/* RN.Node.button : List (Attr e) -> String -> Node e */
function RN_Node_button($0, $1) {
 return {h: 0 /* El */, a1: 'Button', a2: {a1: {h: 0 /* Prop */, a1: 'title', a2: $1}, a2: $0}, a3: {h: 0}};
}


try{__mainExpression_0()}catch(e){if(e instanceof IdrisError){console.log('ERROR: ' + e.message)}else{throw e} }
