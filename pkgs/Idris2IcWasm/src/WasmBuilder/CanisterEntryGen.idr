||| Canister Entry C Code Generator
|||
||| Generates canister_entry.c from a parsed .did file.
||| The generated file contains:
|||   - IC0 extern declarations (fixed)
|||   - Candid argument parsers (fixed)
|||   - Candid reply helpers (fixed)
|||   - canister_init / canister_pre_upgrade / canister_post_upgrade (fixed)
|||   - #define CMD_<METHOD> <N> for each .did method
|||   - canister_query_* / canister_update_* for each method
module WasmBuilder.CanisterEntryGen

import Data.List
import Data.String
import WasmBuilder.CandidStubs

%default covering

-- =============================================================================
-- Generation Options
-- =============================================================================

||| Injection annotation: inject ic0_time or a constant into an arg slot
public export
data Injection
  = InjectTime Nat             -- @inject_time=N: set arg N to ic0_time()/1e9
  | InjectConst Nat Integer    -- @inject_const=N:V: set arg N to constant V
  | InjectTimePlus Nat Integer -- @inject_time_plus=N:V: set arg N to now+V

||| Parsed cmd-map entry with annotations
public export
record CmdMapEntry where
  constructor MkCmdMapEntry
  cmdId      : Nat
  injections : List Injection
  resultFn   : Maybe String    -- @result_fn=funcName: custom result function

||| Options controlling code generation
public export
record GenOptions where
  constructor MkGenOptions
  ffiPrefix           : String     -- e.g. "theworld" for theworld_reset_ffi, theworld_c_set_arg_i32 etc.
  libName             : String     -- e.g. "libic0" (informational; used in comments)
  initFn              : String     -- function called inside canister_init after stable grow, "" = none
  heartbeatCmd        : Maybe Nat  -- CMD id for canister_heartbeat, Nothing = no heartbeat
  heartbeatCheckpoint : Maybe Nat  -- sqlite_stable_save every N heartbeats, Nothing = no checkpoint

-- =============================================================================
-- CMD_ID Assignment
-- =============================================================================

||| Assign sequential CMD IDs starting from 0 in .did appearance order
export
assignCmdIds : List DidMethod -> List (DidMethod, Nat)
assignCmdIds methods = zip methods (go 0 methods)
  where
    go : Nat -> List DidMethod -> List Nat
    go _ []      = []
    go n (_ :: rest) = n :: go (n + 1) rest

||| Parse annotation tokens from cmd-map line remainder
||| e.g. "@inject_time=2 @inject_const=3:900 @result_fn=ic_str_c_get"
parseAnnotations : List String -> (List Injection, Maybe String)
parseAnnotations [] = ([], Nothing)
parseAnnotations (tok :: rest) =
  let (injs, rfn) = parseAnnotations rest
  in if isPrefixOf "@inject_time=" tok
       then case parsePositive {a=Nat} (substr 13 (length tok) tok) of
              Just n  => (InjectTime n :: injs, rfn)
              Nothing => (injs, rfn)
     else if isPrefixOf "@inject_const=" tok
       then let val = substr 14 (length tok) tok
            in case break (== ':') (unpack val) of
                 (nPart, ':' :: vPart) =>
                   case (parsePositive {a=Nat} (pack nPart), parseInteger {a=Integer} (pack vPart)) of
                     (Just n, Just v) => (InjectConst n v :: injs, rfn)
                     _                => (injs, rfn)
                 _ => (injs, rfn)
     else if isPrefixOf "@inject_time_plus=" tok
       then let val = substr 18 (length tok) tok
            in case break (== ':') (unpack val) of
                 (nPart, ':' :: vPart) =>
                   case (parsePositive {a=Nat} (pack nPart), parseInteger {a=Integer} (pack vPart)) of
                     (Just n, Just v) => (InjectTimePlus n v :: injs, rfn)
                     _                => (injs, rfn)
                 _ => (injs, rfn)
     else if isPrefixOf "@result_fn=" tok
       then (injs, Just (substr 11 (length tok) tok))
     else (injs, rfn)

||| Parse a cmd-map file: lines of "methodName=N [@annotation...]" or "# comment"
||| Returns list of (methodName, CmdMapEntry) pairs
export
parseCmdMapEntries : String -> List (String, CmdMapEntry)
parseCmdMapEntries content =
  mapMaybe parseLine (lines content)
  where
    parseLine : String -> Maybe (String, CmdMapEntry)
    parseLine l =
      let t = trim l
      in if null t || isPrefixOf "#" t
           then Nothing
           else let tokens = words t
                in case tokens of
                     [] => Nothing
                     (first :: annots) =>
                       case break (== '=') (unpack first) of
                         (namePart, '=' :: rest) =>
                           let methodName = trim (pack namePart)
                               numStr     = trim (pack rest)
                           in case parseInteger {a=Integer} numStr of
                                Just n  => if n >= 0
                                  then let (injs, rfn) = parseAnnotations annots
                                       in Just (methodName, MkCmdMapEntry (cast n) injs rfn)
                                  else Nothing
                                Nothing => Nothing
                         _ => Nothing

||| Backward-compatible: parse cmd-map returning simple (String, Nat) pairs
export
parseCmdMap : String -> List (String, Nat)
parseCmdMap content = map (\(n, e) => (n, e.cmdId)) (parseCmdMapEntries content)

||| Assign CMD IDs using an explicit map, falling back to auto-numbering
||| for methods not in the map (starting after the max mapped ID).
export
assignCmdIdsWithMap : List DidMethod -> List (String, Nat) -> List (DidMethod, Nat)
assignCmdIdsWithMap methods cmdMap =
  let maxMapped = foldl (\acc, (_, n) => if n > acc then n else acc) 0 cmdMap
      go : Nat -> List DidMethod -> List (DidMethod, Nat)
      go _ []        = []
      go nextAuto (m :: rest) =
        case lookup m.name cmdMap of
          Just n  => (m, n) :: go nextAuto rest
          Nothing => (m, nextAuto) :: go (nextAuto + 1) rest
      startAuto = maxMapped + 1
  in go startAuto methods

||| Convert camelCase to UPPER_SNAKE_CASE for #define macros
toCmdMacro : String -> String
toCmdMacro name =
  let chars = unpack name
      go : List Char -> List Char
      go [] = []
      go (c :: cs) =
        if isUpper c
          then '_' :: toUpper c :: go cs
          else toUpper c :: go cs
      result = go chars
  in pack $ case result of
       ('_' :: rest) => rest
       other         => other

cmdMacroName : String -> String
cmdMacroName name = "CMD_" ++ toCmdMacro name

-- =============================================================================
-- Argument Decode Code Generation (N-ary Generic)
-- =============================================================================

||| Generate C code to decode a single Candid argument at given index.
||| Returns (decode_code, set_arg_code)
||| argIdx: 0-based position in Candid args, ffiIdx: 1-based FFI arg slot
genSingleArgDecode : String -> Nat -> Nat -> CandidType -> (String, String)
genSingleArgDecode pfx argIdx ffiIdx CTNat =
  ( "    uint64_t a" ++ show argIdx ++ " = parse_leb128(offset, &new_offset); offset = new_offset;\n"
  , "    " ++ pfx ++ "_c_set_arg_i32(" ++ show ffiIdx ++ ", (int32_t)a" ++ show argIdx ++ ");\n"
  )
genSingleArgDecode pfx argIdx ffiIdx CTNat64 =
  ( "    uint64_t a" ++ show argIdx ++ " = parse_leb128(offset, &new_offset); offset = new_offset;\n"
  , "    " ++ pfx ++ "_c_set_arg_i32(" ++ show ffiIdx ++ ", (int32_t)a" ++ show argIdx ++ ");\n"
  )
genSingleArgDecode pfx argIdx ffiIdx CTText =
  ( "    { uint64_t tlen" ++ show argIdx ++ " = parse_leb128(offset, &new_offset); offset = new_offset;\n" ++
    "      if (tlen" ++ show argIdx ++ " >= sizeof(text_arg_buf)) tlen" ++ show argIdx ++ " = sizeof(text_arg_buf)-1;\n" ++
    "      for (uint64_t i=0; i<tlen" ++ show argIdx ++ " && offset<arg_buf_size; i++) text_arg_buf[i]=(char)arg_buf[offset++];\n" ++
    "      text_arg_buf[tlen" ++ show argIdx ++ "] = '\\0'; text_arg_len = (int32_t)tlen" ++ show argIdx ++ "; }\n"
  , "    " ++ pfx ++ "_c_set_arg_str(" ++ show ffiIdx ++ ", text_arg_buf);\n"
  )
genSingleArgDecode pfx argIdx ffiIdx CTBool =
  ( "    uint64_t a" ++ show argIdx ++ " = (uint64_t)arg_buf[offset++];\n"
  , "    " ++ pfx ++ "_c_set_arg_i32(" ++ show ffiIdx ++ ", (int32_t)a" ++ show argIdx ++ ");\n"
  )
genSingleArgDecode pfx argIdx ffiIdx CTPrincipal =
  -- Principal decoded as text (hex representation handled by Idris2 side)
  genSingleArgDecode pfx argIdx ffiIdx CTText
genSingleArgDecode pfx argIdx ffiIdx (CTOpt _) =
  -- Opt: pass as text for Idris2 to decode
  genSingleArgDecode pfx argIdx ffiIdx CTText
genSingleArgDecode pfx argIdx ffiIdx _ =
  -- Fallback for complex types: pass as text
  genSingleArgDecode pfx argIdx ffiIdx CTText

||| Generate C code to decode N-ary Candid arguments.
||| Returns (setup_code, set_arg_code_for_idris)
genArgDecodeCode : String -> List CandidType -> (String, String)
genArgDecodeCode _ [] = ("", "")
genArgDecodeCode pfx argTypes =
  let n = length argTypes
      header =
        "    load_candid_args();\n" ++
        "    if (arg_buf_size < 7) goto arg_done;\n" ++
        "    if (arg_buf[0]!='D'||arg_buf[1]!='I'||arg_buf[2]!='D'||arg_buf[3]!='L') goto arg_done;\n" ++
        "    { int32_t offset=4, new_offset;\n" ++
        "    uint64_t type_count = parse_leb128(offset, &new_offset); offset = new_offset;\n" ++
        "    for (uint64_t i=0; i<type_count; i++) { parse_leb128(offset, &new_offset); offset=new_offset; }\n" ++
        "    uint64_t arg_count = parse_leb128(offset, &new_offset); offset=new_offset;\n" ++
        "    if (arg_count < " ++ show n ++ ") goto arg_done_inner;\n" ++
        "    offset += " ++ show n ++ "; /* skip type codes */\n"
      -- Generate decode + set-arg for each argument
      go : Nat -> List CandidType -> (String, String)
      go _ [] = ("", "")
      go idx (t :: ts) =
        let ffiIdx   = idx + 1  -- FFI slots are 1-based (slot 0 = CMD)
            pair1 = genSingleArgDecode pfx idx ffiIdx t
            pair2 = go (idx + 1) ts
        in (fst pair1 ++ fst pair2, snd pair1 ++ snd pair2)
      pair = go 0 argTypes
      decodes = fst pair
      setArgs = snd pair
      footer = "    arg_done_inner: ; }\n    arg_done: ;\n"
  in (header ++ decodes ++ footer, setArgs)

-- =============================================================================
-- Reply Code Generation
-- =============================================================================

||| Generate C reply code for a given Candid return type
genReplyCode : String -> Maybe String -> CandidType -> String
genReplyCode pfx Nothing  CTText    = "    reply_candid_text(" ++ pfx ++ "_c_get_result_str());\n"
genReplyCode _   (Just fn) CTText   = "    reply_candid_text(" ++ fn ++ "());\n"
genReplyCode pfx _        CTNat     = "    reply_candid_nat((uint64_t)" ++ pfx ++ "_c_get_result_u64());\n"
genReplyCode pfx _        CTNat64   = "    reply_candid_nat((uint64_t)" ++ pfx ++ "_c_get_result_u64());\n"
genReplyCode pfx _        CTBool    = "    reply_candid_nat((uint64_t)" ++ pfx ++ "_c_get_result_i32());\n"
genReplyCode _   _        CTNull    = "    reply_empty();\n"
genReplyCode pfx Nothing  _         = "    reply_candid_text(" ++ pfx ++ "_c_get_result_str());\n"
genReplyCode _   (Just fn) _        = "    reply_candid_text(" ++ fn ++ "());\n"

-- =============================================================================
-- Injection Code Generation
-- =============================================================================

||| Generate C code for injecting ic0_time or constants
genInjectionCode : String -> List Injection -> String
genInjectionCode _ [] = ""
genInjectionCode pfx injs =
  let needsTime = any isTimeInj injs
      timeDecl  = if needsTime
                    then "    uint64_t _now = ic0_time() / 1000000000ULL;\n"
                    else ""
      injCode   = concatMap (genOneInj pfx) injs
  in timeDecl ++ injCode
  where
    isTimeInj : Injection -> Bool
    isTimeInj (InjectTime _)     = True
    isTimeInj (InjectTimePlus _ _) = True
    isTimeInj _ = False

    genOneInj : String -> Injection -> String
    genOneInj p (InjectTime n) =
      "    " ++ p ++ "_c_set_arg_i32(" ++ show n ++ ", (int32_t)_now);\n"
    genOneInj p (InjectConst n v) =
      "    " ++ p ++ "_c_set_arg_i32(" ++ show n ++ ", " ++ show v ++ ");\n"
    genOneInj p (InjectTimePlus n v) =
      "    " ++ p ++ "_c_set_arg_i32(" ++ show n ++ ", (int32_t)(_now + " ++ show v ++ "));\n"

-- =============================================================================
-- Method Entry Function Generation
-- =============================================================================

||| Generate one canister_query_* or canister_update_* function
genMethodEntryWithAnnotations : GenOptions -> List (String, CmdMapEntry) -> (DidMethod, Nat) -> String
genMethodEntryWithAnnotations opts cmdMapEntries pair =
  let (method, cmdId) = pair
      _ = cmdId in
  let pfx        = opts.ffiPrefix
      kind       = if method.isQuery then "query" else "update"
      funcName   = "canister_" ++ kind ++ "_" ++ method.name
      exportName = "canister_" ++ kind ++ " " ++ method.name
      entry      = lookup method.name cmdMapEntries
      injections = maybe [] (.injections) entry
      resultFn   = entry >>= (.resultFn)
      argDecPair = genArgDecodeCode pfx method.argTypes
      setupCode  = fst argDecPair
      setArgCode = snd argDecPair
      replyCode  = genReplyCode pfx resultFn method.returnType
      injCode    = genInjectionCode pfx injections
      callCode   =
        "    " ++ pfx ++ "_reset_ffi();\n" ++
        "    " ++ pfx ++ "_c_set_arg_i32(0, " ++ cmdMacroName method.name ++ ");\n" ++
        (if null method.argTypes then "" else setArgCode) ++
        injCode ++
        "    void* closure = __mainExpression_0();\n" ++
        "    idris2_trampoline(closure);\n"
  in "__attribute__((used, visibility(\"default\"), export_name(\"" ++ exportName ++ "\")))\n" ++
     "void " ++ funcName ++ "(void) {\n" ++
     "    debug(\"" ++ pfx ++ ": " ++ method.name ++ "\");\n" ++
     setupCode ++
     callCode ++
     replyCode ++
     "}\n\n"

||| Backward-compatible entry generation (no annotations)
genMethodEntry : GenOptions -> (DidMethod, Nat) -> String
genMethodEntry opts = genMethodEntryWithAnnotations opts []

-- =============================================================================
-- CMD_ID Defines
-- =============================================================================

genCmdDefines : List (DidMethod, Nat) -> String
genCmdDefines pairs =
  unlines (map genLine pairs) ++ "\n"
  where
    genLine : (DidMethod, Nat) -> String
    genLine (m, n) = "#define " ++ cmdMacroName m.name ++ " " ++ show n

-- =============================================================================
-- Heartbeat Generation
-- =============================================================================

||| Generate canister_heartbeat function
genHeartbeat : GenOptions -> String
genHeartbeat opts =
  case opts.heartbeatCmd of
    Nothing => ""
    Just cmd =>
      let pfx = opts.ffiPrefix
          checkpointCode = case opts.heartbeatCheckpoint of
            Nothing => ""
            Just interval =>
              "    /* stable memory checkpoint every " ++ show interval ++ " heartbeats */\n" ++
              "    if (++_hb_count % " ++ show interval ++ " == 0) {\n" ++
              "        sqlite_stable_save(1, _now);\n" ++
              "    }\n"
          counterDecl = case opts.heartbeatCheckpoint of
            Nothing => ""
            Just _  => "    static uint32_t _hb_count = 0;\n"
      in "\n/* =============================================================================\n" ++
         " * Heartbeat\n" ++
         " * ============================================================================= */\n\n" ++
         "__attribute__((used, visibility(\"default\"), export_name(\"canister_heartbeat\")))\n" ++
         "void canister_heartbeat(void) {\n" ++
         counterDecl ++
         "    " ++ pfx ++ "_reset_ffi();\n" ++
         "    " ++ pfx ++ "_c_set_arg_i32(0, " ++ show cmd ++ ");\n" ++
         "    uint64_t _now = ic0_time() / 1000000000ULL;\n" ++
         "    " ++ pfx ++ "_c_set_arg_i32(1, (int32_t)_now);\n" ++
         "    void* closure = __mainExpression_0();\n" ++
         "    idris2_trampoline(closure);\n" ++
         checkpointCode ++
         "}\n\n"

-- =============================================================================
-- Fixed Header
-- =============================================================================

||| Fixed C header: IC0 externs, Idris2 forward decls, FFI bridge, Candid parsers, reply helpers, lifecycle
fixedHeader : GenOptions -> List (String, CmdMapEntry) -> String
fixedHeader opts cmdMapEntries =
  let pfx = opts.ffiPrefix
      -- Collect all unique custom result functions from annotations
      customFns = nub $ mapMaybe (\(_, e) => e.resultFn) cmdMapEntries
      customFnDecls = concatMap (\fn => "extern const char* " ++ fn ++ "(void);\n") customFns
  in
    "/*\n" ++
    " * Canister Entry Points - AUTO-GENERATED from can.did\n" ++
    " *\n" ++
    " * DO NOT EDIT BY HAND. Regenerate with:\n" ++
    " *   idris2-icwasm gen-entry --did=can.did --prefix=" ++ pfx ++ " --lib=" ++ opts.libName ++ " --out=<this file>\n" ++
    " */\n" ++
    "#include <stdint.h>\n" ++
    "#include <string.h>\n" ++
    "\n" ++
    "/* IC0 imports */\n" ++
    "extern void ic0_msg_reply(void);\n" ++
    "extern void ic0_msg_reply_data_append(int32_t src, int32_t size);\n" ++
    "extern int32_t ic0_msg_arg_data_size(void);\n" ++
    "extern void ic0_msg_arg_data_copy(int32_t dst, int32_t offset, int32_t size);\n" ++
    "extern int32_t ic0_msg_caller_size(void);\n" ++
    "extern void ic0_msg_caller_copy(int32_t dst, int32_t offset, int32_t size);\n" ++
    "extern int32_t ic0_msg_reject_code(void);\n" ++
    "extern int32_t ic0_msg_reject_msg_size(void);\n" ++
    "extern void ic0_msg_reject_msg_copy(int32_t dst, int32_t offset, int32_t size);\n" ++
    "extern uint64_t ic0_time(void);\n" ++
    "extern void ic0_debug_print(int32_t src, int32_t size);\n" ++
    "extern void ic0_trap(int32_t src, int32_t size);\n" ++
    "extern int32_t ic0_canister_self_size(void);\n" ++
    "extern void ic0_canister_self_copy(int32_t dst, int32_t offset, int32_t size);\n" ++
    "extern int64_t ic0_stable64_grow(int64_t new_pages);\n" ++
    "extern void ic0_call_new(int32_t callee_src, int32_t callee_size,\n" ++
    "                         int32_t name_src, int32_t name_size,\n" ++
    "                         int32_t reply_fun, int32_t reply_env,\n" ++
    "                         int32_t reject_fun, int32_t reject_env);\n" ++
    "extern void ic0_call_data_append(int32_t src, int32_t size);\n" ++
    "extern void ic0_call_cycles_add128(uint64_t high, uint64_t low);\n" ++
    "extern int32_t ic0_call_perform(void);\n" ++
    "\n" ++
    "/* Forward declarations from Idris2 generated code */\n" ++
    "extern void* __mainExpression_0(void);\n" ++
    "extern void* idris2_trampoline(void*);\n" ++
    "\n" ++
    "/* Forward declarations from FFI bridge */\n" ++
    "extern void " ++ pfx ++ "_c_set_arg_i32(int32_t index, int32_t value);\n" ++
    "extern void " ++ pfx ++ "_c_set_arg_str(int32_t index, const char* value);\n" ++
    "extern int32_t " ++ pfx ++ "_c_get_result_i32(void);\n" ++
    "extern uint64_t " ++ pfx ++ "_c_get_result_u64(void);\n" ++
    "extern const char* " ++ pfx ++ "_c_get_result_str(void);\n" ++
    "extern void " ++ pfx ++ "_reset_ffi(void);\n" ++
    (if null customFns then "" else "\n/* Custom result functions */\n" ++ customFnDecls) ++
    "\n" ++
    "/* Forward declarations from SQLite persistence */\n" ++
    "extern int sqlite_stable_save(uint32_t schema_version, uint64_t timestamp);\n" ++
    "extern int sqlite_stable_load(uint32_t* out_schema_version);\n" ++
    "extern int sqlite_stable_has_snapshot(void);\n" ++
    "\n" ++
    "/* =============================================================================\n" ++
    " * Candid Argument Parsing\n" ++
    " * ============================================================================= */\n" ++
    "\n" ++
    "static uint8_t arg_buf[4096];\n" ++
    "static int32_t arg_buf_size = 0;\n" ++
    "\n" ++
    "static void load_candid_args(void) {\n" ++
    "    arg_buf_size = ic0_msg_arg_data_size();\n" ++
    "    if (arg_buf_size > (int32_t)sizeof(arg_buf)) arg_buf_size = sizeof(arg_buf);\n" ++
    "    if (arg_buf_size > 0) ic0_msg_arg_data_copy((int32_t)(uintptr_t)arg_buf, 0, arg_buf_size);\n" ++
    "}\n" ++
    "\n" ++
    "static uint64_t parse_leb128(int32_t offset, int32_t* new_offset) {\n" ++
    "    uint64_t result = 0; int shift = 0;\n" ++
    "    while (offset < arg_buf_size) {\n" ++
    "        uint8_t byte = arg_buf[offset++];\n" ++
    "        result |= ((uint64_t)(byte & 0x7F)) << shift;\n" ++
    "        if ((byte & 0x80) == 0) break;\n" ++
    "        shift += 7;\n" ++
    "    }\n" ++
    "    *new_offset = offset;\n" ++
    "    return result;\n" ++
    "}\n" ++
    "\n" ++
    "static char text_arg_buf[4096];\n" ++
    "static int32_t text_arg_len = 0;\n" ++
    "\n" ++
    "/* =============================================================================\n" ++
    " * Helper Functions\n" ++
    " * ============================================================================= */\n" ++
    "\n" ++
    "static void debug(const char* msg) {\n" ++
    "    ic0_debug_print((int32_t)(uintptr_t)msg, (int32_t)strlen(msg));\n" ++
    "}\n" ++
    "\n" ++
    "static void reply_empty(void) {\n" ++
    "    static const uint8_t r[] = { 0x44, 0x49, 0x44, 0x4C, 0x00, 0x00 };\n" ++
    "    ic0_msg_reply_data_append((int32_t)(uintptr_t)r, sizeof(r));\n" ++
    "    ic0_msg_reply();\n" ++
    "}\n" ++
    "\n" ++
    "static void reply_candid_text(const char* text) {\n" ++
    "    uint32_t len = (uint32_t)strlen(text);\n" ++
    "    uint8_t header[] = { 0x44, 0x49, 0x44, 0x4C, 0x00, 0x01, 0x71 };\n" ++
    "    ic0_msg_reply_data_append((int32_t)(uintptr_t)header, 7);\n" ++
    "    uint8_t leb_buf[5]; int32_t leb_len = 0;\n" ++
    "    uint32_t val = len;\n" ++
    "    do { uint8_t byte=(uint8_t)(val&0x7F); val>>=7; if(val!=0) byte|=0x80; leb_buf[leb_len++]=byte; } while(val!=0);\n" ++
    "    ic0_msg_reply_data_append((int32_t)(uintptr_t)leb_buf, leb_len);\n" ++
    "    ic0_msg_reply_data_append((int32_t)(uintptr_t)text, (int32_t)len);\n" ++
    "    ic0_msg_reply();\n" ++
    "}\n" ++
    "\n" ++
    "static void reply_candid_nat(uint64_t value) {\n" ++
    "    uint8_t header[] = { 0x44, 0x49, 0x44, 0x4C, 0x00, 0x01, 0x7D };\n" ++
    "    ic0_msg_reply_data_append((int32_t)(uintptr_t)header, 7);\n" ++
    "    uint8_t buf[10]; int i=0;\n" ++
    "    do { buf[i]=(uint8_t)(value&0x7F); value>>=7; if(value!=0) buf[i]|=0x80; i++; } while(value!=0);\n" ++
    "    ic0_msg_reply_data_append((int32_t)(uintptr_t)buf, i);\n" ++
    "    ic0_msg_reply();\n" ++
    "}\n" ++
    "\n" ++
    "/* =============================================================================\n" ++
    " * Canister Lifecycle\n" ++
    " * ============================================================================= */\n" ++
    "\n" ++
    "__attribute__((used, visibility(\"default\"), export_name(\"canister_init\")))\n" ++
    "void canister_init(void) {\n" ++
    "    debug(\"canister_init\");\n" ++
    "    ic0_stable64_grow(10);\n" ++
    (if opts.initFn == "" then "" else "    " ++ opts.initFn ++ "();\n") ++
    "    " ++ pfx ++ "_reset_ffi();\n" ++
    "    " ++ pfx ++ "_c_set_arg_i32(0, 0); /* CMD 0 = init */\n" ++
    "    void* closure = __mainExpression_0();\n" ++
    "    idris2_trampoline(closure);\n" ++
    "}\n" ++
    "\n" ++
    "__attribute__((used, visibility(\"default\"), export_name(\"canister_pre_upgrade\")))\n" ++
    "void canister_pre_upgrade(void) {\n" ++
    "    debug(\"canister_pre_upgrade\");\n" ++
    (if opts.initFn == "" then "" else "    " ++ opts.initFn ++ "(); /* DB must be open for serialize */\n") ++
    "    uint64_t timestamp = ic0_time();\n" ++
    "    sqlite_stable_save(1, timestamp);\n" ++
    "}\n" ++
    "\n" ++
    "__attribute__((used, visibility(\"default\"), export_name(\"canister_post_upgrade\")))\n" ++
    "void canister_post_upgrade(void) {\n" ++
    "    debug(\"canister_post_upgrade\");\n" ++
    (if opts.initFn == "" then "" else "    " ++ opts.initFn ++ "(); /* DB must be open before load */\n") ++
    "    if (sqlite_stable_has_snapshot()) {\n" ++
    "        uint32_t schema_version = 0;\n" ++
    "        sqlite_stable_load(&schema_version);\n" ++
    "    }\n" ++
    "    " ++ pfx ++ "_reset_ffi();\n" ++
    "    " ++ pfx ++ "_c_set_arg_i32(0, 0); /* CMD 0 = init */\n" ++
    "    void* closure = __mainExpression_0();\n" ++
    "    idris2_trampoline(closure);\n" ++
    "}\n" ++
    "\n"

-- =============================================================================
-- Full File Generation
-- =============================================================================

||| Generate the complete canister_entry.c content
||| cmdMap: list of (methodName, cmdId) from --cmd-map file; empty = auto-number from 0
export
generateCanisterEntry : GenOptions -> List DidMethod -> List TypeDef -> List (String, Nat) -> String
generateCanisterEntry opts methods defs cmdMap =
  let _ = defs
      pairs   = if null cmdMap
                  then assignCmdIds methods
                  else assignCmdIdsWithMap methods cmdMap
      header  = fixedHeader opts []
      defines = "/* CMD_ID mapping (must match Main.idr case cmd of) */\n" ++ genCmdDefines pairs
      entries = concatMap (genMethodEntry opts) pairs
      heartbeat = genHeartbeat opts
  in header ++ defines ++ entries ++ heartbeat

||| Generate canister_entry.c with full annotation support (cmd-map entries)
export
generateCanisterEntryFull : GenOptions -> List DidMethod -> List TypeDef -> List (String, CmdMapEntry) -> String
generateCanisterEntryFull opts methods defs cmdMapEntries =
  let _ = defs
      cmdMap  = map (\(n, e) => (n, e.cmdId)) cmdMapEntries
      pairs   = if null cmdMap
                  then assignCmdIds methods
                  else assignCmdIdsWithMap methods cmdMap
      header  = fixedHeader opts cmdMapEntries
      defines = "/* CMD_ID mapping (must match Main.idr case cmd of) */\n" ++ genCmdDefines pairs
      entries = concatMap (genMethodEntryWithAnnotations opts cmdMapEntries) pairs
      heartbeat = genHeartbeat opts
  in header ++ defines ++ entries ++ heartbeat
