||| WASM Function Types
|||
||| Types for representing WASM functions and their mapping to Idris2 source.
module DfxCoverage.WasmMapper.WasmFunc

import Data.List
import Data.List1
import Data.Maybe
import Data.String

%default covering

-- =============================================================================
-- WASM Function Types
-- =============================================================================

||| Function kind in WASM
public export
data WasmFuncKind
  = ImportedFunc String   -- Imported from module (e.g., "ic0")
  | DefinedFunc           -- Defined in this module
  | ExportedFunc String   -- Exported with given name

public export
Show WasmFuncKind where
  show (ImportedFunc mod) = "import:" ++ mod
  show DefinedFunc = "defined"
  show (ExportedFunc name) = "export:" ++ name

public export
Eq WasmFuncKind where
  (ImportedFunc a) == (ImportedFunc b) = a == b
  DefinedFunc == DefinedFunc = True
  (ExportedFunc a) == (ExportedFunc b) = a == b
  _ == _ = False

||| A WASM function with metadata
public export
record WasmFunc where
  constructor MkWasmFunc
  funcIdx : Nat            -- Function index in WASM module
  wasmName : String        -- Name from WASM name section
  kind : WasmFuncKind      -- Import/defined/export
  codeSize : Maybe Nat     -- Code size in bytes (for defined funcs)

public export
Show WasmFunc where
  show f = "func[" ++ show f.funcIdx ++ "] " ++ f.wasmName ++
           " (" ++ show f.kind ++ ")" ++
           maybe "" (\s => " [" ++ show s ++ " bytes]") f.codeSize

public export
Eq WasmFunc where
  a == b = a.funcIdx == b.funcIdx && a.wasmName == b.wasmName

-- =============================================================================
-- Idris2 Source Mapping
-- =============================================================================

||| Idris2 qualified name
public export
record Idris2QName where
  constructor MkIdris2QName
  modulePath : List String  -- ["Module", "SubModule"]
  funcName : String         -- "funcName"

public export
Show Idris2QName where
  show qn = joinBy "." (qn.modulePath ++ [qn.funcName])

public export
Eq Idris2QName where
  a == b = a.modulePath == b.modulePath && a.funcName == b.funcName

||| Full module.function path
export
fullName : Idris2QName -> String
fullName = show

||| Just the function name without module
export
shortName : Idris2QName -> String
shortName qn = qn.funcName

||| Just the module path
export
moduleName : Idris2QName -> String
moduleName qn = joinBy "." qn.modulePath

-- =============================================================================
-- Mapping Types
-- =============================================================================

||| Mapping from WASM function to Idris2 source
public export
record FuncMapping where
  constructor MkFuncMapping
  wasmFunc : WasmFunc
  idris2Name : Maybe Idris2QName  -- Nothing if can't map (runtime/FFI funcs)

public export
Show FuncMapping where
  show fm = show fm.wasmFunc ++
            maybe " [unmapped]" (\qn => " -> " ++ show qn) fm.idris2Name

||| Function mapping table
public export
record FuncMappingTable where
  constructor MkFuncMappingTable
  mappings : List FuncMapping
  totalFuncs : Nat
  mappedFuncs : Nat
  importedFuncs : Nat

public export
Show FuncMappingTable where
  show t = "FuncMappingTable: " ++ show t.mappedFuncs ++ "/" ++
           show t.totalFuncs ++ " mapped, " ++
           show t.importedFuncs ++ " imports"

-- =============================================================================
-- Name Parsing (WASM → Idris2)
-- =============================================================================

||| Parse Idris2-style name from WASM name section
|||
||| WASM mangling conventions:
||| - Dots replaced with underscores: Module.func → Module_func
||| - Special chars mangled: Module.func' → Module_func_0027
||| - Unicode mangled to _XXXX format
export
parseIdris2Name : String -> Maybe Idris2QName
parseIdris2Name wasmName =
  let -- Split on underscores
      parts = forget $ split (== '_') wasmName
  in case parts of
       [] => Nothing
       [single] => Just $ MkIdris2QName [] single
       _ =>
         -- Last part is function name, rest is module path
         let funcName = fromMaybe "" (last' parts)
             moduleParts = fromMaybe [] (init' parts)
         in if null funcName
              then Nothing
              else Just $ MkIdris2QName moduleParts funcName

||| Check if name looks like Idris2-generated function
export
isIdris2Generated : String -> Bool
isIdris2Generated name =
  -- Idris2 generated names typically start with uppercase module
  -- and don't have certain patterns from runtime
  not (isPrefixOf "__" name) &&
  not (isPrefixOf "wasm" name) &&
  not (isInfixOf "builtin" (toLower name)) &&
  (case unpack name of
     (c :: _) => isUpper c || c == '_'
     [] => False)

||| Check if function is an IC0 import
export
isIc0Import : WasmFunc -> Bool
isIc0Import f = case f.kind of
  ImportedFunc mod => mod == "ic0" || isPrefixOf "ic0" mod
  _ => False

||| Check if function is a WASI import
export
isWasiImport : WasmFunc -> Bool
isWasiImport f = case f.kind of
  ImportedFunc mod => isInfixOf "wasi" (toLower mod)
  _ => False

-- =============================================================================
-- Lookup Functions
-- =============================================================================

||| Look up function by index
export
lookupByIdx : Nat -> FuncMappingTable -> Maybe FuncMapping
lookupByIdx idx t = find (\m => m.wasmFunc.funcIdx == idx) t.mappings

||| Look up function by WASM name
export
lookupByWasmName : String -> FuncMappingTable -> Maybe FuncMapping
lookupByWasmName name t = find (\m => m.wasmFunc.wasmName == name) t.mappings

||| Look up function by Idris2 name
export
lookupByIdris2Name : Idris2QName -> FuncMappingTable -> Maybe FuncMapping
lookupByIdris2Name qn t = find (\m => m.idris2Name == Just qn) t.mappings

||| Get all functions in a module
export
functionsInModule : String -> FuncMappingTable -> List FuncMapping
functionsInModule modPath t =
  filter (matchesModule modPath) t.mappings
  where
    matchesModule : String -> FuncMapping -> Bool
    matchesModule mp fm = case fm.idris2Name of
      Nothing => False
      Just qn => moduleName qn == mp

-- =============================================================================
-- Table Construction
-- =============================================================================

||| Convert WasmFunc to FuncMapping
toFuncMapping : WasmFunc -> FuncMapping
toFuncMapping f =
  let idris2 = if isIdris2Generated f.wasmName
                 then parseIdris2Name f.wasmName
                 else Nothing
  in MkFuncMapping f idris2

isImportFunc : WasmFunc -> Bool
isImportFunc f = case f.kind of
  ImportedFunc _ => True
  _ => False

||| Build mapping table from list of WASM functions
export
buildMappingTable : List WasmFunc -> FuncMappingTable
buildMappingTable funcs =
  let mappings = map toFuncMapping funcs
  in MkFuncMappingTable
       mappings
       (length funcs)
       (length $ filter (isJust . idris2Name) mappings)
       (length $ filter isImportFunc funcs)
