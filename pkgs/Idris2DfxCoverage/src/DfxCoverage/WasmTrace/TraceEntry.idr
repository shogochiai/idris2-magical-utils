||| WASM Trace Entry Types
|||
||| Represents execution trace entries from WASM runtime instrumentation.
||| Compatible with wasmtime profiler output format.
module DfxCoverage.WasmTrace.TraceEntry

import Data.List
import Data.Maybe
import Data.String

%default covering

-- =============================================================================
-- Trace Entry Types
-- =============================================================================

||| WASM opcode categories for coverage analysis
public export
data WasmOpcodeCategory
  = OpControl      -- call, call_indirect, br, br_if, br_table, return
  | OpLocal        -- local.get, local.set, local.tee
  | OpGlobal       -- global.get, global.set
  | OpMemory       -- load, store operations
  | OpNumeric      -- i32.add, i64.mul, etc.
  | OpConversion   -- i32.wrap_i64, etc.
  | OpOther        -- everything else

public export
Show WasmOpcodeCategory where
  show OpControl = "control"
  show OpLocal = "local"
  show OpGlobal = "global"
  show OpMemory = "memory"
  show OpNumeric = "numeric"
  show OpConversion = "conversion"
  show OpOther = "other"

public export
Eq WasmOpcodeCategory where
  OpControl == OpControl = True
  OpLocal == OpLocal = True
  OpGlobal == OpGlobal = True
  OpMemory == OpMemory = True
  OpNumeric == OpNumeric = True
  OpConversion == OpConversion = True
  OpOther == OpOther = True
  _ == _ = False

||| Categorize opcode string
export
categorizeOpcode : String -> WasmOpcodeCategory
categorizeOpcode op =
  let lower = toLower op
  in if isInfixOf "call" lower || isInfixOf "br" lower ||
        isInfixOf "return" lower || isInfixOf "block" lower ||
        isInfixOf "loop" lower || isInfixOf "if" lower || isInfixOf "else" lower
       then OpControl
       else if isInfixOf "local" lower
         then OpLocal
         else if isInfixOf "global" lower
           then OpGlobal
           else if isInfixOf "load" lower || isInfixOf "store" lower
             then OpMemory
             else if isInfixOf "add" lower || isInfixOf "sub" lower ||
                     isInfixOf "mul" lower || isInfixOf "div" lower ||
                     isInfixOf "and" lower || isInfixOf "or" lower ||
                     isInfixOf "xor" lower || isInfixOf "shl" lower ||
                     isInfixOf "shr" lower || isInfixOf "eqz" lower ||
                     isInfixOf "eq" lower || isInfixOf "ne" lower ||
                     isInfixOf "lt" lower || isInfixOf "gt" lower ||
                     isInfixOf "le" lower || isInfixOf "ge" lower
               then OpNumeric
               else if isInfixOf "wrap" lower || isInfixOf "extend" lower ||
                       isInfixOf "trunc" lower || isInfixOf "convert" lower
                 then OpConversion
                 else OpOther

-- =============================================================================
-- Trace Entry
-- =============================================================================

||| A single trace entry from WASM execution
|||
||| @funcIdx  WASM function index (from call/call_indirect)
||| @pc       Program counter (instruction offset within function)
||| @opcode   WASM opcode name
||| @depth    Call stack depth
public export
record WasmTraceEntry where
  constructor MkWasmTraceEntry
  funcIdx : Nat
  pc : Nat
  opcode : String
  depth : Nat

public export
Show WasmTraceEntry where
  show te = "func[" ++ show te.funcIdx ++ "] PC=" ++ show te.pc ++
            " " ++ te.opcode ++ " (depth=" ++ show te.depth ++ ")"

public export
Eq WasmTraceEntry where
  a == b = a.funcIdx == b.funcIdx && a.pc == b.pc && a.opcode == b.opcode

||| Get opcode category for trace entry
export
traceCategory : WasmTraceEntry -> WasmOpcodeCategory
traceCategory te = categorizeOpcode te.opcode

-- =============================================================================
-- Function Call Tracking
-- =============================================================================

||| Function call event (entry or exit)
public export
data FuncCallEvent
  = FuncEnter Nat   -- Function index entered
  | FuncExit Nat    -- Function index exited

public export
Show FuncCallEvent where
  show (FuncEnter idx) = "ENTER func[" ++ show idx ++ "]"
  show (FuncExit idx) = "EXIT func[" ++ show idx ++ "]"

public export
Eq FuncCallEvent where
  (FuncEnter a) == (FuncEnter b) = a == b
  (FuncExit a) == (FuncExit b) = a == b
  _ == _ = False

||| Extract function calls from trace
export
extractFuncCalls : List WasmTraceEntry -> List FuncCallEvent
extractFuncCalls = go Nothing
  where
    go : Maybe Nat -> List WasmTraceEntry -> List FuncCallEvent
    go _ [] = []
    go lastFunc (te :: rest) =
      let currentFunc = te.funcIdx
          events = case lastFunc of
            Nothing => [FuncEnter currentFunc]
            Just prev =>
              if prev == currentFunc
                then []
                else [FuncExit prev, FuncEnter currentFunc]
      in events ++ go (Just currentFunc) rest

-- =============================================================================
-- Coverage Aggregation
-- =============================================================================

||| Hit count per function
public export
record FuncHitCount where
  constructor MkFuncHitCount
  funcIdx : Nat
  hitCount : Nat
  uniquePCs : Nat   -- Number of unique PC values hit

public export
Show FuncHitCount where
  show fhc = "func[" ++ show fhc.funcIdx ++ "]: " ++ show fhc.hitCount ++
             " hits, " ++ show fhc.uniquePCs ++ " unique PCs"

||| Aggregate hit counts from trace
export
aggregateHits : List WasmTraceEntry -> List FuncHitCount
aggregateHits traces =
  let grouped = groupByFunc traces
  in map summarize grouped
  where
    groupByFunc : List WasmTraceEntry -> List (Nat, List WasmTraceEntry)
    groupByFunc [] = []
    groupByFunc (te :: rest) =
      let (matching, others) = partition (\t => t.funcIdx == te.funcIdx) rest
          grouped = groupByFunc others
      in (te.funcIdx, te :: matching) :: grouped

    countUniquePCs : List WasmTraceEntry -> Nat
    countUniquePCs tes = length $ nub $ map (.pc) tes

    summarize : (Nat, List WasmTraceEntry) -> FuncHitCount
    summarize (idx, tes) = MkFuncHitCount idx (length tes) (countUniquePCs tes)

-- =============================================================================
-- Branch Coverage
-- =============================================================================

||| Branch point in WASM (if/br_if/br_table)
public export
record WasmBranchPoint where
  constructor MkWasmBranchPoint
  funcIdx : Nat
  pc : Nat
  branchType : String  -- "if", "br_if", "br_table"
  taken : Bool         -- True if branch was taken

public export
Show WasmBranchPoint where
  show bp = "func[" ++ show bp.funcIdx ++ "] PC=" ++ show bp.pc ++
            " " ++ bp.branchType ++ ": " ++
            if bp.taken then "TAKEN" else "NOT_TAKEN"

||| Extract branch points from trace (simplified)
||| Full implementation would track consecutive if/else execution
export
extractBranchPoints : List WasmTraceEntry -> List WasmBranchPoint
extractBranchPoints = mapMaybe toBranch
  where
    isBranchOp : String -> Bool
    isBranchOp op = isInfixOf "br_if" op || isInfixOf "if" op

    toBranch : WasmTraceEntry -> Maybe WasmBranchPoint
    toBranch te =
      if isBranchOp te.opcode
        then Just $ MkWasmBranchPoint te.funcIdx te.pc te.opcode True
        else Nothing
