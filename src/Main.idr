||| idris2-evm CLI - Pure Idris2 EVM Interpreter
|||
||| This tool executes EVM bytecode through a pure Idris2 interpreter.
||| When compiled with --coverage, the Chez Scheme profiler tracks
||| which interpreter branches are executed, giving semantic coverage
||| of the EVM code.
|||
||| Usage:
|||   idris2-evm-run [options] <bytecode.hex>
|||   idris2-evm-run --calldata 0x371303c0 <bytecode.hex>
module Main

import EVM.Word256
import EVM.Stack
import EVM.Memory
import EVM.Storage
import EVM.Opcodes
import EVM.Bytecode
import EVM.Interpreter
import EVM.WorldState
import EVM.MultiInterpreter

import System
import System.File
import Data.List
import Data.List1
import Data.String
import Data.Maybe

%default covering

-- =============================================================================
-- CLI Options
-- =============================================================================

record Options where
  constructor MkOptions
  bytecodeFile : Maybe String
  bytecodeHex : Maybe String
  calldataHex : String
  callValueHex : Maybe String                       -- Value to send with call (in wei, hex)
  gasLimit : Nat
  showHelp : Bool
  showVersion : Bool
  verbose : Bool
  disassemble : Bool
  trace : Bool
  traceFile : Maybe String
  loadStorage : Maybe String
  saveStorage : Maybe String
  labelsFile : Maybe String                         -- Labels CSV for coverage mapping
  coverageJson : Maybe String                       -- Output JSON coverage report to file
  -- Multi-contract options
  contracts : List (String, String, Maybe String)  -- (address, bytecode file, optional storage file)
  callAddress : Maybe String                        -- Address to call in multi-contract mode
  loadWorld : Maybe String                          -- Load world state from JSON
  saveWorld : Maybe String                          -- Save world state to JSON

defaultOptions : Options
defaultOptions = MkOptions Nothing Nothing "" Nothing 1000000 False False False False False Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing

-- =============================================================================
-- Argument Parsing
-- =============================================================================

parseArgs : List String -> Options -> Options
parseArgs [] opts = opts
parseArgs ("--help" :: rest) opts = parseArgs rest ({ showHelp := True } opts)
parseArgs ("-h" :: rest) opts = parseArgs rest ({ showHelp := True } opts)
parseArgs ("--version" :: rest) opts = parseArgs rest ({ showVersion := True } opts)
parseArgs ("-v" :: rest) opts = parseArgs rest ({ showVersion := True } opts)
parseArgs ("--verbose" :: rest) opts = parseArgs rest ({ verbose := True } opts)
parseArgs ("--disassemble" :: rest) opts = parseArgs rest ({ disassemble := True } opts)
parseArgs ("-d" :: rest) opts = parseArgs rest ({ disassemble := True } opts)
parseArgs ("--trace" :: rest) opts = parseArgs rest ({ trace := True } opts)
parseArgs ("-t" :: rest) opts = parseArgs rest ({ trace := True } opts)
parseArgs ("--trace-file" :: f :: rest) opts = parseArgs rest ({ trace := True, traceFile := Just f } opts)
parseArgs ("--gas" :: g :: rest) opts =
  let gas = fromMaybe 1000000 (parsePositive {a=Nat} g)
  in parseArgs rest ({ gasLimit := gas } opts)
parseArgs ("--calldata" :: cd :: rest) opts =
  parseArgs rest ({ calldataHex := cd } opts)
parseArgs ("--callvalue" :: val :: rest) opts =
  parseArgs rest ({ callValueHex := Just val } opts)
parseArgs ("--bytecode" :: bc :: rest) opts =
  parseArgs rest ({ bytecodeHex := Just bc } opts)
parseArgs ("--load-storage" :: file :: rest) opts =
  parseArgs rest ({ loadStorage := Just file } opts)
parseArgs ("--save-storage" :: file :: rest) opts =
  parseArgs rest ({ saveStorage := Just file } opts)
parseArgs ("--contract" :: spec :: rest) opts =
  let parts = forget $ split (== ':') spec
      parsed : (String, String, Maybe String)
      parsed = case parts of
        [a, c] => (a, c, Nothing)
        [a, c, s] => (a, c, Just s)
        _ => ("0x0", spec, Nothing)
  in parseArgs rest ({ contracts $= (parsed ::) } opts)
parseArgs ("--call" :: addr :: rest) opts =
  parseArgs rest ({ callAddress := Just addr } opts)
parseArgs ("--load-world" :: file :: rest) opts =
  parseArgs rest ({ loadWorld := Just file } opts)
parseArgs ("--save-world" :: file :: rest) opts =
  parseArgs rest ({ saveWorld := Just file } opts)
parseArgs ("--labels" :: file :: rest) opts =
  parseArgs rest ({ labelsFile := Just file } opts)
parseArgs ("--coverage-json" :: file :: rest) opts =
  parseArgs rest ({ coverageJson := Just file } opts)
parseArgs (arg :: rest) opts =
  if isPrefixOf "-" arg
    then parseArgs rest opts  -- Skip unknown flags
    else parseArgs rest ({ bytecodeFile := Just arg } opts)

-- =============================================================================
-- Help Text
-- =============================================================================

helpText : String
helpText = """
idris2-evm - Pure Idris2 EVM Interpreter

USAGE:
  idris2-evm-run [options] <bytecode-file>
  idris2-evm-run --bytecode 0x600160... [options]

DESCRIPTION:
  Executes EVM bytecode through a pure Idris2 interpreter.

  When this tool is compiled with Idris2's --coverage flag, the Chez Scheme
  profiler tracks which interpreter branches are executed. This gives
  semantic coverage of your EVM/Yul code mapped back to Idris2 functions.

OPTIONS:
  -h, --help              Show this help message
  -v, --version           Show version
  --verbose               Verbose output (show each step)
  -d, --disassemble       Disassemble bytecode and exit
  -t, --trace             Output opcode execution trace (for coverage)
  --trace-file <file>     Write trace to file instead of stdout
  --gas <limit>           Gas limit (default: 1000000)
  --calldata <hex>        Calldata as hex (e.g., 0x371303c0)
  --callvalue <hex>       Call value in wei as hex (e.g., 0x1000)
  --bytecode <hex>        Bytecode as hex (alternative to file)
  --load-storage <file>   Load storage state from JSON file
  --save-storage <file>   Save storage state to JSON file after execution

MULTI-CONTRACT OPTIONS:
  --contract <spec>       Deploy contract: address:bytecode[:storage]
                          Can be specified multiple times
  --call <address>        Address to call in multi-contract mode
  --load-world <file>     Load world state from simple format file
  --save-world <file>     Save world state to simple format file

EXAMPLES:
  # Execute bytecode file with calldata
  idris2-evm-run --calldata 0x371303c0 contract.bin

  # Execute inline bytecode (Counter increment)
  idris2-evm-run --bytecode 0x60016000540160005500 --calldata 0x

  # Disassemble bytecode
  idris2-evm-run -d contract.bin

  # Multi-contract execution (Proxy -> Dictionary -> Implementation)
  idris2-evm-run \\
    --contract 0x1000:proxy.bin:proxy_storage.json \\
    --contract 0x2000:dictionary.bin:dictionary_storage.json \\
    --contract 0x3000:members.bin \\
    --call 0x1000 \\
    --calldata 0xca6d56dc00...

INTEGRATION WITH COVERAGE:
  To collect coverage data, rebuild with --coverage:
    idris2 --cg chez --coverage src/Main.idr -o idris2-evm-run

  Then run the interpreter. The Chez .ssi files will contain hit counts
  for each branch in the interpreter, reflecting which EVM opcodes
  were executed.

SUPPORTED OPCODES:
  - Arithmetic: ADD, MUL, SUB, DIV, MOD, EXP, etc.
  - Comparison: LT, GT, EQ, ISZERO, etc.
  - Bitwise: AND, OR, XOR, NOT, SHL, SHR, etc.
  - Memory: MLOAD, MSTORE, MSTORE8
  - Storage: SLOAD, SSTORE
  - Control: JUMP, JUMPI, JUMPDEST, STOP, RETURN, REVERT
  - Stack: POP, PUSH0-PUSH32, DUP1-DUP16, SWAP1-SWAP16
  - Environment: CALLER, CALLVALUE, CALLDATALOAD, etc.
  - Block: NUMBER, TIMESTAMP, CHAINID, etc.
"""

versionText : String
versionText = "idris2-evm 0.2.0 - Pure Idris2 EVM Interpreter"

-- =============================================================================
-- Main Execution
-- =============================================================================

loadBytecode : Options -> IO (Either String Bytecode)
loadBytecode opts =
  case opts.bytecodeHex of
    Just hex =>
      case fromHex hex of
        Just code => pure (Right code)
        Nothing => pure (Left "Invalid bytecode hex")
    Nothing =>
      case opts.bytecodeFile of
        Nothing => pure (Left "No bytecode specified")
        Just path => do
          Right content <- readFile path
            | Left err => pure (Left $ "Failed to read file: " ++ show err)
          let trimmed = trim content
          case fromHex trimmed of
            Just code => pure (Right code)
            Nothing => pure (Left "Invalid bytecode in file")

loadStorageFile : String -> IO (Either String Storage)
loadStorageFile path = do
  Right content <- readFile path
    | Left err => pure (Left $ "Failed to read storage file: " ++ show err)
  case Storage.fromJSON (trim content) of
    Just store => pure (Right store)
    Nothing => pure (Left "Invalid storage JSON format")

saveStorageFile : String -> Storage -> IO (Either String ())
saveStorageFile path store = do
  Right _ <- writeFile path (Storage.toJSON store)
    | Left err => pure (Left $ "Failed to write storage file: " ++ show err)
  pure (Right ())

hexToWord256 : String -> Maybe Word256
hexToWord256 s =
  let chars = unpack s
      chars' = case chars of
        ('0' :: 'x' :: rest) => rest
        ('0' :: 'X' :: rest) => rest
        _ => chars
  in parseHexDigits chars' 0
  where
    hexVal : Char -> Maybe Integer
    hexVal c = if c >= '0' && c <= '9' then Just (cast (ord c - ord '0'))
               else if c >= 'a' && c <= 'f' then Just (cast (ord c - ord 'a' + 10))
               else if c >= 'A' && c <= 'F' then Just (cast (ord c - ord 'A' + 10))
               else Nothing
    parseHexDigits : List Char -> Integer -> Maybe Word256
    parseHexDigits [] acc = Just (fromInteger acc)
    parseHexDigits (c :: cs) acc = case hexVal c of
      Nothing => Nothing
      Just v => parseHexDigits cs (acc * 16 + v)

||| Convert Word256 to hex string (64 chars)
showHex256 : Word256 -> String
showHex256 w = padLeft 64 '0' (toHexString (toInteger w))
  where
    nibbleToChar : Integer -> Char
    nibbleToChar n = if n < 10 then chr (cast {to=Int} (ord '0' + cast n))
                     else chr (cast {to=Int} (ord 'a' + cast n - 10))
    toHexString : Integer -> String
    toHexString 0 = ""
    toHexString n = toHexString (n `div` 16) ++ singleton (nibbleToChar (n `mod` 16))
    padLeft : Nat -> Char -> String -> String
    padLeft n c s = let len = length s in
                    if len >= n then s
                    else replicate (n `minus` len) c ++ s

||| ProfileFlush event topic (keccak256("ProfileFlush(uint256[])"))
profileFlushTopic : Word256
profileFlushTopic = fromInteger 0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925

||| Check if a log is a ProfileFlush event
isProfileFlush : LogEntry -> Bool
isProfileFlush entry = case entry.logTopics of
  [t] => t == profileFlushTopic
  _ => False

||| Parse 32-byte words from log data as counter values
parseCounters : List Bits8 -> List Integer
parseCounters [] = []
parseCounters bytes =
  let (chunk, rest) = splitAt 32 bytes
      val = bytesToInteger chunk
  in if length chunk < 32 then []
     else val :: parseCounters rest
  where
    bytesToInteger : List Bits8 -> Integer
    bytesToInteger bs = foldl (\acc, b => acc * 256 + cast b) 0 bs

-- =============================================================================
-- Yul Name Demangling (from YulMapper.idr)
-- =============================================================================

||| Demangled Idris function representation
record IdrisFunc where
  constructor MkIdrisFunc
  modulePath : String
  idrisFuncName : String

Show IdrisFunc where
  show f = f.modulePath ++ "." ++ f.idrisFuncName

||| Skip numeric parts in list (for nested function markers like _n_1234_5678_u_)
skipNumeric : List String -> (List String, List String)
skipNumeric [] = ([], [])
skipNumeric (x :: rest) =
  case parsePositive {a=Nat} x of
    Just _ => let (nums, rem) = skipNumeric rest in (x :: nums, rem)
    Nothing => ([], x :: rest)

||| Find function marker (_u_, _m_, _n_) in underscore-split parts
findFuncMarker : List String -> List String -> Maybe (List String, String, List String)
findFuncMarker _ [] = Nothing
findFuncMarker acc ("u" :: rest) = Just (reverse acc, "u", rest)
findFuncMarker acc ("m" :: rest) = Just (reverse acc, "m", rest)
findFuncMarker acc ("n" :: rest) =
  case skipNumeric rest of
    (_, remaining) =>
      if null remaining then Nothing
      else Just (reverse acc, "n", remaining)
findFuncMarker acc (x :: rest) = findFuncMarker (x :: acc) rest

||| Demangle Yul function name to Idris module.function
||| e.g., Main_Functions_Vote_u_castVote → IdrisFunc "Main.Functions.Vote" "castVote"
demangleYulFunc : String -> Maybe IdrisFunc
demangleYulFunc name =
  let parts = forget $ split (== '_') name
  in case findFuncMarker [] parts of
       Just (modParts, marker, funcParts) =>
         let modulePath = joinBy "." modParts
             funcNm = joinBy "_" funcParts
         in Just $ MkIdrisFunc modulePath funcNm
       Nothing => Nothing

||| Get full demangled name as "Module.funcName" string
demangle : String -> String
demangle mangledName =
  case demangleYulFunc mangledName of
    Just f => show f
    Nothing => mangledName  -- Return original if can't demangle

-- =============================================================================
-- Label Entry with Demangled Name
-- =============================================================================

||| Label entry from CSV: (index, function_name, has_switch)
record LabelEntry where
  constructor MkLabelEntry
  labelIndex : Nat
  mangledName : String    -- Original mangled name (e.g., Main_Functions_Vote_u_castVote)
  demangledName : String  -- Demangled name (e.g., Main.Functions.Vote.castVote)
  hasSwitch : Bool

||| Parse a single CSV line (index,function_name,has_switch)
parseLabel : String -> Maybe LabelEntry
parseLabel line =
  let parts = forget (split (== ',') line)  -- List1 -> List
  in case parts of
    (idx :: name :: sw :: _) =>
      case parsePositive {a=Nat} idx of
        Just i => Just (MkLabelEntry i name (demangle name) (trim sw == "True"))
        Nothing => Nothing
    _ => Nothing

||| Load labels from CSV file
loadLabels : String -> IO (Either String (List LabelEntry))
loadLabels path = do
  Right content <- readFile path
    | Left err => pure (Left $ "Failed to read labels file: " ++ show err)
  let ls = lines content
  -- Skip header line
  let dataLines = drop 1 ls
  let labels = mapMaybe parseLabel dataLines
  pure (Right labels)

||| Look up demangled function name by index
lookupLabel : Nat -> List LabelEntry -> Maybe String
lookupLabel idx labels = map demangledName (find (\l => l.labelIndex == idx) labels)

||| Look up full label entry by index
lookupLabelEntry : Nat -> List LabelEntry -> Maybe LabelEntry
lookupLabelEntry idx labels = find (\l => l.labelIndex == idx) labels

-- =============================================================================
-- FunctionRuntimeHit (from idris2-coverage-core/Coverage/Core/RuntimeHit.idr)
-- =============================================================================

||| Per-function runtime coverage data
record FunctionRuntimeHit where
  constructor MkFunctionRuntimeHit
  funcName       : String   -- Demangled Idris name (e.g., "Main.Functions.Vote.castVote")
  mangledFunc    : String   -- Mangled name (e.g., "Main_Functions_Vote_u_castVote")
  canonicalCount : Nat      -- Static branch count from dumpcases (0 if no branches)
  executedCount  : Nat      -- Runtime hit count from ProfileFlush

Show FunctionRuntimeHit where
  show h = h.funcName ++ ": " ++ show h.executedCount ++
           (if h.canonicalCount > 0 then "/" ++ show h.canonicalCount else "")

||| Coverage percentage for a function
functionCoveragePercent : FunctionRuntimeHit -> Double
functionCoveragePercent h =
  if h.canonicalCount == 0 then 100.0
  else min 100.0 (cast h.executedCount / cast h.canonicalCount * 100.0)

||| Function coverage result
record CoverageResult where
  constructor MkCoverageResult
  totalFunctions : Nat
  hitFunctions   : Nat
  coveragePercent : Double
  hits : List FunctionRuntimeHit

||| Build FunctionRuntimeHit from label entry and hit count
buildHit : LabelEntry -> Integer -> FunctionRuntimeHit
buildHit lbl hitCount =
  let canonical : Nat = if lbl.hasSwitch then 1 else 0  -- Simplified: 1 branch if has_switch
  in MkFunctionRuntimeHit lbl.demangledName lbl.mangledName canonical (cast {to=Nat} hitCount)

||| Make hit from label and count
mkHit : List LabelEntry -> (Nat, Integer) -> Maybe FunctionRuntimeHit
mkHit labels (idx, cnt) = case lookupLabelEntry idx labels of
  Nothing => Nothing
  Just lbl => Just (buildHit lbl cnt)

||| Build coverage result from labels and counter data
buildCoverageResult : List LabelEntry -> List (Nat, Integer) -> CoverageResult
buildCoverageResult labels counters =
  let hits = mapMaybe (mkHit labels) counters in
  let nonZeroHits = filter (\h => h.executedCount > 0) hits in
  let totalFuncs = length labels in
  let hitFuncs = length nonZeroHits in
  let pct = if totalFuncs == 0 then 100.0 else cast hitFuncs / cast totalFuncs * 100.0 in
  MkCoverageResult totalFuncs hitFuncs pct hits

||| Convert coverage result to JSON string
coverageToJson : CoverageResult -> String
coverageToJson r = unlines
  [ "{"
  , "  \"totalFunctions\": " ++ show r.totalFunctions ++ ","
  , "  \"hitFunctions\": " ++ show r.hitFunctions ++ ","
  , "  \"coveragePercent\": " ++ show r.coveragePercent ++ ","
  , "  \"functions\": ["
  , "    " ++ joinBy ",\n    " (map hitToJson r.hits)
  , "  ]"
  , "}"
  ]
  where
    hitToJson : FunctionRuntimeHit -> String
    hitToJson h = "{\"name\": \"" ++ h.funcName ++ "\", " ++
                  "\"mangled\": \"" ++ h.mangledFunc ++ "\", " ++
                  "\"canonical\": " ++ show h.canonicalCount ++ ", " ++
                  "\"executed\": " ++ show h.executedCount ++ "}"

executeWithStorage : Bytecode -> List Bits8 -> Nat -> Storage -> Maybe Word256 -> Result
executeWithStorage code calldata gasLimit initialStore mCallValue =
  let callVal = fromMaybe Word256.zero mCallValue
      ctx = { callData := calldata, callValue := callVal } defaultContext
      vm = initVMWithStorage code ctx gasLimit initialStore
  in run gasLimit vm

runMain : Options -> IO ()
runMain opts = do
  codeResult <- loadBytecode opts
  case codeResult of
    Left err => do
      putStrLn $ "Error: " ++ err
      exitWith (ExitFailure 1)
    Right code => do
      -- Load initial storage if specified
      initialStore <- case opts.loadStorage of
        Nothing => pure Storage.empty
        Just path => do
          storeResult <- loadStorageFile path
          case storeResult of
            Left err => do
              putStrLn $ "Warning: " ++ err ++ ", using empty storage"
              pure Storage.empty
            Right store => do
              when opts.verbose $ do
                putStrLn $ "Loaded storage from: " ++ path
                let slots = Storage.toList store
                putStrLn $ "  " ++ show (length slots) ++ " slots loaded"
              pure store

      when opts.verbose $ do
        putStrLn $ "Bytecode size: " ++ show (codeSize code) ++ " bytes"
        putStrLn $ "Gas limit: " ++ show opts.gasLimit
        putStrLn $ "Calldata: " ++ (if null opts.calldataHex then "(none)" else opts.calldataHex)
        putStrLn ""

      if opts.disassemble
        then do
          putStrLn "=== Disassembly ==="
          putStrLn $ showDisassembly code
        else if opts.trace
          then do
            -- Load labels if specified
            labels <- case opts.labelsFile of
              Nothing => pure []
              Just path => do
                result <- loadLabels path
                case result of
                  Left err => do
                    putStrLn $ "Warning: " ++ err
                    pure []
                  Right lbls => do
                    when opts.verbose $
                      putStrLn $ "Loaded " ++ show (length lbls) ++ " labels"
                    pure lbls

            -- Trace mode: output opcode trace for coverage analysis
            let calldata = fromMaybe [] (fromHex opts.calldataHex)
            let mCallValue = opts.callValueHex >>= hexToWord256
            let (result, traceEntries, logs) = executeWithTrace code calldata opts.gasLimit initialStore mCallValue

            -- Output trace header
            let traceOutput = "step,pc,opcode,name,stack_depth\n" ++
                              unlines (map show traceEntries)

            case opts.traceFile of
              Nothing => putStrLn traceOutput
              Just path => do
                Right _ <- writeFile path traceOutput
                  | Left err => putStrLn $ "Error writing trace: " ++ show err
                putStrLn $ "Trace written to: " ++ path ++ " (" ++ show (length traceEntries) ++ " steps)"

            -- Output captured logs
            when (not $ null logs) $ do
              putStrLn $ "\n=== Logs (" ++ show (length logs) ++ " events) ==="
              let printLog : List LabelEntry -> Nat -> LogEntry -> IO ()
                  printLog lbls i entry = do
                    if isProfileFlush entry
                      then do
                        putStrLn $ "Log #" ++ show i ++ ": ProfileFlush"
                        let counters = parseCounters entry.logData
                        putStrLn $ "  Counters: " ++ show (length counters)
                        -- Build indexed counter list
                        let indexed = zip [0 .. length counters `minus` 1] counters
                        let nonZero = filter (\(_, v) => v > 0) indexed
                        putStrLn $ "  Non-zero: " ++ show (length nonZero)

                        if null lbls
                          then do
                            -- No labels, just print indices
                            for_ nonZero $ \(idx, val) =>
                              putStrLn $ "    [" ++ show idx ++ "] = " ++ show val
                          else do
                            -- Build coverage result
                            let coverageResult = buildCoverageResult lbls indexed
                            -- Show summary
                            putStrLn $ "\n  === Coverage Summary ==="
                            putStrLn $ "  Total functions: " ++ show coverageResult.totalFunctions
                            putStrLn $ "  Hit functions: " ++ show coverageResult.hitFunctions
                            putStrLn $ "  Coverage: " ++ show coverageResult.coveragePercent ++ "%"
                            -- Show function names
                            putStrLn "\n  === Coverage Report ==="
                            let nonZeroHits = filter (\h => h.executedCount > 0) coverageResult.hits
                            for_ nonZeroHits $ \h =>
                              putStrLn $ "    " ++ h.funcName ++ " : " ++ show h.executedCount
                            -- Output JSON if requested
                            case opts.coverageJson of
                              Nothing => pure ()
                              Just jsonPath => do
                                let jsonContent = coverageToJson coverageResult
                                Right _ <- writeFile jsonPath jsonContent
                                  | Left err => putStrLn $ "Error writing JSON: " ++ show err
                                putStrLn $ "\n  Coverage JSON written to: " ++ jsonPath
                      else do
                        putStrLn $ "Log #" ++ show i ++ ":"
                        putStrLn $ "  Topics: " ++ show (length entry.logTopics)
                        let printTopic : Nat -> Word256 -> IO ()
                            printTopic j topic = putStrLn $ "    [" ++ show j ++ "] 0x" ++ showHex256 topic
                        ignore $ foldlM (\j, t => printTopic j t >> pure (S j)) 0 entry.logTopics
                        putStrLn $ "  Data: " ++ show (length entry.logData) ++ " bytes"
                        when (not $ null entry.logData) $
                          putStrLn $ "    " ++ toHex entry.logData
              ignore $ foldlM (\i, e => printLog labels i e >> pure (S i)) 0 logs

            -- Also show result summary
            case result of
              Success _ gasUsed _ => putStrLn $ "Result: SUCCESS (gas used: " ++ show (opts.gasLimit `minus` gasUsed) ++ ")"
              Revert _ gasUsed => putStrLn $ "Result: REVERT (gas used: " ++ show (opts.gasLimit `minus` gasUsed) ++ ")"
              OutOfGas => putStrLn "Result: OUT OF GAS"
              InvalidJump pc => putStrLn $ "Result: INVALID JUMP to PC=" ++ show pc
              StackError msg => putStrLn $ "Result: STACK ERROR - " ++ msg
              InvalidOpcode op => putStrLn $ "Result: INVALID OPCODE 0x" ++ show op
        else do
          let calldata = fromMaybe [] (fromHex opts.calldataHex)
          let mCallValue = opts.callValueHex >>= hexToWord256
          when opts.verbose $ do
            putStrLn "=== Execution ==="

          let result = executeWithStorage code calldata opts.gasLimit initialStore mCallValue

          case result of
            Success retData gasUsed store => do
              putStrLn $ "Result: SUCCESS"
              putStrLn $ "Gas used: " ++ show (opts.gasLimit `minus` gasUsed)
              when (not $ null retData) $ do
                putStrLn $ "Return data: " ++ toHex retData
              let slots = Storage.toList store
              when (not $ null slots) $ do
                putStrLn "Storage:"
                for_ slots $ \(k, v) =>
                  putStrLn $ "  " ++ show k ++ " => " ++ show v
              -- Save storage if specified
              case opts.saveStorage of
                Nothing => pure ()
                Just path => do
                  saveResult <- saveStorageFile path store
                  case saveResult of
                    Left err => putStrLn $ "Warning: " ++ err
                    Right () => when opts.verbose $
                      putStrLn $ "Storage saved to: " ++ path

            Revert retData gasUsed => do
              putStrLn $ "Result: REVERT"
              putStrLn $ "Gas used: " ++ show (opts.gasLimit `minus` gasUsed)
              when (not $ null retData) $ do
                putStrLn $ "Revert data: " ++ toHex retData

            OutOfGas => do
              putStrLn "Result: OUT OF GAS"

            InvalidJump pc => do
              putStrLn $ "Result: INVALID JUMP to PC=" ++ show pc

            StackError msg => do
              putStrLn $ "Result: STACK ERROR - " ++ msg

            InvalidOpcode op => do
              putStrLn $ "Result: INVALID OPCODE 0x" ++ show op

-- =============================================================================
-- Multi-Contract Execution
-- =============================================================================

loadContract : (String, String, Maybe String) -> IO (Either String (Word256, Account))
loadContract (addrStr, codeFile, storageFile) = do
  case hexToWord256 addrStr of
    Nothing => pure (Left $ "Invalid address: " ++ addrStr)
    Just addr => do
      Right codeContent <- readFile codeFile
        | Left err => pure (Left $ "Failed to read " ++ codeFile ++ ": " ++ show err)
      case fromHex (trim codeContent) of
        Nothing => pure (Left $ "Invalid bytecode in " ++ codeFile)
        Just code => do
          storage <- case storageFile of
            Nothing => pure Storage.empty
            Just sf => do
              Right sContent <- readFile sf
                | Left _ => pure Storage.empty
              pure $ fromMaybe Storage.empty (Storage.fromJSON (trim sContent))
          pure (Right (addr, MkAccount code storage Word256.zero 0))

loadWorldState : List (String, String, Maybe String) -> IO (Either String WorldState)
loadWorldState contracts = go contracts WorldState.empty
  where
    go : List (String, String, Maybe String) -> WorldState -> IO (Either String WorldState)
    go [] world = pure (Right world)
    go (c :: cs) world = do
      result <- loadContract c
      case result of
        Left err => pure (Left err)
        Right (addr, acc) => go cs (setAccount addr acc world)

showMultiResult : MultiResult -> Nat -> IO ()
showMultiResult result gasLimit = case result of
  MSuccess retData gasUsed world => do
    putStrLn "Result: SUCCESS"
    putStrLn $ "Gas used: " ++ show (gasLimit `minus` gasUsed)
    when (not $ null retData) $ do
      putStrLn $ "Return data: " ++ toHex retData
    putStrLn "World state:"
    let addrs = WorldState.addresses world
    for_ addrs $ \addr => do
      let acc = getAccount addr world
      let slots = Storage.toList acc.storage
      putStrLn $ "  " ++ show addr ++ ":"
      when (not $ null slots) $ do
        for_ slots $ \(k, v) =>
          putStrLn $ "    [" ++ show k ++ "] = " ++ show v
  MRevert retData gasUsed _ => do
    putStrLn "Result: REVERT"
    putStrLn $ "Gas used: " ++ show (gasLimit `minus` gasUsed)
    when (not $ null retData) $ do
      putStrLn $ "Revert data: " ++ toHex retData
  MOutOfGas _ => putStrLn "Result: OUT OF GAS"
  MInvalidJump pc _ => putStrLn $ "Result: INVALID JUMP to PC=" ++ show pc
  MStackError msg _ => putStrLn $ "Result: STACK ERROR - " ++ msg
  MInvalidOpcode op _ => putStrLn $ "Result: INVALID OPCODE 0x" ++ show op
  MCallDepthExceeded _ => putStrLn "Result: CALL DEPTH EXCEEDED"
  MStaticViolation _ => putStrLn "Result: STATIC CALL VIOLATION (state modification attempted)"

runMultiContract : Options -> IO ()
runMultiContract opts = do
  -- Load world state from contracts or file
  worldResult <- case opts.loadWorld of
    Just path => do
      Right content <- readFile path
        | Left err => pure (Left $ "Failed to read " ++ path ++ ": " ++ show err)
      case WorldState.fromSimpleFormat (trim content) of
        Just w => pure (Right w)
        Nothing => pure (Left "Invalid world state format")
    Nothing => loadWorldState opts.contracts

  case worldResult of
    Left err => do
      putStrLn $ "Error: " ++ err
      exitWith (ExitFailure 1)
    Right world => do
      case opts.callAddress of
        Nothing => do
          putStrLn "Error: --call address required for multi-contract mode"
          exitWith (ExitFailure 1)
        Just addrStr => do
          case hexToWord256 addrStr of
            Nothing => do
              putStrLn $ "Error: Invalid call address: " ++ addrStr
              exitWith (ExitFailure 1)
            Just addr => do
              let calldata = fromMaybe [] (fromHex opts.calldataHex)
              let caller = fromInteger 0x9999  -- Default external caller

              when opts.verbose $ do
                putStrLn "=== Multi-Contract Execution ==="
                putStrLn $ "Target: " ++ show addr
                putStrLn $ "Calldata: " ++ opts.calldataHex
                putStrLn $ "Gas limit: " ++ show opts.gasLimit
                putStrLn ""

              -- Create initial context and VM
              let ctx = MkCallContext
                    addr                         -- address
                    addr                         -- storageAddress
                    caller                       -- caller
                    caller                       -- origin
                    Word256.zero                 -- callValue
                    calldata                     -- callData
                    (Word256.fromInteger 1)      -- gasPrice
                    (Word256.fromInteger 1000000) -- blockNumber
                    (Word256.fromInteger 1700000000) -- timestamp
                    (Word256.fromInteger 1)      -- chainId
                    (Word256.fromInteger 30000000) -- gasLimit
                    False                        -- isStatic
                    0                            -- callDepth
              let code = getCode addr world
              let mvm = initMultiVM code ctx opts.gasLimit

              let result = runMulti opts.gasLimit mvm world
              showMultiResult result opts.gasLimit

              -- Save world state if requested
              case (result, opts.saveWorld) of
                (MSuccess _ _ newWorld, Just path) => do
                  Right _ <- writeFile path (WorldState.toSimpleFormat newWorld)
                    | Left err => putStrLn $ "Warning: Failed to save world: " ++ show err
                  when opts.verbose $ putStrLn $ "World state saved to: " ++ path
                _ => pure ()

-- =============================================================================
-- Entry Point
-- =============================================================================

isMultiContractMode : Options -> Bool
isMultiContractMode opts =
  not (null opts.contracts) || isJust opts.loadWorld || isJust opts.callAddress

main : IO ()
main = do
  args <- getArgs
  let opts = parseArgs (drop 1 args) defaultOptions

  if opts.showHelp
    then putStrLn helpText
    else if opts.showVersion
      then putStrLn versionText
      else if isMultiContractMode opts
        then runMultiContract opts
        else runMain opts
