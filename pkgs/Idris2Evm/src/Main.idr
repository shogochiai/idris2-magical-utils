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
import EVM.Storage.Model as Storage
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
  -- Multi-contract options
  contracts : List (String, String, Maybe String)  -- (address, bytecode file, optional storage file)
  callAddress : Maybe String                        -- Address to call in multi-contract mode
  loadWorld : Maybe String                          -- Load world state from JSON
  saveWorld : Maybe String                          -- Save world state to JSON

defaultOptions : Options
defaultOptions = MkOptions Nothing Nothing "" Nothing 1000000 False False False False False Nothing Nothing Nothing [] Nothing Nothing Nothing

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

            -- Output captured logs (raw data only - use idris2-evm-coverage for analysis)
            when (not $ null logs) $ do
              putStrLn $ "\n=== Logs (" ++ show (length logs) ++ " events) ==="
              let printLog : Nat -> LogEntry -> IO ()
                  printLog i entry = do
                    putStrLn $ "Log #" ++ show i ++ ":"
                    putStrLn $ "  Topics: " ++ show (length entry.logTopics)
                    let printTopic : Nat -> Word256 -> IO ()
                        printTopic j topic = putStrLn $ "    [" ++ show j ++ "] 0x" ++ showHex256 topic
                    ignore $ foldlM (\j, t => printTopic j t >> pure (S j)) 0 entry.logTopics
                    putStrLn $ "  Data: " ++ show (length entry.logData) ++ " bytes"
                    when (not $ null entry.logData) $
                      putStrLn $ "    " ++ toHex entry.logData
              ignore $ foldlM (\i, e => printLog i e >> pure (S i)) 0 logs

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
