||| EVM.Interpreter - Pure Idris2 EVM Interpreter
module EVM.Interpreter

import EVM.Word256
import EVM.Stack
import EVM.Memory
import EVM.Storage
import EVM.Opcodes
import EVM.Bytecode
import Data.List
import Data.Maybe

%default covering

public export
record Context where
  constructor MkContext
  address : Word256
  caller : Word256
  origin : Word256
  callValue : Word256
  callData : List Bits8
  gasPrice : Word256
  blockNumber : Word256
  timestamp : Word256
  chainId : Word256
  gasLimit : Word256

public export
defaultContext : Context
defaultContext = MkContext
  (Word256.fromInteger 0x1234567890ABCDEF)
  (Word256.fromInteger 0xCAFEBABE)
  (Word256.fromInteger 0xCAFEBABE)
  Word256.zero [] (Word256.fromInteger 1) (Word256.fromInteger 1000000)
  (Word256.fromInteger 1700000000) (Word256.fromInteger 1) (Word256.fromInteger 30000000)

||| Log entry: topics + data
public export
record LogEntry where
  constructor MkLogEntry
  logTopics : List Word256
  logData : List Bits8

public export
record VM where
  constructor MkVM
  pc : Nat
  stack : Stack
  memory : Memory
  storage : Storage
  code : Bytecode
  gas : Nat
  context : Context
  returnData : List Bits8
  stopped : Bool
  reverted : Bool
  logs : List LogEntry

public export
initVM : Bytecode -> Context -> Nat -> VM
initVM code ctx gas = MkVM 0 Stack.empty Memory.empty Storage.empty code gas ctx [] False False []

||| Initialize VM with pre-loaded storage
public export
initVMWithStorage : Bytecode -> Context -> Nat -> Storage -> VM
initVMWithStorage code ctx gas store = MkVM 0 Stack.empty Memory.empty store code gas ctx [] False False []

public export
data Result : Type where
  Success : List Bits8 -> Nat -> Storage -> Result
  Revert : List Bits8 -> Nat -> Result
  OutOfGas : Result
  InvalidJump : Nat -> Result
  StackError : String -> Result
  InvalidOpcode : Bits8 -> Result

public export
Show Result where
  show (Success _ gas _) = "Success(gas=" ++ show gas ++ ")"
  show (Revert _ gas) = "Revert(gas=" ++ show gas ++ ")"
  show OutOfGas = "OutOfGas"
  show (InvalidJump pc) = "InvalidJump(" ++ show pc ++ ")"
  show (StackError msg) = "StackError(" ++ msg ++ ")"
  show (InvalidOpcode op) = "InvalidOpcode"

-- Helpers
readPushData : Nat -> Nat -> Bytecode -> Word256
readPushData pc size code =
  let bytes = Bytecode.readBytes (pc + 1) size code
      val = foldl (\acc, b => acc * 256 + cast b) 0 bytes
  in Word256.fromInteger val

pushVal : Word256 -> VM -> Either Result VM
pushVal val vm = case push val vm.stack of
  Ok stack' => Right ({ stack := stack', pc $= (+1) } vm)
  _ => Left (StackError "push overflow")

unaryOp : (Word256 -> Word256) -> VM -> Either Result VM
unaryOp f vm = case pop vm.stack of
  Ok (a, stack') => case push (f a) stack' of
    Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
    _ => Left (StackError "unary push failed")
  _ => Left (StackError "unary pop failed")

binaryOp : (Word256 -> Word256 -> Word256) -> VM -> Either Result VM
binaryOp f vm = case popN 2 vm.stack of
  Ok ([a, b], stack') => case push (f a b) stack' of
    Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
    _ => Left (StackError "binary push failed")
  _ => Left (StackError "binary pop failed")

ternaryOp : (Word256 -> Word256 -> Word256 -> Word256) -> VM -> Either Result VM
ternaryOp f vm = case popN 3 vm.stack of
  Ok ([a, b, c], stack') => case push (f a b c) stack' of
    Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
    _ => Left (StackError "ternary push failed")
  _ => Left (StackError "ternary pop failed")

dupOp : Nat -> VM -> Either Result VM
dupOp n vm = case dup n vm.stack of
  Ok stack' => Right ({ stack := stack', pc $= (+1) } vm)
  _ => Left (StackError "DUP failed")

swapOp : Nat -> VM -> Either Result VM
swapOp n vm = case swap n vm.stack of
  Ok stack' => Right ({ stack := stack', pc $= (+1) } vm)
  _ => Left (StackError "SWAP failed")

logOp : Nat -> VM -> Either Result VM
logOp numTopics vm = case popN (2 + numTopics) vm.stack of
  Ok (args, stack') =>
    -- args = [offset, size, topic0, topic1, ...]
    case args of
      (offset :: size :: topics) =>
        let off = cast {to=Integer} (toInteger offset)
            sz = cast {to=Nat} (toInteger size)
            logDataBytes = Memory.readBytes off sz vm.memory
            entry = MkLogEntry topics logDataBytes
        in Right ({ stack := stack', pc $= (+1), logs $= (entry ::) } vm)
      _ => Left (StackError "LOG failed - invalid args")
  _ => Left (StackError "LOG failed")

toByteList : Memory -> List Bits8
toByteList mem = map (\i => readByte i mem) [0 .. cast (memorySize mem * 32)]

returnOp : VM -> Either Result VM
returnOp vm = case popN 2 vm.stack of
  Ok ([offset, size], _) =>
    let retData = Bytecode.readBytes (cast $ toInteger offset) (cast $ toInteger size) (toByteList vm.memory)
    in Right ({ stopped := True, returnData := retData } vm)
  _ => Left (StackError "RETURN failed")

revertOp : VM -> Either Result VM
revertOp vm = case popN 2 vm.stack of
  Ok ([offset, size], _) =>
    let retData = Bytecode.readBytes (cast $ toInteger offset) (cast $ toInteger size) (toByteList vm.memory)
    in Right ({ stopped := True, reverted := True, returnData := retData } vm)
  _ => Left (StackError "REVERT failed")

-- | Dummy address for CREATE/CREATE2 stubs (non-zero to allow execution to continue)
stubAddress : Word256
stubAddress = Word256.fromInteger 0xDEAD0001

createStub : VM -> Either Result VM
createStub vm = case popN 3 vm.stack of
  Ok (_, stack') => case push stubAddress stack' of
    Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
    _ => Left (StackError "CREATE push failed")
  _ => Left (StackError "CREATE failed")

-- | CREATE2 takes 4 arguments: value, offset, size, salt
create2Stub : VM -> Either Result VM
create2Stub vm = case popN 4 vm.stack of
  Ok (_, stack') => case push stubAddress stack' of
    Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
    _ => Left (StackError "CREATE2 push failed")
  _ => Left (StackError "CREATE2 failed")

callStub : VM -> Either Result VM
callStub vm = case popN 7 vm.stack of
  Ok (_, stack') => case push Word256.one stack' of
    Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
    _ => Left (StackError "CALL push failed")
  _ => case popN 6 vm.stack of
    Ok (_, stack') => case push Word256.one stack' of
      Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
      _ => Left (StackError "CALL push failed")
    _ => Left (StackError "CALL failed")

mutual
  executeDupSwap : Opcode -> VM -> Either Result VM
  executeDupSwap DUP1 vm = dupOp 0 vm
  executeDupSwap DUP2 vm = dupOp 1 vm
  executeDupSwap DUP3 vm = dupOp 2 vm
  executeDupSwap DUP4 vm = dupOp 3 vm
  executeDupSwap DUP5 vm = dupOp 4 vm
  executeDupSwap DUP6 vm = dupOp 5 vm
  executeDupSwap DUP7 vm = dupOp 6 vm
  executeDupSwap DUP8 vm = dupOp 7 vm
  executeDupSwap DUP9 vm = dupOp 8 vm
  executeDupSwap DUP10 vm = dupOp 9 vm
  executeDupSwap DUP11 vm = dupOp 10 vm
  executeDupSwap DUP12 vm = dupOp 11 vm
  executeDupSwap DUP13 vm = dupOp 12 vm
  executeDupSwap DUP14 vm = dupOp 13 vm
  executeDupSwap DUP15 vm = dupOp 14 vm
  executeDupSwap DUP16 vm = dupOp 15 vm
  executeDupSwap SWAP1 vm = swapOp 1 vm
  executeDupSwap SWAP2 vm = swapOp 2 vm
  executeDupSwap SWAP3 vm = swapOp 3 vm
  executeDupSwap SWAP4 vm = swapOp 4 vm
  executeDupSwap SWAP5 vm = swapOp 5 vm
  executeDupSwap SWAP6 vm = swapOp 6 vm
  executeDupSwap SWAP7 vm = swapOp 7 vm
  executeDupSwap SWAP8 vm = swapOp 8 vm
  executeDupSwap SWAP9 vm = swapOp 9 vm
  executeDupSwap SWAP10 vm = swapOp 10 vm
  executeDupSwap SWAP11 vm = swapOp 11 vm
  executeDupSwap SWAP12 vm = swapOp 12 vm
  executeDupSwap SWAP13 vm = swapOp 13 vm
  executeDupSwap SWAP14 vm = swapOp 14 vm
  executeDupSwap SWAP15 vm = swapOp 15 vm
  executeDupSwap SWAP16 vm = swapOp 16 vm
  executeDupSwap LOG0 vm = logOp 0 vm
  executeDupSwap LOG1 vm = logOp 1 vm
  executeDupSwap LOG2 vm = logOp 2 vm
  executeDupSwap LOG3 vm = logOp 3 vm
  executeDupSwap LOG4 vm = logOp 4 vm
  executeDupSwap RETURN vm = returnOp vm
  executeDupSwap REVERT vm = revertOp vm
  executeDupSwap INVALID vm = Left (InvalidOpcode 0xfe)
  executeDupSwap SELFDESTRUCT vm = Right ({ stopped := True } vm)
  executeDupSwap CREATE vm = createStub vm
  executeDupSwap CREATE2 vm = create2Stub vm
  executeDupSwap CALL vm = callStub vm
  executeDupSwap CALLCODE vm = callStub vm
  executeDupSwap DELEGATECALL vm = callStub vm
  executeDupSwap STATICCALL vm = callStub vm
  executeDupSwap (UNKNOWN b) vm = Left (InvalidOpcode b)
  executeDupSwap _ vm = Right ({ pc $= (+1) } vm)

  executeOp : Opcode -> VM -> Either Result VM
  executeOp STOP vm = Right ({ stopped := True } vm)
  executeOp ADD vm = binaryOp Word256.add vm
  executeOp MUL vm = binaryOp Word256.mul vm
  executeOp SUB vm = binaryOp Word256.sub vm
  executeOp DIV vm = binaryOp Word256.div vm
  executeOp SDIV vm = binaryOp Word256.sdiv vm
  executeOp MOD vm = binaryOp Word256.mod vm
  executeOp SMOD vm = binaryOp Word256.mod vm
  executeOp LT vm = binaryOp Word256.lt vm
  executeOp GT vm = binaryOp Word256.gt vm
  executeOp SLT vm = binaryOp Word256.lt vm
  executeOp SGT vm = binaryOp Word256.gt vm
  executeOp EQ vm = binaryOp Word256.eq vm
  executeOp ISZERO vm = unaryOp Word256.iszero vm
  executeOp AND vm = binaryOp Word256.and vm
  executeOp OR vm = binaryOp Word256.or vm
  executeOp XOR vm = binaryOp Word256.xor vm
  executeOp NOT vm = unaryOp Word256.not vm
  executeOp BYTE vm = binaryOp Word256.byte vm
  executeOp SHL vm = binaryOp Word256.shl vm
  executeOp SHR vm = binaryOp Word256.shr vm
  executeOp SAR vm = binaryOp Word256.shr vm
  executeOp ADDRESS vm = pushVal vm.context.address vm
  executeOp ORIGIN vm = pushVal vm.context.origin vm
  executeOp CALLER vm = pushVal vm.context.caller vm
  executeOp CALLVALUE vm = pushVal vm.context.callValue vm
  executeOp CALLDATASIZE vm = pushVal (Word256.fromInteger $ cast $ length vm.context.callData) vm
  executeOp CALLDATALOAD vm = case pop vm.stack of
    Ok (offset, stack') =>
      let off = cast {to=Nat} (toInteger offset)
          bytes = take 32 (drop off vm.context.callData ++ replicate 32 0)
          val = foldl (\acc, b => acc * 256 + cast b) 0 bytes
      in case push (Word256.fromInteger val) stack' of
        Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
        _ => Left (StackError "CALLDATALOAD push failed")
    _ => Left (StackError "CALLDATALOAD failed")
  executeOp CALLDATACOPY vm = case popN 3 vm.stack of
    Ok ([destOffset, srcOffset, size], stack') =>
      let destOff = toInteger destOffset
          srcOff = cast {to=Nat} (toInteger srcOffset)
          sz = cast {to=Nat} (toInteger size)
          calldataBytes = drop srcOff vm.context.callData ++ replicate sz 0
          bytesToCopy = take sz calldataBytes
          mem' = writeBytes destOff bytesToCopy vm.memory
      in Right ({ stack := stack', memory := mem', pc $= (+1) } vm)
    _ => Left (StackError "CALLDATACOPY failed")
  executeOp CODESIZE vm = pushVal (Word256.fromInteger $ cast $ codeSize vm.code) vm
  executeOp CODECOPY vm = case popN 3 vm.stack of
    Ok ([destOffset, srcOffset, size], stack') =>
      let destOff = toInteger destOffset
          srcOff = cast {to=Nat} (toInteger srcOffset)
          sz = cast {to=Nat} (toInteger size)
          srcBytes = drop srcOff vm.code ++ replicate sz 0
          bytesToCopy = take sz srcBytes
          mem' = writeBytes destOff bytesToCopy vm.memory
      in Right ({ stack := stack', memory := mem', pc $= (+1) } vm)
    _ => Left (StackError "CODECOPY failed")
  executeOp GASPRICE vm = pushVal vm.context.gasPrice vm
  executeOp TIMESTAMP vm = pushVal vm.context.timestamp vm
  executeOp NUMBER vm = pushVal vm.context.blockNumber vm
  executeOp GASLIMIT vm = pushVal vm.context.gasLimit vm
  executeOp CHAINID vm = pushVal vm.context.chainId vm
  executeOp SELFBALANCE vm = pushVal (Word256.fromInteger 1000000000000000000) vm
  executeOp BASEFEE vm = pushVal Word256.one vm
  executeOp COINBASE vm = pushVal Word256.zero vm
  executeOp PREVRANDAO vm = pushVal Word256.zero vm
  executeOp POP vm = case pop vm.stack of
    Ok (_, stack') => Right ({ stack := stack', pc $= (+1) } vm)
    _ => Left (StackError "POP underflow")
  executeOp MLOAD vm = case pop vm.stack of
    Ok (offset, stack') =>
      let val = mload offset vm.memory
      in case push val stack' of
        Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
        _ => Left (StackError "MLOAD push failed")
    _ => Left (StackError "MLOAD failed")
  executeOp MSTORE vm = case popN 2 vm.stack of
    Ok ([offset, val], stack') =>
      let mem' = mstore offset val vm.memory
      in Right ({ stack := stack', memory := mem', pc $= (+1) } vm)
    _ => Left (StackError "MSTORE failed")
  executeOp MSTORE8 vm = case popN 2 vm.stack of
    Ok ([offset, val], stack') =>
      let b = cast {to=Bits8} (toInteger val `mod` 256)
          mem' = writeByte (cast $ toInteger offset) b vm.memory
      in Right ({ stack := stack', memory := mem', pc $= (+1) } vm)
    _ => Left (StackError "MSTORE8 failed")
  -- KECCAK256: Read memory[offset..offset+size] and hash it
  -- Using a simple hash for testing (not cryptographically secure)
  executeOp KECCAK256 vm = case popN 2 vm.stack of
    Ok ([offset, size], stack') =>
      let bytes = Bytecode.readBytes (cast $ toInteger offset) (cast $ toInteger size) (toByteList vm.memory)
          -- Simple hash: sum bytes with position mixing, then apply some mixing
          simpleHash : Integer
          simpleHash = foldl (\acc, (i, b) => (acc * 31 + b + i * 17) `mod` 0x10000000000000000) 0 (zip (the (List Integer) [0..cast (length bytes)]) (map (cast {to=Integer}) bytes))
          -- Mix further to get 256-bit looking result
          h1 : Integer
          h1 = simpleHash `mod` 0x100000000
          h2 : Integer
          h2 = (simpleHash * 7 + 13) `mod` 0x100000000
          h3 : Integer
          h3 = (simpleHash * 11 + 23) `mod` 0x100000000
          h4 : Integer
          h4 = (simpleHash * 17 + 37) `mod` 0x100000000
          hash256 : Integer
          hash256 = h1 * 0x1000000000000000000000000 + h2 * 0x10000000000000000 + h3 * 0x100000000 + h4
          hashWord = Word256.fromInteger hash256
      in case push hashWord stack' of
        Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
        _ => Left (StackError "KECCAK256 push failed")
    _ => Left (StackError "KECCAK256 failed")
  executeOp SLOAD vm = case pop vm.stack of
    Ok (key, stack') =>
      let val = sload key vm.storage
      in case push val stack' of
        Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
        _ => Left (StackError "SLOAD push failed")
    _ => Left (StackError "SLOAD failed")
  executeOp SSTORE vm = case popN 2 vm.stack of
    Ok ([key, val], stack') =>
      let store' = sstore key val vm.storage
      in Right ({ stack := stack', storage := store', pc $= (+1) } vm)
    _ => Left (StackError "SSTORE failed")
  executeOp JUMP vm = case pop vm.stack of
    Ok (dest, stack') =>
      let destNat = cast {to=Nat} (toInteger dest)
      in if isValidJumpDest destNat vm.code
        then Right ({ stack := stack', pc := destNat } vm)
        else Left (InvalidJump destNat)
    _ => Left (StackError "JUMP failed")
  executeOp JUMPI vm = case popN 2 vm.stack of
    Ok ([dest, cond], stack') =>
      if cond /= Word256.zero
        then let destNat = cast {to=Nat} (toInteger dest)
             in if isValidJumpDest destNat vm.code
               then Right ({ stack := stack', pc := destNat } vm)
               else Left (InvalidJump destNat)
        else Right ({ stack := stack', pc $= (+1) } vm)
    _ => Left (StackError "JUMPI failed")
  executeOp PC vm = pushVal (Word256.fromInteger $ cast vm.pc) vm
  executeOp MSIZE vm = pushVal (Word256.fromInteger $ cast $ memorySize vm.memory * 32) vm
  executeOp GAS vm = pushVal (Word256.fromInteger $ cast vm.gas) vm
  executeOp JUMPDEST vm = Right ({ pc $= (+1) } vm)
  executeOp PUSH0 vm = case push Word256.zero vm.stack of
    Ok stack' => Right ({ stack := stack', pc $= (+1) } vm)
    _ => Left (StackError "PUSH0 overflow")
  executeOp op vm =
    if isPush op
      then let size = pushSize op
               val = readPushData vm.pc size vm.code
           in case push val vm.stack of
             Ok stack' => Right ({ stack := stack', pc $= (+ (1 + size)) } vm)
             _ => Left (StackError "PUSH overflow")
      else executeDupSwap op vm

export
step : VM -> Either Result VM
step vm =
  if vm.stopped then Left (Success vm.returnData vm.gas vm.storage)
  else if vm.reverted then Left (Revert vm.returnData vm.gas)
  else if vm.pc >= codeSize vm.code then Left (Success [] vm.gas vm.storage)
  else let opByte = Bytecode.readAt vm.pc vm.code
           op = fromByte opByte
       in executeOp op vm

export
run : Nat -> VM -> Result
run 0 vm = OutOfGas
run (S n) vm = case step vm of
  Left result => result
  Right vm' => run n vm'

export
execute : Bytecode -> List Bits8 -> Nat -> Result
execute code calldata gasLimit =
  let ctx = { callData := calldata } defaultContext
      vm = initVM code ctx gasLimit
  in run gasLimit vm

export
executeHex : String -> String -> Nat -> Either String Result
executeHex codeHex calldataHex gasLimit =
  case (Bytecode.fromHex codeHex, Bytecode.fromHex calldataHex) of
    (Just code, Just calldata) => Right $ execute code calldata gasLimit
    (Nothing, _) => Left "Invalid bytecode hex"
    (_, Nothing) => Left "Invalid calldata hex"

-- =============================================================================
-- Trace Execution (for coverage analysis)
-- =============================================================================

||| Trace entry for a single step
public export
record TraceEntry where
  constructor MkTraceEntry
  stepNum : Nat
  pc : Nat
  opcode : Bits8
  opcodeName : String
  stackDepth : Nat

export
Show TraceEntry where
  show e = show e.stepNum ++ "," ++ show e.pc ++ "," ++
           show e.opcode ++ "," ++ e.opcodeName ++ "," ++ show e.stackDepth

||| Run with trace collection (pure, returns trace list and logs)
export
runWithTrace : Nat -> VM -> (Result, List TraceEntry, List LogEntry)
runWithTrace fuel vm = go fuel vm 0 []
  where
    go : Nat -> VM -> Nat -> List TraceEntry -> (Result, List TraceEntry, List LogEntry)
    go 0 vm _ acc = (OutOfGas, reverse acc, reverse vm.logs)
    go (S n) vm stepN acc =
      if vm.stopped then (Success vm.returnData vm.gas vm.storage, reverse acc, reverse vm.logs)
      else if vm.reverted then (Revert vm.returnData vm.gas, reverse acc, reverse vm.logs)
      else if vm.pc >= codeSize vm.code then (Success [] vm.gas vm.storage, reverse acc, reverse vm.logs)
      else
        let opByte = Bytecode.readAt vm.pc vm.code
            op = fromByte opByte
            entry = MkTraceEntry stepN vm.pc opByte (show op) (depth vm.stack)
        in case executeOp op vm of
             Left result => (result, reverse (entry :: acc), reverse vm.logs)
             Right vm' => go n vm' (S stepN) (entry :: acc)

||| Execute bytecode and return trace and logs
export
executeWithTrace : Bytecode -> List Bits8 -> Nat -> Storage -> Maybe Word256 -> (Result, List TraceEntry, List LogEntry)
executeWithTrace code calldata gasLimit initialStore mCallValue =
  let callVal = fromMaybe Word256.zero mCallValue
      ctx = { callData := calldata, callValue := callVal } defaultContext
      vm = initVMWithStorage code ctx gasLimit initialStore
  in runWithTrace gasLimit vm
