||| EVM.MultiInterpreter - Multi-contract EVM Interpreter
|||
||| Extends the basic interpreter with CALL, DELEGATECALL, STATICCALL support.
||| Manages a WorldState with multiple contract accounts.
module EVM.MultiInterpreter

import EVM.Word256
import EVM.Stack
import EVM.Memory
import EVM.Storage
import EVM.Opcodes
import EVM.Bytecode
import EVM.WorldState
import Data.List

%default covering

-- =============================================================================
-- Execution Context
-- =============================================================================

||| Execution context for a single call frame
public export
record CallContext where
  constructor MkCallContext
  address : Word256       -- Current executing contract address
  storageAddress : Word256 -- Address whose storage to use
  caller : Word256        -- msg.sender
  origin : Word256        -- tx.origin
  callValue : Word256     -- msg.value
  callData : List Bits8   -- Input data
  gasPrice : Word256
  blockNumber : Word256
  timestamp : Word256
  chainId : Word256
  gasLimit : Word256
  isStatic : Bool         -- True during STATICCALL
  callDepth : Nat         -- Current call depth

||| Default context for testing
public export
defaultCallContext : CallContext
defaultCallContext = MkCallContext
  (Word256.fromInteger 0x1000)
  (Word256.fromInteger 0x1000)
  (Word256.fromInteger 0xCAFEBABE)
  (Word256.fromInteger 0xCAFEBABE)
  Word256.zero [] (Word256.fromInteger 1) (Word256.fromInteger 1000000)
  (Word256.fromInteger 1700000000) (Word256.fromInteger 1) (Word256.fromInteger 30000000)
  False 0

-- =============================================================================
-- Multi-Contract VM State
-- =============================================================================

||| VM state for multi-contract execution
public export
record MultiVM where
  constructor MkMultiVM
  pc : Nat
  stack : Stack
  memory : Memory
  code : Bytecode
  gas : Nat
  context : CallContext
  returnData : List Bits8
  stopped : Bool
  reverted : Bool

||| Initialize VM for a call
public export
initMultiVM : Bytecode -> CallContext -> Nat -> MultiVM
initMultiVM code ctx gas = MkMultiVM 0 Stack.empty Memory.empty code gas ctx [] False False

-- =============================================================================
-- Execution Result
-- =============================================================================

||| Result of multi-contract execution
public export
data MultiResult : Type where
  MSuccess : List Bits8 -> Nat -> WorldState -> MultiResult
  MRevert : List Bits8 -> Nat -> WorldState -> MultiResult
  MOutOfGas : WorldState -> MultiResult
  MInvalidJump : Nat -> WorldState -> MultiResult
  MStackError : String -> WorldState -> MultiResult
  MInvalidOpcode : Bits8 -> WorldState -> MultiResult
  MStaticViolation : WorldState -> MultiResult
  MCallDepthExceeded : WorldState -> MultiResult

public export
Show MultiResult where
  show (MSuccess _ gas _) = "Success(gas=" ++ show gas ++ ")"
  show (MRevert _ gas _) = "Revert(gas=" ++ show gas ++ ")"
  show (MOutOfGas _) = "OutOfGas"
  show (MInvalidJump pc _) = "InvalidJump(" ++ show pc ++ ")"
  show (MStackError msg _) = "StackError(" ++ msg ++ ")"
  show (MInvalidOpcode op _) = "InvalidOpcode"
  show (MStaticViolation _) = "StaticViolation"
  show (MCallDepthExceeded _) = "CallDepthExceeded"

-- =============================================================================
-- Helper Functions
-- =============================================================================

readPushData : Nat -> Nat -> Bytecode -> Word256
readPushData pc size code =
  let bytes = Bytecode.readBytes (pc + 1) size code
      val = foldl (\acc, b => acc * 256 + cast b) 0 bytes
  in Word256.fromInteger val

toByteList : Memory -> List Bits8
toByteList mem = map (\i => readByte i mem) [0 .. cast (memorySize mem * 32)]

MAX_CALL_DEPTH : Nat
MAX_CALL_DEPTH = 1024

-- =============================================================================
-- Step Result (internal)
-- =============================================================================

data StepResult : Type where
  Continue : MultiVM -> WorldState -> StepResult
  Done : MultiResult -> StepResult

-- =============================================================================
-- SLOAD/SSTORE with WorldState
-- =============================================================================

executeSload : MultiVM -> WorldState -> StepResult
executeSload vm world = case pop vm.stack of
  Ok (key, stack') =>
    let val = getStorageAt vm.context.storageAddress key world
    in case push val stack' of
      Ok stack'' => Continue ({ stack := stack'', pc $= (+1) } vm) world
      _ => Done (MStackError "SLOAD push failed" world)
  _ => Done (MStackError "SLOAD failed" world)

executeSstore : MultiVM -> WorldState -> StepResult
executeSstore vm world =
  if vm.context.isStatic
    then Done (MStaticViolation world)
    else case popN 2 vm.stack of
      Ok ([key, val], stack') =>
        let world' = setStorageAt vm.context.storageAddress key val world
        in Continue ({ stack := stack', pc $= (+1) } vm) world'
      _ => Done (MStackError "SSTORE failed" world)

-- =============================================================================
-- Simple Opcode Execution (no world state changes)
-- =============================================================================

pushValSimple : Word256 -> MultiVM -> Either String MultiVM
pushValSimple val vm = case push val vm.stack of
  Ok stack' => Right ({ stack := stack', pc $= (+1) } vm)
  _ => Left "push overflow"

unaryOp : (Word256 -> Word256) -> MultiVM -> Either String MultiVM
unaryOp f vm = case pop vm.stack of
  Ok (a, stack') => case push (f a) stack' of
    Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
    _ => Left "unary push failed"
  _ => Left "unary pop failed"

binaryOp : (Word256 -> Word256 -> Word256) -> MultiVM -> Either String MultiVM
binaryOp f vm = case popN 2 vm.stack of
  Ok ([a, b], stack') => case push (f a b) stack' of
    Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
    _ => Left "binary push failed"
  _ => Left "binary pop failed"

dupOp : Nat -> MultiVM -> Either String MultiVM
dupOp n vm = case dup n vm.stack of
  Ok stack' => Right ({ stack := stack', pc $= (+1) } vm)
  _ => Left "DUP failed"

swapOp : Nat -> MultiVM -> Either String MultiVM
swapOp n vm = case swap n vm.stack of
  Ok stack' => Right ({ stack := stack', pc $= (+1) } vm)
  _ => Left "SWAP failed"

logOp : Nat -> MultiVM -> Either String MultiVM
logOp numTopics vm = case popN (2 + numTopics) vm.stack of
  Ok (_, stack') => Right ({ stack := stack', pc $= (+1) } vm)
  _ => Left "LOG failed"

-- =============================================================================
-- Main Execution
-- =============================================================================

mutual
  runMultiLoop : Nat -> MultiVM -> WorldState -> MultiResult
  runMultiLoop 0 vm world = MOutOfGas world
  runMultiLoop (S n) vm world =
    if vm.stopped then MSuccess vm.returnData vm.gas world
    else if vm.reverted then MRevert vm.returnData vm.gas world
    else if vm.pc >= codeSize vm.code then MSuccess [] vm.gas world
    else case stepOpcode vm world of
      Done result => result
      Continue vm' world' => runMultiLoop n vm' world'

  stepOpcode : MultiVM -> WorldState -> StepResult
  stepOpcode vm world =
    let opByte = Bytecode.readAt vm.pc vm.code
        op = fromByte opByte
    in executeOp op vm world

  executeOp : Opcode -> MultiVM -> WorldState -> StepResult
  -- Storage operations
  executeOp SLOAD vm world = executeSload vm world
  executeOp SSTORE vm world = executeSstore vm world

  -- Call operations
  executeOp CALL vm world = executeCall vm world
  executeOp DELEGATECALL vm world = executeDelegateCall vm world
  executeOp STATICCALL vm world = executeStaticCall vm world

  -- Control flow termination
  executeOp STOP vm world = Done (MSuccess [] vm.gas world)
  executeOp RETURN vm world = case popN 2 vm.stack of
    Ok ([offset, size], _) =>
      let retData = Bytecode.readBytes (cast $ toInteger offset) (cast $ toInteger size) (toByteList vm.memory)
      in Done (MSuccess retData vm.gas world)
    _ => Done (MStackError "RETURN failed" world)
  executeOp REVERT vm world = case popN 2 vm.stack of
    Ok ([offset, size], _) =>
      let retData = Bytecode.readBytes (cast $ toInteger offset) (cast $ toInteger size) (toByteList vm.memory)
      in Done (MRevert retData vm.gas world)
    _ => Done (MStackError "REVERT failed" world)
  executeOp INVALID vm world = Done (MInvalidOpcode 0xfe world)
  executeOp (UNKNOWN b) vm world = Done (MInvalidOpcode b world)

  -- Simple opcodes - delegate to helper
  executeOp op vm world = case executeSimple op vm of
    Right vm' => Continue vm' world
    Left msg => Done (MStackError msg world)

  executeSimple : Opcode -> MultiVM -> Either String MultiVM
  executeSimple ADD vm = binaryOp Word256.add vm
  executeSimple MUL vm = binaryOp Word256.mul vm
  executeSimple SUB vm = binaryOp Word256.sub vm
  executeSimple DIV vm = binaryOp Word256.div vm
  executeSimple SDIV vm = binaryOp Word256.sdiv vm
  executeSimple MOD vm = binaryOp Word256.mod vm
  executeSimple SMOD vm = binaryOp Word256.mod vm
  executeSimple LT vm = binaryOp Word256.lt vm
  executeSimple GT vm = binaryOp Word256.gt vm
  executeSimple SLT vm = binaryOp Word256.lt vm
  executeSimple SGT vm = binaryOp Word256.gt vm
  executeSimple EQ vm = binaryOp Word256.eq vm
  executeSimple ISZERO vm = unaryOp Word256.iszero vm
  executeSimple AND vm = binaryOp Word256.and vm
  executeSimple OR vm = binaryOp Word256.or vm
  executeSimple XOR vm = binaryOp Word256.xor vm
  executeSimple NOT vm = unaryOp Word256.not vm
  executeSimple BYTE vm = binaryOp Word256.byte vm
  executeSimple SHL vm = binaryOp Word256.shl vm
  executeSimple SHR vm = binaryOp Word256.shr vm
  executeSimple SAR vm = binaryOp Word256.shr vm
  executeSimple ADDRESS vm = pushValSimple vm.context.address vm
  executeSimple ORIGIN vm = pushValSimple vm.context.origin vm
  executeSimple CALLER vm = pushValSimple vm.context.caller vm
  executeSimple CALLVALUE vm = pushValSimple vm.context.callValue vm
  executeSimple CALLDATASIZE vm = pushValSimple (Word256.fromInteger $ cast $ length vm.context.callData) vm
  executeSimple CODESIZE vm = pushValSimple (Word256.fromInteger $ cast $ codeSize vm.code) vm
  executeSimple GASPRICE vm = pushValSimple vm.context.gasPrice vm
  executeSimple TIMESTAMP vm = pushValSimple vm.context.timestamp vm
  executeSimple NUMBER vm = pushValSimple vm.context.blockNumber vm
  executeSimple GASLIMIT vm = pushValSimple vm.context.gasLimit vm
  executeSimple CHAINID vm = pushValSimple vm.context.chainId vm
  executeSimple SELFBALANCE vm = pushValSimple (Word256.fromInteger 1000000000000000000) vm
  executeSimple BASEFEE vm = pushValSimple Word256.one vm
  executeSimple COINBASE vm = pushValSimple Word256.zero vm
  executeSimple PREVRANDAO vm = pushValSimple Word256.zero vm
  executeSimple POP vm = case pop vm.stack of
    Ok (_, stack') => Right ({ stack := stack', pc $= (+1) } vm)
    _ => Left "POP underflow"
  executeSimple MLOAD vm = case pop vm.stack of
    Ok (offset, stack') =>
      let val = mload offset vm.memory
      in case push val stack' of
        Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
        _ => Left "MLOAD push failed"
    _ => Left "MLOAD failed"
  executeSimple MSTORE vm = case popN 2 vm.stack of
    Ok ([offset, val], stack') =>
      let mem' = mstore offset val vm.memory
      in Right ({ stack := stack', memory := mem', pc $= (+1) } vm)
    _ => Left "MSTORE failed"
  executeSimple MSTORE8 vm = case popN 2 vm.stack of
    Ok ([offset, val], stack') =>
      let b = cast {to=Bits8} (toInteger val `mod` 256)
          mem' = writeByte (cast $ toInteger offset) b vm.memory
      in Right ({ stack := stack', memory := mem', pc $= (+1) } vm)
    _ => Left "MSTORE8 failed"
  -- KECCAK256: Read memory and hash
  executeSimple KECCAK256 vm = case popN 2 vm.stack of
    Ok ([offset, size], stack') =>
      let bytes = Bytecode.readBytes (cast $ toInteger offset) (cast $ toInteger size) (toByteList vm.memory)
          simpleHash : Integer
          simpleHash = foldl (\acc, (i, b) => (acc * 31 + b + i * 17) `mod` 0x10000000000000000) 0 (zip (the (List Integer) [0..cast (length bytes)]) (map (cast {to=Integer}) bytes))
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
        _ => Left "KECCAK256 push failed"
    _ => Left "KECCAK256 failed"
  executeSimple JUMP vm = case pop vm.stack of
    Ok (dest, stack') =>
      let destNat = cast {to=Nat} (toInteger dest)
      in if isValidJumpDest destNat vm.code
        then Right ({ stack := stack', pc := destNat } vm)
        else Left $ "Invalid jump destination: PC=" ++ show vm.pc ++ " dest=" ++ show destNat ++ " codeLen=" ++ show (length vm.code)
    _ => Left "JUMP failed"
  executeSimple JUMPI vm = case popN 2 vm.stack of
    Ok ([dest, cond], stack') =>
      if cond /= Word256.zero
        then let destNat = cast {to=Nat} (toInteger dest)
             in if isValidJumpDest destNat vm.code
               then Right ({ stack := stack', pc := destNat } vm)
               else Left $ "Invalid jump destination (JUMPI): PC=" ++ show vm.pc ++ " dest=" ++ show destNat ++ " codeLen=" ++ show (length vm.code)
        else Right ({ stack := stack', pc $= (+1) } vm)
    _ => Left "JUMPI failed"
  executeSimple PC vm = pushValSimple (Word256.fromInteger $ cast vm.pc) vm
  executeSimple MSIZE vm = pushValSimple (Word256.fromInteger $ cast $ memorySize vm.memory * 32) vm
  executeSimple GAS vm = pushValSimple (Word256.fromInteger $ cast vm.gas) vm
  executeSimple JUMPDEST vm = Right ({ pc $= (+1) } vm)
  executeSimple PUSH0 vm = case push Word256.zero vm.stack of
    Ok stack' => Right ({ stack := stack', pc $= (+1) } vm)
    _ => Left "PUSH0 overflow"

  -- CALLDATALOAD
  executeSimple CALLDATALOAD vm = case pop vm.stack of
    Ok (offset, stack') =>
      let off = cast {to=Nat} (toInteger offset)
          bytes = take 32 (drop off vm.context.callData ++ replicate 32 0)
          val = foldl (\acc, b => acc * 256 + cast b) 0 bytes
      in case push (Word256.fromInteger val) stack' of
        Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
        _ => Left "CALLDATALOAD push failed"
    _ => Left "CALLDATALOAD failed"

  -- CALLDATACOPY
  executeSimple CALLDATACOPY vm = case popN 3 vm.stack of
    Ok ([destOffset, srcOffset, size], stack') =>
      let destOff = toInteger destOffset
          srcOff = cast {to=Nat} (toInteger srcOffset)
          sz = cast {to=Nat} (toInteger size)
          calldataBytes = drop srcOff vm.context.callData ++ replicate sz 0
          bytesToCopy = take sz calldataBytes
          mem' = writeBytes destOff bytesToCopy vm.memory
      in Right ({ stack := stack', memory := mem', pc $= (+1) } vm)
    _ => Left "CALLDATACOPY failed"

  -- DUP operations
  executeSimple DUP1 vm = dupOp 0 vm
  executeSimple DUP2 vm = dupOp 1 vm
  executeSimple DUP3 vm = dupOp 2 vm
  executeSimple DUP4 vm = dupOp 3 vm
  executeSimple DUP5 vm = dupOp 4 vm
  executeSimple DUP6 vm = dupOp 5 vm
  executeSimple DUP7 vm = dupOp 6 vm
  executeSimple DUP8 vm = dupOp 7 vm
  executeSimple DUP9 vm = dupOp 8 vm
  executeSimple DUP10 vm = dupOp 9 vm
  executeSimple DUP11 vm = dupOp 10 vm
  executeSimple DUP12 vm = dupOp 11 vm
  executeSimple DUP13 vm = dupOp 12 vm
  executeSimple DUP14 vm = dupOp 13 vm
  executeSimple DUP15 vm = dupOp 14 vm
  executeSimple DUP16 vm = dupOp 15 vm

  -- SWAP operations
  executeSimple SWAP1 vm = swapOp 1 vm
  executeSimple SWAP2 vm = swapOp 2 vm
  executeSimple SWAP3 vm = swapOp 3 vm
  executeSimple SWAP4 vm = swapOp 4 vm
  executeSimple SWAP5 vm = swapOp 5 vm
  executeSimple SWAP6 vm = swapOp 6 vm
  executeSimple SWAP7 vm = swapOp 7 vm
  executeSimple SWAP8 vm = swapOp 8 vm
  executeSimple SWAP9 vm = swapOp 9 vm
  executeSimple SWAP10 vm = swapOp 10 vm
  executeSimple SWAP11 vm = swapOp 11 vm
  executeSimple SWAP12 vm = swapOp 12 vm
  executeSimple SWAP13 vm = swapOp 13 vm
  executeSimple SWAP14 vm = swapOp 14 vm
  executeSimple SWAP15 vm = swapOp 15 vm
  executeSimple SWAP16 vm = swapOp 16 vm

  -- LOG operations
  executeSimple LOG0 vm = logOp 0 vm
  executeSimple LOG1 vm = logOp 1 vm
  executeSimple LOG2 vm = logOp 2 vm
  executeSimple LOG3 vm = logOp 3 vm
  executeSimple LOG4 vm = logOp 4 vm

  -- PUSH operations
  executeSimple op vm =
    if isPush op
      then let size = pushSize op
               val = readPushData vm.pc size vm.code
           in case push val vm.stack of
             Ok stack' => Right ({ stack := stack', pc $= (+ (1 + size)) } vm)
             _ => Left "PUSH overflow"
      else Right ({ pc $= (+1) } vm)  -- fallback: skip unknown opcode

  -- CALL implementation
  executeCall : MultiVM -> WorldState -> StepResult
  executeCall vm world = case popN 7 vm.stack of
    Ok ([gasVal, addrVal, valueVal, argsOffset, argsSize, retOffset, retSize], stack') =>
      if vm.context.callDepth >= MAX_CALL_DEPTH
        then case push Word256.zero stack' of
          Ok stack'' => Continue ({ stack := stack'', pc $= (+1) } vm) world
          _ => Done (MStackError "CALL push failed" world)
        else
          let targetAddr = addrVal
              targetCode = getCode targetAddr world
              calldata = Bytecode.readBytes (cast $ toInteger argsOffset) (cast $ toInteger argsSize) (toByteList vm.memory)
              newCtx = MkCallContext
                targetAddr targetAddr vm.context.address vm.context.origin valueVal
                calldata vm.context.gasPrice vm.context.blockNumber vm.context.timestamp
                vm.context.chainId vm.context.gasLimit vm.context.isStatic (vm.context.callDepth + 1)
              callGas = cast {to=Nat} (toInteger gasVal)
          in case runMultiLoop callGas (initMultiVM targetCode newCtx callGas) world of
            MSuccess retData _ world' =>
              let vm' = copyReturnData vm retData (cast $ toInteger retOffset) (cast $ toInteger retSize)
              in case push Word256.one stack' of
                Ok stack'' => Continue ({ stack := stack'', pc $= (+1), returnData := retData } vm') world'
                _ => Done (MStackError "CALL push failed" world')
            MRevert retData _ world' =>
              let vm' = copyReturnData vm retData (cast $ toInteger retOffset) (cast $ toInteger retSize)
              in case push Word256.zero stack' of
                Ok stack'' => Continue ({ stack := stack'', pc $= (+1), returnData := retData } vm') world'
                _ => Done (MStackError "CALL push failed" world')
            _ => case push Word256.zero stack' of
              Ok stack'' => Continue ({ stack := stack'', pc $= (+1) } vm) world
              _ => Done (MStackError "CALL push failed" world)
    _ => Done (MStackError "CALL failed" world)

  -- DELEGATECALL implementation
  executeDelegateCall : MultiVM -> WorldState -> StepResult
  executeDelegateCall vm world = case popN 6 vm.stack of
    Ok ([gasVal, addrVal, argsOffset, argsSize, retOffset, retSize], stack') =>
      if vm.context.callDepth >= MAX_CALL_DEPTH
        then case push Word256.zero stack' of
          Ok stack'' => Continue ({ stack := stack'', pc $= (+1) } vm) world
          _ => Done (MStackError "DELEGATECALL push failed" world)
        else
          let targetAddr = addrVal
              targetCode = getCode targetAddr world
              calldata = Bytecode.readBytes (cast $ toInteger argsOffset) (cast $ toInteger argsSize) (toByteList vm.memory)
              -- DELEGATECALL: use current storage context, keep caller/value
              newCtx = MkCallContext
                vm.context.address vm.context.storageAddress vm.context.caller vm.context.origin
                vm.context.callValue calldata vm.context.gasPrice vm.context.blockNumber
                vm.context.timestamp vm.context.chainId vm.context.gasLimit vm.context.isStatic
                (vm.context.callDepth + 1)
              callGas = cast {to=Nat} (toInteger gasVal)
          in case runMultiLoop callGas (initMultiVM targetCode newCtx callGas) world of
            MSuccess retData _ world' =>
              let vm' = copyReturnData vm retData (cast $ toInteger retOffset) (cast $ toInteger retSize)
              in case push Word256.one stack' of
                Ok stack'' => Continue ({ stack := stack'', pc $= (+1), returnData := retData } vm') world'
                _ => Done (MStackError "DELEGATECALL push failed" world')
            MRevert retData _ world' =>
              let vm' = copyReturnData vm retData (cast $ toInteger retOffset) (cast $ toInteger retSize)
              in case push Word256.zero stack' of
                Ok stack'' => Continue ({ stack := stack'', pc $= (+1), returnData := retData } vm') world'
                _ => Done (MStackError "DELEGATECALL push failed" world')
            _ => case push Word256.zero stack' of
              Ok stack'' => Continue ({ stack := stack'', pc $= (+1) } vm) world
              _ => Done (MStackError "DELEGATECALL push failed" world)
    _ => Done (MStackError "DELEGATECALL failed" world)

  -- STATICCALL implementation
  executeStaticCall : MultiVM -> WorldState -> StepResult
  executeStaticCall vm world = case popN 6 vm.stack of
    Ok ([gasVal, addrVal, argsOffset, argsSize, retOffset, retSize], stack') =>
      if vm.context.callDepth >= MAX_CALL_DEPTH
        then case push Word256.zero stack' of
          Ok stack'' => Continue ({ stack := stack'', pc $= (+1) } vm) world
          _ => Done (MStackError "STATICCALL push failed" world)
        else
          let targetAddr = addrVal
              targetCode = getCode targetAddr world
              calldata = Bytecode.readBytes (cast $ toInteger argsOffset) (cast $ toInteger argsSize) (toByteList vm.memory)
              newCtx = MkCallContext
                targetAddr targetAddr vm.context.address vm.context.origin Word256.zero
                calldata vm.context.gasPrice vm.context.blockNumber vm.context.timestamp
                vm.context.chainId vm.context.gasLimit True (vm.context.callDepth + 1)
              callGas = cast {to=Nat} (toInteger gasVal)
          in case runMultiLoop callGas (initMultiVM targetCode newCtx callGas) world of
            MSuccess retData _ world' =>
              let vm' = copyReturnData vm retData (cast $ toInteger retOffset) (cast $ toInteger retSize)
              in case push Word256.one stack' of
                Ok stack'' => Continue ({ stack := stack'', pc $= (+1), returnData := retData } vm') world'
                _ => Done (MStackError "STATICCALL push failed" world')
            MRevert retData _ world' =>
              let vm' = copyReturnData vm retData (cast $ toInteger retOffset) (cast $ toInteger retSize)
              in case push Word256.zero stack' of
                Ok stack'' => Continue ({ stack := stack'', pc $= (+1), returnData := retData } vm') world'
                _ => Done (MStackError "STATICCALL push failed" world')
            MStaticViolation world' =>
              case push Word256.zero stack' of
                Ok stack'' => Continue ({ stack := stack'', pc $= (+1) } vm) world
                _ => Done (MStackError "STATICCALL push failed" world)
            _ => case push Word256.zero stack' of
              Ok stack'' => Continue ({ stack := stack'', pc $= (+1) } vm) world
              _ => Done (MStackError "STATICCALL push failed" world)
    _ => Done (MStackError "STATICCALL failed" world)

  copyReturnData : MultiVM -> List Bits8 -> Nat -> Nat -> MultiVM
  copyReturnData vm retData offset size =
    let bytesToCopy = take size retData
        writeBytes : Memory -> Nat -> List Bits8 -> Memory
        writeBytes mem _ [] = mem
        writeBytes mem idx (b :: bs) = writeBytes (writeByte (cast (offset + idx)) b mem) (S idx) bs
    in { memory := writeBytes vm.memory 0 bytesToCopy } vm

-- =============================================================================
-- Public API
-- =============================================================================

||| Run multi-contract execution
export
runMulti : Nat -> MultiVM -> WorldState -> MultiResult
runMulti = runMultiLoop

||| Execute a transaction in multi-contract environment
export
executeTransaction : Word256 -> List Bits8 -> Word256 -> Nat -> WorldState -> MultiResult
executeTransaction targetAddr calldata callerAddr gasLimit world =
  let code = getCode targetAddr world
      ctx = { address := targetAddr
            , storageAddress := targetAddr
            , caller := callerAddr
            , callData := calldata
            } defaultCallContext
      vm = initMultiVM code ctx gasLimit
  in runMulti gasLimit vm world

||| Get the final world state from a result
export
getWorldState : MultiResult -> WorldState
getWorldState (MSuccess _ _ w) = w
getWorldState (MRevert _ _ w) = w
getWorldState (MOutOfGas w) = w
getWorldState (MInvalidJump _ w) = w
getWorldState (MStackError _ w) = w
getWorldState (MInvalidOpcode _ w) = w
getWorldState (MStaticViolation w) = w
getWorldState (MCallDepthExceeded w) = w

||| Check if result is success
export
isSuccess : MultiResult -> Bool
isSuccess (MSuccess _ _ _) = True
isSuccess _ = False
