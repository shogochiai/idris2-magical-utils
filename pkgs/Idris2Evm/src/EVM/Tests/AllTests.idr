||| EVM Tests - Comprehensive test suite for EVM components
module EVM.Tests.AllTests

import Data.List

import EVM.Word256
import EVM.Stack
import EVM.Memory
import EVM.Storage.Model as Storage
import EVM.Opcodes

-- =============================================================================
-- Word256 Tests
-- =============================================================================

||| REQ_W256_001: zero should be (0, 0)
export
test_word256_zero : IO Bool
test_word256_zero = pure $ Word256.zero.high == 0 && Word256.zero.low == 0

||| REQ_W256_002: one should be (0, 1)
export
test_word256_one : IO Bool
test_word256_one = pure $ Word256.one.high == 0 && Word256.one.low == 1

||| REQ_W256_003: fromInteger 0 should equal zero
export
test_word256_fromInteger_zero : IO Bool
test_word256_fromInteger_zero =
  let w = Word256.fromInteger 0
  in pure $ w.high == 0 && w.low == 0

||| REQ_W256_004: fromInteger should handle small values
export
test_word256_fromInteger_small : IO Bool
test_word256_fromInteger_small =
  let w = Word256.fromInteger 42
  in pure $ w.high == 0 && w.low == 42

||| REQ_W256_005: fromInteger should handle large values
export
test_word256_fromInteger_large : IO Bool
test_word256_fromInteger_large =
  let w = Word256.fromInteger 0xFFFFFFFFFFFFFFFF  -- 64-bit max
  in pure $ w.high == 0 && w.low == 0xFFFFFFFFFFFFFFFF

||| REQ_W256_006: maxWord should have all bits set
export
test_word256_maxWord : IO Bool
test_word256_maxWord =
  pure $ maxWord.high == 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF &&
         maxWord.low == 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

-- =============================================================================
-- Stack Tests
-- =============================================================================

||| REQ_STACK_001: empty stack should be empty
export
test_stack_empty : IO Bool
test_stack_empty = pure $ isEmpty Stack.empty

||| REQ_STACK_002: empty stack should have depth 0
export
test_stack_empty_depth : IO Bool
test_stack_empty_depth = pure $ depth Stack.empty == 0

||| REQ_STACK_003: maxStackDepth should be 1024
export
test_stack_max_depth : IO Bool
test_stack_max_depth = pure $ maxStackDepth == 1024

||| REQ_STACK_004: empty stack should not overflow
export
test_stack_no_overflow : IO Bool
test_stack_no_overflow = pure $ not (wouldOverflow Stack.empty)

-- =============================================================================
-- Opcode Tests (using fromByte for verification)
-- =============================================================================

||| REQ_OP_001: fromByte 0x00 should be STOP
export
test_opcode_stop : IO Bool
test_opcode_stop = pure $ fromByte 0x00 == STOP

||| REQ_OP_002: fromByte 0x01 should be ADD
export
test_opcode_add : IO Bool
test_opcode_add = pure $ fromByte 0x01 == ADD

||| REQ_OP_003: fromByte 0x02 should be MUL
export
test_opcode_mul : IO Bool
test_opcode_mul = pure $ fromByte 0x02 == MUL

||| REQ_OP_004: fromByte 0x03 should be SUB
export
test_opcode_sub : IO Bool
test_opcode_sub = pure $ fromByte 0x03 == SUB

||| REQ_OP_005: fromByte 0x04 should be DIV
export
test_opcode_div : IO Bool
test_opcode_div = pure $ fromByte 0x04 == DIV

||| REQ_OP_006: fromByte 0x60 should be PUSH1
export
test_opcode_push1 : IO Bool
test_opcode_push1 = pure $ fromByte 0x60 == PUSH1

||| REQ_OP_007: fromByte 0x7f should be PUSH32
export
test_opcode_push32 : IO Bool
test_opcode_push32 = pure $ fromByte 0x7f == PUSH32

||| REQ_OP_008: fromByte 0x80 should be DUP1
export
test_opcode_dup1 : IO Bool
test_opcode_dup1 = pure $ fromByte 0x80 == DUP1

||| REQ_OP_009: fromByte 0x90 should be SWAP1
export
test_opcode_swap1 : IO Bool
test_opcode_swap1 = pure $ fromByte 0x90 == SWAP1

||| REQ_OP_010: fromByte 0x54 should be SLOAD
export
test_opcode_sload : IO Bool
test_opcode_sload = pure $ fromByte 0x54 == SLOAD

||| REQ_OP_011: fromByte 0x55 should be SSTORE
export
test_opcode_sstore : IO Bool
test_opcode_sstore = pure $ fromByte 0x55 == SSTORE

||| REQ_OP_012: fromByte 0xf1 should be CALL
export
test_opcode_call : IO Bool
test_opcode_call = pure $ fromByte 0xf1 == CALL

||| REQ_OP_013: fromByte 0xf3 should be RETURN
export
test_opcode_return : IO Bool
test_opcode_return = pure $ fromByte 0xf3 == RETURN

||| REQ_OP_014: fromByte 0xfd should be REVERT
export
test_opcode_revert : IO Bool
test_opcode_revert = pure $ fromByte 0xfd == REVERT

||| REQ_OP_015: fromByte 0xf5 should be CREATE2
export
test_opcode_create2 : IO Bool
test_opcode_create2 = pure $ fromByte 0xf5 == CREATE2

||| REQ_OP_016: PUSH1 should have pushSize 1
export
test_opcode_push1_size : IO Bool
test_opcode_push1_size = pure $ pushSize PUSH1 == 1

||| REQ_OP_017: PUSH32 should have pushSize 32
export
test_opcode_push32_size : IO Bool
test_opcode_push32_size = pure $ pushSize PUSH32 == 32

||| REQ_OP_018: ADD should not be PUSH
export
test_opcode_add_not_push : IO Bool
test_opcode_add_not_push = pure $ not (isPush ADD)

||| REQ_OP_019: PUSH1 should be PUSH
export
test_opcode_push1_is_push : IO Bool
test_opcode_push1_is_push = pure $ isPush PUSH1

||| REQ_OP_020: STOP should be terminating
export
test_opcode_stop_terminates : IO Bool
test_opcode_stop_terminates = pure $ isTerminating STOP

||| REQ_OP_021: RETURN should be terminating
export
test_opcode_return_terminates : IO Bool
test_opcode_return_terminates = pure $ isTerminating RETURN

||| REQ_OP_022: REVERT should be terminating
export
test_opcode_revert_terminates : IO Bool
test_opcode_revert_terminates = pure $ isTerminating REVERT

||| REQ_OP_023: ADD should not be terminating
export
test_opcode_add_not_terminates : IO Bool
test_opcode_add_not_terminates = pure $ not (isTerminating ADD)

-- =============================================================================
-- Extended Opcode Tests (High-Impact Coverage)
-- =============================================================================

||| REQ_OP_024: INVALID should be terminating
export
test_opcode_invalid_terminates : IO Bool
test_opcode_invalid_terminates = pure $ isTerminating INVALID

||| REQ_OP_025: SELFDESTRUCT should be terminating
export
test_opcode_selfdestruct_terminates : IO Bool
test_opcode_selfdestruct_terminates = pure $ isTerminating SELFDESTRUCT

||| REQ_OP_026: PUSH0 is push
export
test_opcode_push0_is_push : IO Bool
test_opcode_push0_is_push = pure $ isPush PUSH0

||| REQ_OP_027: All PUSH2-16 are push
export
test_opcode_push2_16_is_push : IO Bool
test_opcode_push2_16_is_push =
  pure $ isPush PUSH2 && isPush PUSH3 && isPush PUSH4 && isPush PUSH5 &&
         isPush PUSH6 && isPush PUSH7 && isPush PUSH8 && isPush PUSH9 &&
         isPush PUSH10 && isPush PUSH11 && isPush PUSH12 && isPush PUSH13 &&
         isPush PUSH14 && isPush PUSH15 && isPush PUSH16

||| REQ_OP_028: All PUSH17-32 are push
export
test_opcode_push17_32_is_push : IO Bool
test_opcode_push17_32_is_push =
  pure $ isPush PUSH17 && isPush PUSH18 && isPush PUSH19 && isPush PUSH20 &&
         isPush PUSH21 && isPush PUSH22 && isPush PUSH23 && isPush PUSH24 &&
         isPush PUSH25 && isPush PUSH26 && isPush PUSH27 && isPush PUSH28 &&
         isPush PUSH29 && isPush PUSH30 && isPush PUSH31 && isPush PUSH32

||| REQ_OP_029: pushSize PUSH2-8
export
test_opcode_push2_8_size : IO Bool
test_opcode_push2_8_size =
  pure $ pushSize PUSH2 == 2 && pushSize PUSH3 == 3 && pushSize PUSH4 == 4 &&
         pushSize PUSH5 == 5 && pushSize PUSH6 == 6 && pushSize PUSH7 == 7 &&
         pushSize PUSH8 == 8

||| REQ_OP_030: pushSize PUSH9-16
export
test_opcode_push9_16_size : IO Bool
test_opcode_push9_16_size =
  pure $ pushSize PUSH9 == 9 && pushSize PUSH10 == 10 && pushSize PUSH11 == 11 &&
         pushSize PUSH12 == 12 && pushSize PUSH13 == 13 && pushSize PUSH14 == 14 &&
         pushSize PUSH15 == 15 && pushSize PUSH16 == 16

||| REQ_OP_031: pushSize PUSH17-24
export
test_opcode_push17_24_size : IO Bool
test_opcode_push17_24_size =
  pure $ pushSize PUSH17 == 17 && pushSize PUSH18 == 18 && pushSize PUSH19 == 19 &&
         pushSize PUSH20 == 20 && pushSize PUSH21 == 21 && pushSize PUSH22 == 22 &&
         pushSize PUSH23 == 23 && pushSize PUSH24 == 24

||| REQ_OP_032: pushSize PUSH25-31
export
test_opcode_push25_31_size : IO Bool
test_opcode_push25_31_size =
  pure $ pushSize PUSH25 == 25 && pushSize PUSH26 == 26 && pushSize PUSH27 == 27 &&
         pushSize PUSH28 == 28 && pushSize PUSH29 == 29 && pushSize PUSH30 == 30 &&
         pushSize PUSH31 == 31

||| REQ_OP_033: pushSize non-push returns 0
export
test_opcode_pushsize_nonpush : IO Bool
test_opcode_pushsize_nonpush =
  pure $ pushSize ADD == 0 && pushSize STOP == 0 && pushSize CALL == 0

||| REQ_OP_034: fromByte comparison opcodes
export
test_opcode_fromByte_comparison : IO Bool
test_opcode_fromByte_comparison =
  pure $ fromByte 0x10 == LT && fromByte 0x11 == GT &&
         fromByte 0x12 == SLT && fromByte 0x13 == SGT &&
         fromByte 0x14 == EQ && fromByte 0x15 == ISZERO

||| REQ_OP_035: fromByte bitwise opcodes
export
test_opcode_fromByte_bitwise : IO Bool
test_opcode_fromByte_bitwise =
  pure $ fromByte 0x16 == AND && fromByte 0x17 == OR &&
         fromByte 0x18 == XOR && fromByte 0x19 == NOT &&
         fromByte 0x1a == BYTE

||| REQ_OP_036: fromByte shift opcodes
export
test_opcode_fromByte_shift : IO Bool
test_opcode_fromByte_shift =
  pure $ fromByte 0x1b == SHL && fromByte 0x1c == SHR && fromByte 0x1d == SAR

||| REQ_OP_037: fromByte environmental opcodes
export
test_opcode_fromByte_env : IO Bool
test_opcode_fromByte_env =
  pure $ fromByte 0x30 == ADDRESS && fromByte 0x31 == BALANCE &&
         fromByte 0x32 == ORIGIN && fromByte 0x33 == CALLER &&
         fromByte 0x34 == CALLVALUE

||| REQ_OP_038: fromByte memory opcodes
export
test_opcode_fromByte_memory : IO Bool
test_opcode_fromByte_memory =
  pure $ fromByte 0x50 == POP && fromByte 0x51 == MLOAD &&
         fromByte 0x52 == MSTORE && fromByte 0x53 == MSTORE8

||| REQ_OP_039: fromByte LOG opcodes
export
test_opcode_fromByte_log : IO Bool
test_opcode_fromByte_log =
  pure $ fromByte 0xa0 == LOG0 && fromByte 0xa1 == LOG1 &&
         fromByte 0xa2 == LOG2 && fromByte 0xa3 == LOG3 &&
         fromByte 0xa4 == LOG4

||| REQ_OP_040: fromByte system opcodes
export
test_opcode_fromByte_system : IO Bool
test_opcode_fromByte_system =
  pure $ fromByte 0xf0 == CREATE && fromByte 0xf1 == CALL &&
         fromByte 0xf2 == CALLCODE && fromByte 0xf4 == DELEGATECALL &&
         fromByte 0xfa == STATICCALL

||| REQ_OP_041: fromByte DUP opcodes
export
test_opcode_fromByte_dup : IO Bool
test_opcode_fromByte_dup =
  pure $ fromByte 0x80 == DUP1 && fromByte 0x81 == DUP2 &&
         fromByte 0x8f == DUP16

||| REQ_OP_042: fromByte SWAP opcodes
export
test_opcode_fromByte_swap : IO Bool
test_opcode_fromByte_swap =
  pure $ fromByte 0x90 == SWAP1 && fromByte 0x91 == SWAP2 &&
         fromByte 0x9f == SWAP16

||| REQ_OP_043: fromByte block info opcodes
export
test_opcode_fromByte_blockinfo : IO Bool
test_opcode_fromByte_blockinfo =
  pure $ fromByte 0x40 == BLOCKHASH && fromByte 0x41 == COINBASE &&
         fromByte 0x42 == TIMESTAMP && fromByte 0x43 == NUMBER &&
         fromByte 0x46 == CHAINID

||| REQ_OP_044: fromByte PUSH0
export
test_opcode_fromByte_push0 : IO Bool
test_opcode_fromByte_push0 = pure $ fromByte 0x5f == PUSH0

-- =============================================================================
-- Memory Tests
-- =============================================================================

||| REQ_MEM_001: empty memory should have size 0
export
test_memory_empty_size : IO Bool
test_memory_empty_size = pure $ memorySize Memory.empty == 0

||| REQ_MEM_002: readByte from empty memory should be 0
export
test_memory_empty_read : IO Bool
test_memory_empty_read = pure $ readByte 0 Memory.empty == 0

-- =============================================================================
-- Storage Tests
-- =============================================================================

||| REQ_STOR_001: sload from empty storage should return zero
export
test_storage_empty_sload : IO Bool
test_storage_empty_sload =
  let val = sload Word256.zero Storage.empty
  in pure $ val.high == 0 && val.low == 0

-- =============================================================================
-- Test Runner
-- =============================================================================

allTests : List (String, IO Bool)
allTests =
  [ ("REQ_W256_001: zero constant", test_word256_zero)
  , ("REQ_W256_002: one constant", test_word256_one)
  , ("REQ_W256_003: fromInteger zero", test_word256_fromInteger_zero)
  , ("REQ_W256_004: fromInteger small", test_word256_fromInteger_small)
  , ("REQ_W256_005: fromInteger large", test_word256_fromInteger_large)
  , ("REQ_W256_006: maxWord constant", test_word256_maxWord)
  , ("REQ_STACK_001: empty is empty", test_stack_empty)
  , ("REQ_STACK_002: empty depth", test_stack_empty_depth)
  , ("REQ_STACK_003: max depth", test_stack_max_depth)
  , ("REQ_STACK_004: no overflow", test_stack_no_overflow)
  , ("REQ_OP_001: STOP byte", test_opcode_stop)
  , ("REQ_OP_002: ADD byte", test_opcode_add)
  , ("REQ_OP_003: MUL byte", test_opcode_mul)
  , ("REQ_OP_004: SUB byte", test_opcode_sub)
  , ("REQ_OP_005: DIV byte", test_opcode_div)
  , ("REQ_OP_006: PUSH1 byte", test_opcode_push1)
  , ("REQ_OP_007: PUSH32 byte", test_opcode_push32)
  , ("REQ_OP_008: DUP1 byte", test_opcode_dup1)
  , ("REQ_OP_009: SWAP1 byte", test_opcode_swap1)
  , ("REQ_OP_010: SLOAD byte", test_opcode_sload)
  , ("REQ_OP_011: SSTORE byte", test_opcode_sstore)
  , ("REQ_OP_012: CALL byte", test_opcode_call)
  , ("REQ_OP_013: RETURN byte", test_opcode_return)
  , ("REQ_OP_014: REVERT byte", test_opcode_revert)
  , ("REQ_OP_015: CREATE2 byte", test_opcode_create2)
  , ("REQ_OP_016: PUSH1 size", test_opcode_push1_size)
  , ("REQ_OP_017: PUSH32 size", test_opcode_push32_size)
  , ("REQ_OP_018: ADD not push", test_opcode_add_not_push)
  , ("REQ_OP_019: PUSH1 is push", test_opcode_push1_is_push)
  , ("REQ_OP_020: STOP terminates", test_opcode_stop_terminates)
  , ("REQ_OP_021: RETURN terminates", test_opcode_return_terminates)
  , ("REQ_OP_022: REVERT terminates", test_opcode_revert_terminates)
  , ("REQ_OP_023: ADD not terminates", test_opcode_add_not_terminates)
  , ("REQ_OP_024: INVALID terminates", test_opcode_invalid_terminates)
  , ("REQ_OP_025: SELFDESTRUCT terminates", test_opcode_selfdestruct_terminates)
  , ("REQ_OP_026: PUSH0 is push", test_opcode_push0_is_push)
  , ("REQ_OP_027: PUSH2-16 is push", test_opcode_push2_16_is_push)
  , ("REQ_OP_028: PUSH17-32 is push", test_opcode_push17_32_is_push)
  , ("REQ_OP_029: pushSize PUSH2-8", test_opcode_push2_8_size)
  , ("REQ_OP_030: pushSize PUSH9-16", test_opcode_push9_16_size)
  , ("REQ_OP_031: pushSize PUSH17-24", test_opcode_push17_24_size)
  , ("REQ_OP_032: pushSize PUSH25-31", test_opcode_push25_31_size)
  , ("REQ_OP_033: pushSize non-push", test_opcode_pushsize_nonpush)
  , ("REQ_OP_034: fromByte comparison", test_opcode_fromByte_comparison)
  , ("REQ_OP_035: fromByte bitwise", test_opcode_fromByte_bitwise)
  , ("REQ_OP_036: fromByte shift", test_opcode_fromByte_shift)
  , ("REQ_OP_037: fromByte env", test_opcode_fromByte_env)
  , ("REQ_OP_038: fromByte memory", test_opcode_fromByte_memory)
  , ("REQ_OP_039: fromByte LOG", test_opcode_fromByte_log)
  , ("REQ_OP_040: fromByte system", test_opcode_fromByte_system)
  , ("REQ_OP_041: fromByte DUP", test_opcode_fromByte_dup)
  , ("REQ_OP_042: fromByte SWAP", test_opcode_fromByte_swap)
  , ("REQ_OP_043: fromByte blockinfo", test_opcode_fromByte_blockinfo)
  , ("REQ_OP_044: fromByte PUSH0", test_opcode_fromByte_push0)
  , ("REQ_MEM_001: empty memory size", test_memory_empty_size)
  , ("REQ_MEM_002: empty memory read", test_memory_empty_read)
  , ("REQ_STOR_001: empty storage sload", test_storage_empty_sload)
  ]

runTest : (String, IO Bool) -> IO (String, Bool)
runTest (name, test) = do
  result <- test
  putStrLn $ (if result then "[PASS] " else "[FAIL] ") ++ name
  pure (name, result)

||| Run all tests - entry point for lazy test runner
export
runAllTests : IO ()
runAllTests = do
  putStrLn "Running EVM Tests..."
  putStrLn ""
  results <- traverse runTest allTests
  let passed = filter snd results
  let failed = filter (not . snd) results
  traverse_ (\(name, _) => putStrLn $ "  FAIL: " ++ name) failed
  putStrLn ""
  putStrLn $ "Results: " ++ show (length passed) ++ "/" ++ show (length results) ++ " passed"
  if length failed == 0
     then putStrLn "ALL TESTS PASSED"
     else putStrLn "SOME TESTS FAILED"

export
main : IO ()
main = runAllTests
