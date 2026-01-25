||| TokenPJ Transfer Tests
|||
||| Test cases for transfer functionality.
||| Run via idris2-evm interpreter.
module Main.Tests.TransferTest

import EVM.Primitives
import Main.Storages.Schema
import Main.Functions.Transfer

-- =============================================================================
-- Test Helpers
-- =============================================================================

||| Assert condition, revert if false
assert : Bool -> IO ()
assert True = pure ()
assert False = evmRevert 0 0

||| Test addresses
ALICE : Integer
ALICE = 0x1111111111111111111111111111111111111111

BOB : Integer
BOB = 0x2222222222222222222222222222222222222222

-- =============================================================================
-- Tests
-- =============================================================================

||| Test: Transfer succeeds with sufficient balance
||| Setup: Alice has 1000 tokens
||| Action: Alice transfers 100 to Bob
||| Expected: Alice has 900, Bob has 100
export
testTransferSuccess : IO ()
testTransferSuccess = do
  -- Setup
  setBalance ALICE 1000
  setBalance BOB 0

  -- Action
  success <- transfer ALICE BOB 100

  -- Assert
  assert success
  aliceBal <- getBalance ALICE
  assert (aliceBal == 900)
  bobBal <- getBalance BOB
  assert (bobBal == 100)

||| Test: Transfer fails with insufficient balance
||| Setup: Alice has 50 tokens
||| Action: Alice tries to transfer 100 to Bob
||| Expected: Transfer returns False, balances unchanged
export
testTransferInsufficientBalance : IO ()
testTransferInsufficientBalance = do
  -- Setup
  setBalance ALICE 50
  setBalance BOB 0

  -- Action
  success <- transfer ALICE BOB 100

  -- Assert
  assert (not success)
  aliceBal <- getBalance ALICE
  assert (aliceBal == 50)
  bobBal <- getBalance BOB
  assert (bobBal == 0)

||| Test: Transfer zero amount
||| Should succeed (no-op)
export
testTransferZero : IO ()
testTransferZero = do
  -- Setup
  setBalance ALICE 1000

  -- Action
  success <- transfer ALICE BOB 0

  -- Assert
  assert success
  aliceBal <- getBalance ALICE
  assert (aliceBal == 1000)

-- =============================================================================
-- Test Runner
-- =============================================================================

||| Run all transfer tests
export
runTests : IO ()
runTests = do
  testTransferSuccess
  testTransferInsufficientBalance
  testTransferZero
  -- All tests passed
  stop
