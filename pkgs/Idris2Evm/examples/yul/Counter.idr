||| Simple Counter Contract
||| Demonstrates basic storage and function dispatch
module Main

import EVM.Primitives

-- =============================================================================
-- Storage Layout
-- =============================================================================

||| Storage slot for counter value
COUNTER_SLOT : Integer
COUNTER_SLOT = 0

-- =============================================================================
-- Contract Logic
-- =============================================================================

||| Get current counter value
getCount : IO Integer
getCount = sload COUNTER_SLOT

||| Increment counter by 1
increment : IO ()
increment = do
  current <- sload COUNTER_SLOT
  sstore COUNTER_SLOT (current + 1)

||| Decrement counter by 1
decrement : IO ()
decrement = do
  current <- sload COUNTER_SLOT
  if current > 0
    then sstore COUNTER_SLOT (current - 1)
    else pure ()

||| Set counter to specific value
setCount : Integer -> IO ()
setCount val = sstore COUNTER_SLOT val

-- =============================================================================
-- Function Selectors (keccak256 first 4 bytes)
-- =============================================================================

-- getCount() -> 0xa87d942c
SEL_GET_COUNT : Integer
SEL_GET_COUNT = 0xa87d942c

-- increment() -> 0xd09de08a
SEL_INCREMENT : Integer
SEL_INCREMENT = 0xd09de08a

-- decrement() -> 0x2baeceb7
SEL_DECREMENT : Integer
SEL_DECREMENT = 0x2baeceb7

-- setCount(uint256) -> 0xd14e62b8
SEL_SET_COUNT : Integer
SEL_SET_COUNT = 0xd14e62b8

-- =============================================================================
-- Entry Point (Dispatcher)
-- =============================================================================

||| Main dispatch function
main : IO ()
main = do
  selector <- getSelector
  if selector == SEL_GET_COUNT
    then do
      count <- getCount
      returnUint count
    else if selector == SEL_INCREMENT
    then increment
    else if selector == SEL_DECREMENT
    then decrement
    else if selector == SEL_SET_COUNT
    then do
      arg <- calldataload 4
      setCount arg
    else evmRevert 0 0  -- Unknown function
