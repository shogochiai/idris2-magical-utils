||| MC Standard: ERC20 Token
||| Idris2 implementation of ERC-20 token standard
|||
||| Implements balanceOf, transfer, approve, transferFrom, allowance
module Main

import EVM.Primitives

-- =============================================================================
-- Storage Layout
-- =============================================================================

||| Base slot for balances mapping
||| slot = keccak256(account . SLOT_BALANCES)
SLOT_BALANCES : Integer
SLOT_BALANCES = 0

||| Base slot for allowances mapping (nested: owner -> spender -> amount)
||| slot = keccak256(spender . keccak256(owner . SLOT_ALLOWANCES))
SLOT_ALLOWANCES : Integer
SLOT_ALLOWANCES = 1

||| Storage slot for total supply
SLOT_TOTAL_SUPPLY : Integer
SLOT_TOTAL_SUPPLY = 2

-- =============================================================================
-- Storage Helpers
-- =============================================================================

||| Get storage slot for an account's balance
getBalanceSlot : Integer -> IO Integer
getBalanceSlot account = do
  mstore 0 account
  mstore 32 SLOT_BALANCES
  keccak256 0 64

||| Get storage slot for allowance(owner, spender)
getAllowanceSlot : Integer -> Integer -> IO Integer
getAllowanceSlot owner spender = do
  -- First hash: keccak256(owner . SLOT_ALLOWANCES)
  mstore 0 owner
  mstore 32 SLOT_ALLOWANCES
  ownerSlot <- keccak256 0 64
  -- Second hash: keccak256(spender . ownerSlot)
  mstore 0 spender
  mstore 32 ownerSlot
  keccak256 0 64

-- =============================================================================
-- Event Signatures
-- =============================================================================

||| Transfer(address indexed from, address indexed to, uint256 value)
||| keccak256("Transfer(address,address,uint256)")
EVENT_TRANSFER : Integer
EVENT_TRANSFER = 0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef

||| Approval(address indexed owner, address indexed spender, uint256 value)
||| keccak256("Approval(address,address,uint256)")
EVENT_APPROVAL : Integer
EVENT_APPROVAL = 0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925

-- =============================================================================
-- Event Emission
-- =============================================================================

emitTransfer : Integer -> Integer -> Integer -> IO ()
emitTransfer from to amount = do
  mstore 0 amount
  log3 0 32 EVENT_TRANSFER from to

emitApproval : Integer -> Integer -> Integer -> IO ()
emitApproval owner spender amount = do
  mstore 0 amount
  log3 0 32 EVENT_APPROVAL owner spender

-- =============================================================================
-- ERC20 Functions
-- =============================================================================

||| Get total supply
totalSupply : IO Integer
totalSupply = sload SLOT_TOTAL_SUPPLY

||| Get balance of an account
balanceOf : Integer -> IO Integer
balanceOf account = do
  slot <- getBalanceSlot account
  sload slot

||| Get allowance(owner, spender)
allowance : Integer -> Integer -> IO Integer
allowance owner spender = do
  slot <- getAllowanceSlot owner spender
  sload slot

||| Transfer tokens from caller to recipient
||| Returns True on success
transfer : Integer -> Integer -> IO Bool
transfer to amount = do
  from <- caller
  fromSlot <- getBalanceSlot from
  fromBalance <- sload fromSlot
  -- Check sufficient balance
  if fromBalance < amount
    then pure False
    else do
      toSlot <- getBalanceSlot to
      toBalance <- sload toSlot
      -- Update balances
      sstore fromSlot (fromBalance - amount)
      sstore toSlot (toBalance + amount)
      -- Emit Transfer event
      emitTransfer from to amount
      pure True

||| Approve spender to spend amount on behalf of caller
approve : Integer -> Integer -> IO Bool
approve spender amount = do
  owner <- caller
  slot <- getAllowanceSlot owner spender
  sstore slot amount
  -- Emit Approval event
  emitApproval owner spender amount
  pure True

||| Transfer tokens from 'from' to 'to' (requires allowance)
transferFrom : Integer -> Integer -> Integer -> IO Bool
transferFrom from to amount = do
  spender <- caller
  -- Check allowance
  allowanceSlot <- getAllowanceSlot from spender
  allowed <- sload allowanceSlot
  if allowed < amount
    then pure False
    else do
      -- Check balance
      fromSlot <- getBalanceSlot from
      fromBalance <- sload fromSlot
      if fromBalance < amount
        then pure False
        else do
          toSlot <- getBalanceSlot to
          toBalance <- sload toSlot
          -- Update state
          sstore fromSlot (fromBalance - amount)
          sstore toSlot (toBalance + amount)
          sstore allowanceSlot (allowed - amount)
          -- Emit Transfer event
          emitTransfer from to amount
          pure True

-- =============================================================================
-- Function Selectors (ERC20 standard)
-- =============================================================================

-- totalSupply() -> 0x18160ddd
SEL_TOTAL_SUPPLY : Integer
SEL_TOTAL_SUPPLY = 0x18160ddd

-- balanceOf(address) -> 0x70a08231
SEL_BALANCE_OF : Integer
SEL_BALANCE_OF = 0x70a08231

-- transfer(address,uint256) -> 0xa9059cbb
SEL_TRANSFER : Integer
SEL_TRANSFER = 0xa9059cbb

-- approve(address,uint256) -> 0x095ea7b3
SEL_APPROVE : Integer
SEL_APPROVE = 0x095ea7b3

-- allowance(address,address) -> 0xdd62ed3e
SEL_ALLOWANCE : Integer
SEL_ALLOWANCE = 0xdd62ed3e

-- transferFrom(address,address,uint256) -> 0x23b872dd
SEL_TRANSFER_FROM : Integer
SEL_TRANSFER_FROM = 0x23b872dd

-- mint(address,uint256) -> 0x40c10f19 (for testing)
SEL_MINT : Integer
SEL_MINT = 0x40c10f19

-- =============================================================================
-- Helper: Mint (for testing, no access control)
-- =============================================================================

mint : Integer -> Integer -> IO ()
mint to amount = do
  toSlot <- getBalanceSlot to
  toBalance <- sload toSlot
  supply <- sload SLOT_TOTAL_SUPPLY
  sstore toSlot (toBalance + amount)
  sstore SLOT_TOTAL_SUPPLY (supply + amount)
  -- Emit Transfer from zero address
  emitTransfer 0 to amount

-- =============================================================================
-- Entry Point
-- =============================================================================

||| Read address from calldata (right-aligned in 32 bytes)
readAddress : Integer -> IO Integer
readAddress offset = calldataload offset

main : IO ()
main = do
  selector <- getSelector

  if selector == SEL_TOTAL_SUPPLY
    then do
      supply <- totalSupply
      returnUint supply

    else if selector == SEL_BALANCE_OF
    then do
      account <- readAddress 4
      bal <- balanceOf account
      returnUint bal

    else if selector == SEL_TRANSFER
    then do
      to <- readAddress 4
      amount <- calldataload 36
      success <- transfer to amount
      returnBool success

    else if selector == SEL_APPROVE
    then do
      spender <- readAddress 4
      amount <- calldataload 36
      success <- approve spender amount
      returnBool success

    else if selector == SEL_ALLOWANCE
    then do
      owner <- readAddress 4
      spender <- readAddress 36
      allowed <- allowance owner spender
      returnUint allowed

    else if selector == SEL_TRANSFER_FROM
    then do
      from <- readAddress 4
      to <- readAddress 36
      amount <- calldataload 68
      success <- transferFrom from to amount
      returnBool success

    else if selector == SEL_MINT
    then do
      to <- readAddress 4
      amount <- calldataload 36
      mint to amount
      returnBool True

    else evmRevert 0 0
