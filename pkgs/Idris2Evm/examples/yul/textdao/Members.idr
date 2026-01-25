||| TextDAO Members Function
||| REQ_MEMBERS_001: Member registration and lookup
module TextDAO.Functions.Members

import TextDAO.Storages.Schema
import EVM.Primitives

%default covering

-- =============================================================================
-- Member Storage Layout
-- =============================================================================

||| Offset for member address within member struct
MEMBER_OFFSET_ADDR : Integer
MEMBER_OFFSET_ADDR = 0

||| Offset for member metadata CID within member struct
MEMBER_OFFSET_METADATA : Integer
MEMBER_OFFSET_METADATA = 1

||| Member struct size (2 slots: addr + metadata)
MEMBER_SIZE : Integer
MEMBER_SIZE = 2

-- =============================================================================
-- Function Selectors
-- =============================================================================

||| addMember(address,bytes32) -> 0xca6d56dc
SEL_ADD_MEMBER : Integer
SEL_ADD_MEMBER = 0xca6d56dc

||| getMember(uint256) -> 0x9c0a0cd2
SEL_GET_MEMBER : Integer
SEL_GET_MEMBER = 0x9c0a0cd2

||| getMemberCount() -> 0x997072f7
SEL_GET_MEMBER_COUNT : Integer
SEL_GET_MEMBER_COUNT = 0x997072f7

||| isMember(address) -> 0xa230c524
SEL_IS_MEMBER : Integer
SEL_IS_MEMBER = 0xa230c524

-- =============================================================================
-- Entry Point Helpers
-- =============================================================================

-- =============================================================================
-- Member Read Functions
-- =============================================================================

||| Get member address by index
||| REQ_MEMBERS_002
export
getMemberAddr : Integer -> IO Address
getMemberAddr index = do
  slot <- getMemberSlot index
  sload (slot + MEMBER_OFFSET_ADDR)

||| Get member metadata by index
export
getMemberMetadata : Integer -> IO MetadataCid
getMemberMetadata index = do
  slot <- getMemberSlot index
  sload (slot + MEMBER_OFFSET_METADATA)

mutual
  ||| Check if address is a member (linear search)
  ||| REQ_MEMBERS_003
  export
  isMember : Address -> IO Bool
  isMember addr = do
    count <- getMemberCount
    checkMemberLoop addr 0 count

  ||| Helper function for member lookup loop
  checkMemberLoop : Address -> Integer -> Integer -> IO Bool
  checkMemberLoop target idx count =
    if idx >= count
      then pure False
      else getMemberAddr idx >>= \memberAddr =>
        if memberAddr == target
          then pure True
          else checkMemberLoop target (idx + 1) count

-- =============================================================================
-- Member Write Functions
-- =============================================================================

||| Add a new member
||| REQ_MEMBERS_004
export
addMember : Address -> MetadataCid -> IO Integer
addMember addr metadata = do
  count <- getMemberCount
  slot <- getMemberSlot count
  sstore (slot + MEMBER_OFFSET_ADDR) addr
  sstore (slot + MEMBER_OFFSET_METADATA) metadata
  setMemberCount (count + 1)
  pure count

-- =============================================================================
-- Access Control Modifier
-- =============================================================================

||| Revert if caller is not a member
||| REQ_MEMBERS_005
export
requireMember : Address -> IO Bool
requireMember callerAddr = do
  member <- isMember callerAddr
  if member
    then pure True
    else do
      evmRevert 0 0
      pure False

-- =============================================================================
-- Entry Point
-- =============================================================================

||| Main entry point for Members contract
export
main : IO ()
main = do
  selector <- getSelector

  if selector == SEL_ADD_MEMBER
    then do
      memberAddr <- calldataload 4
      metadataCid <- calldataload 36
      idx <- addMember memberAddr metadataCid
      returnUint idx

    else if selector == SEL_GET_MEMBER
    then do
      index <- calldataload 4
      addr <- getMemberAddr index
      returnUint addr

    else if selector == SEL_GET_MEMBER_COUNT
    then do
      count <- getMemberCount
      returnUint count

    else if selector == SEL_IS_MEMBER
    then do
      addr <- calldataload 4
      member <- isMember addr
      returnBool member

    else evmRevert 0 0
