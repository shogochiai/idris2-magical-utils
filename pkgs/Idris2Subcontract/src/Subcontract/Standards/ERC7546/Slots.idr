||| ERC-7546 Storage Slots
|||
||| Contains the standard storage slot constants for ERC-7546 UCS pattern.
|||
||| Reference: https://eips.ethereum.org/EIPS/eip-7546
module Subcontract.Standards.ERC7546.Slots

-- =============================================================================
-- ERC-7546 Storage Slots
-- =============================================================================

||| The storage slot for Dictionary address
||| keccak256("erc7546.proxy.dictionary") - 1
||| = 0x267691be3525af8a813d30db0c9e2bad08f63baecf6dceb85e2cf3676cff56f4
public export
DICTIONARY_SLOT : Integer
DICTIONARY_SLOT = 0x267691be3525af8a813d30db0c9e2bad08f63baecf6dceb85e2cf3676cff56f4

||| Function selector for getImplementation(bytes4)
||| keccak256("getImplementation(bytes4)")[:4]
public export
SEL_GET_IMPL : Integer
SEL_GET_IMPL = 0xdc9cc645

||| Function selector for setImplementation(bytes4,address)
public export
SEL_SET_IMPL : Integer
SEL_SET_IMPL = 0x2c3c3e4e

||| Function selector for owner()
public export
SEL_OWNER : Integer
SEL_OWNER = 0x8da5cb5b

||| Function selector for transferOwnership(address)
public export
SEL_TRANSFER : Integer
SEL_TRANSFER = 0xf2fde38b

||| Event: DictionaryUpgraded(address indexed dictionary)
||| keccak256("DictionaryUpgraded(address)")
public export
EVENT_DICTIONARY_UPGRADED : Integer
EVENT_DICTIONARY_UPGRADED = 0x23e44f489d6c7c6c9c5b5f5b5f5b5f5b5f5b5f5b5f5b5f5b5f5b5f5b5f5b5f5b
