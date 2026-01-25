||| EVM Storage Namespace Calculator
|||
||| Provides storage slot calculation following ERC-7201 specification:
||| `erc7201(id) = keccak256(keccak256(id) - 1) & ~0xff`
|||
||| Also provides Solidity-compatible storage layout calculations.
|||
||| All EVM primitives are imported from EVM.Primitives.
module EVM.Storage.Namespace

import public EVM.Primitives

-- =============================================================================
-- Solidity Storage Layout Calculations
-- =============================================================================

||| Calculate slot for a mapping entry: keccak256(key . baseSlot)
||| This follows Solidity's mapping storage layout
|||
||| @param baseSlot - The storage slot of the mapping variable
||| @param key - The mapping key (address, uint256, bytes32, etc.)
||| @return The storage slot for mapping[key]
export
mappingSlot : Integer -> Integer -> IO Integer
mappingSlot baseSlot key = do
  mstore 0 key
  mstore 32 baseSlot
  keccak256 0 64

||| Calculate slot for a nested mapping: mapping(K1 => mapping(K2 => V))
||| slot = keccak256(key2 . keccak256(key1 . baseSlot))
|||
||| @param baseSlot - The storage slot of the outer mapping
||| @param key1 - First level key
||| @param key2 - Second level key
||| @return The storage slot for mapping[key1][key2]
export
nestedMappingSlot : Integer -> Integer -> Integer -> IO Integer
nestedMappingSlot baseSlot key1 key2 = do
  slot1 <- mappingSlot baseSlot key1
  mappingSlot slot1 key2

||| Calculate slot for a dynamic array element
||| Array data starts at keccak256(baseSlot), element i is at that + i * elementSize
|||
||| @param baseSlot - The storage slot storing array.length
||| @param index - The array index
||| @param elementSize - Number of 32-byte slots per element
||| @return The storage slot for array[index]
export
arrayElementSlot : Integer -> Integer -> Integer -> IO Integer
arrayElementSlot baseSlot index elementSize = do
  mstore 0 baseSlot
  dataStart <- keccak256 0 32
  pure (dataStart + index * elementSize)

||| Get array length stored at baseSlot
export
arrayLength : Integer -> IO Integer
arrayLength baseSlot = sload baseSlot

||| Calculate slot for a struct field
||| Struct fields are stored contiguously: baseSlot + fieldOffset
|||
||| @param baseSlot - The storage slot of the struct
||| @param fieldOffset - The field's offset (0 for first field, 1 for second, etc.)
||| @return The storage slot for struct.field
export
structFieldSlot : Integer -> Integer -> Integer
structFieldSlot baseSlot fieldOffset = baseSlot + fieldOffset

-- =============================================================================
-- Type-Safe Storage Accessors
-- =============================================================================

||| Read a uint256 from storage
export
readUint : Integer -> IO Integer
readUint = sload

||| Write a uint256 to storage
export
writeUint : Integer -> Integer -> IO ()
writeUint = sstore

||| Read an address from storage (masking to 160 bits)
export
readAddress : Integer -> IO Integer
readAddress slot = do
  val <- sload slot
  pure (val `mod` 0x10000000000000000000000000000000000000000)  -- 2^160

||| Write an address to storage
export
writeAddress : Integer -> Integer -> IO ()
writeAddress = sstore

||| Read a bool from storage
export
readBool : Integer -> IO Bool
readBool slot = do
  val <- sload slot
  pure (val /= 0)

||| Write a bool to storage
export
writeBool : Integer -> Bool -> IO ()
writeBool slot b = sstore slot (if b then 1 else 0)

||| Read a bytes4 selector from storage
export
readSelector : Integer -> IO Integer
readSelector slot = do
  val <- sload slot
  pure (val `mod` 0x100000000)  -- 0xffffffff

-- =============================================================================
-- ERC-7201 Namespace Calculation
-- =============================================================================

||| ERC-7201 namespace slot calculation
||| Formula: keccak256(keccak256(id) - 1) & ~0xff
|||
||| This provides collision-resistant storage namespaces.
||| The final AND with ~0xff ensures the slot is 256-byte aligned,
||| allowing 256 consecutive slots per namespace.
|||
||| @param namespaceId - The namespace identifier string (hashed before use)
||| @return Base storage slot for the namespace
export
erc7201Slot : Integer -> IO Integer
erc7201Slot namespaceIdHash = do
  -- Store the hash at memory 0
  mstore 0 namespaceIdHash
  -- Calculate keccak256(hash)
  innerHash <- keccak256 0 32
  -- Subtract 1
  let adjusted = innerHash - 1
  -- Store and hash again
  mstore 0 adjusted
  outerHash <- keccak256 0 32
  -- AND with ~0xff (clear last byte)
  pure (outerHash `div` 256 * 256)
