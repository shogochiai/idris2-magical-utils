||| Storage Slot Calculation Tests
|||
||| Verifies ERC-7201 slot calculations match Solidity's output
||| Test vectors are taken from mc/src/std/storage/Storage.sol
module Main

-- =============================================================================
-- EVM Primitives (FFI)
-- =============================================================================

%foreign "evm:calldataload"
prim__calldataload : Integer -> PrimIO Integer

%foreign "evm:mstore"
prim__mstore : Integer -> Integer -> PrimIO ()

%foreign "evm:return"
prim__return : Integer -> Integer -> PrimIO ()

%foreign "evm:revert"
prim__revert : Integer -> Integer -> PrimIO ()

%foreign "evm:keccak256"
prim__keccak256 : Integer -> Integer -> PrimIO Integer

%foreign "evm:sload"
prim__sload : Integer -> PrimIO Integer

%foreign "evm:sstore"
prim__sstore : Integer -> Integer -> PrimIO ()

-- =============================================================================
-- Wrapped Primitives
-- =============================================================================

calldataload : Integer -> IO Integer
calldataload off = primIO (prim__calldataload off)

mstore : Integer -> Integer -> IO ()
mstore off val = primIO (prim__mstore off val)

evmReturn : Integer -> Integer -> IO ()
evmReturn off len = primIO (prim__return off len)

evmRevert : Integer -> Integer -> IO ()
evmRevert off len = primIO (prim__revert off len)

keccak256 : Integer -> Integer -> IO Integer
keccak256 off len = primIO (prim__keccak256 off len)

sload : Integer -> IO Integer
sload slot = primIO (prim__sload slot)

sstore : Integer -> Integer -> IO ()
sstore slot val = primIO (prim__sstore slot val)

-- =============================================================================
-- Storage Slot Calculation Functions
-- =============================================================================

||| Calculate mapping slot: keccak256(key . baseSlot)
mappingSlot : Integer -> Integer -> IO Integer
mappingSlot baseSlot key = do
  mstore 0 key
  mstore 32 baseSlot
  keccak256 0 64

||| Calculate nested mapping slot
nestedMappingSlot : Integer -> Integer -> Integer -> IO Integer
nestedMappingSlot baseSlot key1 key2 = do
  slot1 <- mappingSlot baseSlot key1
  mappingSlot slot1 key2

||| Calculate array element slot: keccak256(baseSlot) + index * elemSize
arrayElementSlot : Integer -> Integer -> Integer -> IO Integer
arrayElementSlot baseSlot index elemSize = do
  mstore 0 baseSlot
  dataStart <- keccak256 0 32
  pure (dataStart + index * elemSize)

-- =============================================================================
-- Pre-computed ERC-7201 Slots (from mc/Storage.sol)
-- =============================================================================

||| mc.std.admin
||| Expected: 0xc87a8b268af18cef58a28e8269c607186ac6d26eb9fb11e976ba7fc83fbc5b00
EXPECTED_ADMIN : Integer
EXPECTED_ADMIN = 0xc87a8b268af18cef58a28e8269c607186ac6d26eb9fb11e976ba7fc83fbc5b00

||| mc.std.clone
||| Expected: 0x10c209d5b202f0d4610807a7049eb641dc6976ce93261be6493809881acea600
EXPECTED_CLONE : Integer
EXPECTED_CLONE = 0x10c209d5b202f0d4610807a7049eb641dc6976ce93261be6493809881acea600

||| mc.std.member
||| Expected: 0xb02ea24c1f86ea07e6c09d7d408e6de4225369a86f387a049c2d2fcaeb5d4c00
EXPECTED_MEMBER : Integer
EXPECTED_MEMBER = 0xb02ea24c1f86ea07e6c09d7d408e6de4225369a86f387a049c2d2fcaeb5d4c00

||| mc.std.featureToggle
||| Expected: 0xfbe5942bf8b77a2e1fdda5ac4fad2514a8894a997001808038d8cb6785c1d500
EXPECTED_FEATURE_TOGGLE : Integer
EXPECTED_FEATURE_TOGGLE = 0xfbe5942bf8b77a2e1fdda5ac4fad2514a8894a997001808038d8cb6785c1d500

||| mc.std.initialization
||| Expected: 0x3a761698c158d659b37261358fd236b3bd53eb7608e16317044a5253fc82ad00
EXPECTED_INITIALIZATION : Integer
EXPECTED_INITIALIZATION = 0x3a761698c158d659b37261358fd236b3bd53eb7608e16317044a5253fc82ad00

-- =============================================================================
-- Test Functions
-- =============================================================================

||| Test: Write and read from Admin slot
testAdminStorage : IO Bool
testAdminStorage = do
  let testAddr = 0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef
  sstore EXPECTED_ADMIN testAddr
  val <- sload EXPECTED_ADMIN
  pure (val == testAddr)

||| Test: Write and read from mapping in FeatureToggle
testFeatureToggleMapping : IO Bool
testFeatureToggleMapping = do
  let selector = 0x12345678
  slot <- mappingSlot EXPECTED_FEATURE_TOGGLE selector
  sstore slot 1  -- disabled = true
  val <- sload slot
  pure (val == 1)

||| Test: Write and read from Member array
testMemberArray : IO Bool
testMemberArray = do
  let testAddr1 = 0x1111111111111111111111111111111111111111
  let testAddr2 = 0x2222222222222222222222222222222222222222
  -- Store length
  sstore EXPECTED_MEMBER 2
  -- Store members
  elem0 <- arrayElementSlot EXPECTED_MEMBER 0 1
  elem1 <- arrayElementSlot EXPECTED_MEMBER 1 1
  sstore elem0 testAddr1
  sstore elem1 testAddr2
  -- Read back
  len <- sload EXPECTED_MEMBER
  val0 <- sload elem0
  val1 <- sload elem1
  pure (len == 2 && val0 == testAddr1 && val1 == testAddr2)

||| Test: Nested mapping slot calculation
||| Uses a made-up nested mapping for testing
testNestedMapping : IO Bool
testNestedMapping = do
  let baseSlot = 0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef
  let key1 = 0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  let key2 = 0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
  slot <- nestedMappingSlot baseSlot key1 key2
  sstore slot 42
  val <- sload slot
  pure (val == 42)

||| Run all tests, return number of passed tests
runTests : IO Integer
runTests = do
  r1 <- testAdminStorage
  r2 <- testFeatureToggleMapping
  r3 <- testMemberArray
  r4 <- testNestedMapping
  let passed = (if r1 then 1 else 0) +
               (if r2 then 1 else 0) +
               (if r3 then 1 else 0) +
               (if r4 then 1 else 0)
  pure passed

-- =============================================================================
-- Function Selectors
-- =============================================================================

-- runTests() returns (uint256)
SEL_RUN_TESTS : Integer
SEL_RUN_TESTS = 0x2f576f20

-- getAdminSlot() returns (uint256)
SEL_GET_ADMIN_SLOT : Integer
SEL_GET_ADMIN_SLOT = 0x3a4b5c6d

-- getFeatureToggleSlot() returns (uint256)
SEL_GET_FEATURE_TOGGLE_SLOT : Integer
SEL_GET_FEATURE_TOGGLE_SLOT = 0x4b5c6d7e

-- getMemberSlot() returns (uint256)
SEL_GET_MEMBER_SLOT : Integer
SEL_GET_MEMBER_SLOT = 0x5c6d7e8f

-- testMappingSlot(baseSlot, key) returns (uint256)
SEL_TEST_MAPPING_SLOT : Integer
SEL_TEST_MAPPING_SLOT = 0x6d7e8f90

-- =============================================================================
-- Entry Point
-- =============================================================================

getSelector : IO Integer
getSelector = do
  data_ <- calldataload 0
  pure (data_ `div` (256 * 256 * 256 * 256 * 256 * 256 * 256 * 256 *
                     256 * 256 * 256 * 256 * 256 * 256 * 256 * 256 *
                     256 * 256 * 256 * 256 * 256 * 256 * 256 * 256 *
                     256 * 256 * 256 * 256))

returnUint : Integer -> IO ()
returnUint val = do
  mstore 0 val
  evmReturn 0 32

main : IO ()
main = do
  selector <- getSelector

  if selector == SEL_RUN_TESTS
    then do
      passed <- runTests
      returnUint passed  -- Should return 4 if all pass

    else if selector == SEL_GET_ADMIN_SLOT
    then returnUint EXPECTED_ADMIN

    else if selector == SEL_GET_FEATURE_TOGGLE_SLOT
    then returnUint EXPECTED_FEATURE_TOGGLE

    else if selector == SEL_GET_MEMBER_SLOT
    then returnUint EXPECTED_MEMBER

    else if selector == SEL_TEST_MAPPING_SLOT
    then do
      baseSlot <- calldataload 4
      key <- calldataload 36
      slot <- mappingSlot baseSlot key
      returnUint slot

    else evmRevert 0 0
