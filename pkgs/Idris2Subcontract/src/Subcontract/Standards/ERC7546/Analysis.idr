||| ERC-7546 Contract State Analysis API
|||
||| Provides functions to analyze and query Dictionary contract state.
||| Used by lazy evm-lifecycle ask for comparing local vs deployed implementations.
|||
||| Reference: https://eips.ethereum.org/EIPS/eip-7546
module Subcontract.Standards.ERC7546.Analysis

import public EVM.Primitives
import Subcontract.Standards.ERC7546.Slots
import Subcontract.Standards.ERC7546.Dictionary

import Data.List
import Data.String

-- =============================================================================
-- Local FFI for extcodesize (not in EVM.Primitives yet)
-- =============================================================================

%foreign "evm:extcodesize"
prim__extcodesize : Integer -> PrimIO Integer

extcodesize : Integer -> IO Integer
extcodesize addr = primIO (prim__extcodesize addr)

-- =============================================================================
-- State Snapshot Types
-- =============================================================================

||| A snapshot of a single selector's implementation
public export
record ImplEntry where
  constructor MkImplEntry
  selector : Integer
  implAddr : Integer

public export
Show ImplEntry where
  show e = "0x" ++ show e.selector ++ " -> 0x" ++ show e.implAddr

||| A snapshot of Dictionary state at a point in time
public export
record DictionarySnapshot where
  constructor MkDictionarySnapshot
  ||| Dictionary contract address
  dictionaryAddr : Integer
  ||| Owner address
  ownerAddr : Integer
  ||| Block number when snapshot was taken
  blockNum : Integer
  ||| Known implementations (selector -> impl pairs)
  implementations : List ImplEntry

public export
Show DictionarySnapshot where
  show s = unlines
    [ "DictionarySnapshot @ block " ++ show s.blockNum
    , "  Dictionary: 0x" ++ show s.dictionaryAddr
    , "  Owner: 0x" ++ show s.ownerAddr
    , "  Implementations: " ++ show (length s.implementations)
    ]

-- =============================================================================
-- State Query Functions
-- =============================================================================

||| Query owner address from a Dictionary contract via STATICCALL
export
queryDictionaryOwner : Integer -> IO Integer
queryDictionaryOwner dictAddr = do
  -- Build calldata: owner() selector = 0x8da5cb5b
  mstore 0 SEL_OWNER
  -- STATICCALL to dictionary
  success <- staticcall 0xFFFFFFFF dictAddr 0 4 0 32
  if success /= 0
    then mload 0
    else pure 0

||| Query implementation for a selector via STATICCALL to Dictionary
export
queryImplementation : Integer -> Integer -> IO Integer
queryImplementation dictAddr sel = do
  -- Build calldata: getImplementation(bytes4) = 0xdc9cc645
  mstore 0 SEL_GET_IMPL
  mstore 4 sel
  -- STATICCALL to dictionary
  success <- staticcall 0xFFFFFFFF dictAddr 0 36 0 32
  if success /= 0
    then mload 0
    else pure 0

||| Query multiple implementations in batch
export
queryImplementations : Integer -> List Integer -> IO (List ImplEntry)
queryImplementations _ [] = pure []
queryImplementations dictAddr (sel :: rest) = do
  impl <- queryImplementation dictAddr sel
  let entry = MkImplEntry sel impl
  restEntries <- queryImplementations dictAddr rest
  pure (entry :: restEntries)

||| Check if an address has code (not a zombie reference)
export
addressHasCode : Integer -> IO Bool
addressHasCode addr = do
  size <- extcodesize addr
  pure (size > 0)

||| Check if implementation is valid (non-zero and has code)
export
isValidImplementation : Integer -> IO Bool
isValidImplementation impl = do
  if impl == 0
    then pure False
    else addressHasCode impl

-- =============================================================================
-- Snapshot Functions
-- =============================================================================

||| Take a snapshot of Dictionary state for given selectors
export
takeSnapshot : Integer -> List Integer -> IO DictionarySnapshot
takeSnapshot dictAddr selectors = do
  owner <- queryDictionaryOwner dictAddr
  blk <- number
  impls <- queryImplementations dictAddr selectors
  pure $ MkDictionarySnapshot
    { dictionaryAddr = dictAddr
    , ownerAddr = owner
    , blockNum = blk
    , implementations = impls
    }

-- =============================================================================
-- Analysis Functions
-- =============================================================================

||| Find implementation in snapshot by selector
lookupImpl : Integer -> List ImplEntry -> Maybe Integer
lookupImpl _ [] = Nothing
lookupImpl sel (e :: rest) =
  if e.selector == sel
    then Just e.implAddr
    else lookupImpl sel rest

||| Check if a specific selector's implementation matches expected
export
implementationMatches : DictionarySnapshot -> Integer -> Integer -> Bool
implementationMatches snapshot sel expectedImpl =
  case lookupImpl sel snapshot.implementations of
    Nothing => False
    Just impl => impl == expectedImpl

||| Get list of selectors with zero implementation (missing)
export
getMissingSelectors : DictionarySnapshot -> List Integer
getMissingSelectors snapshot =
  map selector $ filter (\e => e.implAddr == 0) snapshot.implementations

||| Filter helper for IO
filterM : (a -> IO Bool) -> List a -> IO (List a)
filterM _ [] = pure []
filterM f (x :: xs) = do
  keep <- f x
  rest <- filterM f xs
  pure $ if keep then x :: rest else rest

||| Validate all implementations have code (returns zombies)
export
validateImplementations : List ImplEntry -> IO (List ImplEntry)
validateImplementations entries = filterM checkZombie entries
  where
    checkZombie : ImplEntry -> IO Bool
    checkZombie e = do
      if e.implAddr == 0
        then pure False
        else do
          hasCode <- addressHasCode e.implAddr
          pure (not hasCode)  -- Return True if NO code (zombie)

-- =============================================================================
-- Standard Lifecycle Selectors
-- =============================================================================

||| All lifecycle-related selectors to monitor
public export
lifecycleSelectors : List Integer
lifecycleSelectors =
  [ 0x8129fc1c  -- initialize()
  , 0x3659cfe6  -- upgradeTo(address)
  , 0x4bb5274a  -- commitUpgrade()
  , 0x7a0ed627  -- rollbackUpgrade()
  , 0x2c4e722e  -- deprecate()
  , 0x8456cb59  -- pause()
  , 0x3f4ba83a  -- unpause()
  ]
