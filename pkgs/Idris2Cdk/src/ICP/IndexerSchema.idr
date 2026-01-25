||| Indexer Schema and Mapping Layer
|||
||| Provides SQL-like queries over B+Tree storage.
|||
||| Usage:
|||   -- Define schema
|||   event <- mkEvent chainId blockNum logIdx address topic data
|||
|||   -- Insert with auto-indexing
|||   insertEvent event
|||
|||   -- Query
|||   results <- query $ byAddress "0x123" .&. inBlockRange 1000 2000
module ICP.IndexerSchema

import ICP.StableBTree
import Data.List
import Data.List1
import Data.Maybe
import Data.String

%default covering

--------------------------------------------------------------------------------
-- Core Types
--------------------------------------------------------------------------------

||| Chain identifier
public export
ChainId : Type
ChainId = Nat

||| Block number
public export
BlockNum : Type
BlockNum = Nat

||| Log index within block
public export
LogIndex : Type
LogIndex = Nat

||| Ethereum address (hex string without 0x)
public export
Address : Type
Address = String

||| Event topic (bytes32 as hex)
public export
Topic : Type
Topic = String

||| Indexed Event - the core data structure
public export
record IndexedEvent where
  constructor MkIndexedEvent
  chainId   : ChainId
  blockNum  : BlockNum
  logIndex  : LogIndex
  address   : Address      -- Contract address
  topic0    : Topic        -- Event signature
  topic1    : Maybe Topic  -- Indexed param 1
  topic2    : Maybe Topic  -- Indexed param 2
  topic3    : Maybe Topic  -- Indexed param 3
  data_     : String       -- Non-indexed data (hex)
  txHash    : String       -- Transaction hash

public export
Show IndexedEvent where
  show e = "Event{chain=" ++ show e.chainId ++
           ",block=" ++ show e.blockNum ++
           ",log=" ++ show e.logIndex ++
           ",addr=" ++ e.address ++
           ",topic=" ++ e.topic0 ++ "}"

--------------------------------------------------------------------------------
-- Key Generation for Indexes
--------------------------------------------------------------------------------

||| Separator for composite keys
keySep : String
keySep = ":"

||| Pad number to fixed width for lexicographic ordering
padNum : Nat -> Nat -> String
padNum width n =
  let s = show n
      padding = pack (replicate (minus width (length s)) '0')
  in padding ++ s

||| Primary key: unique identifier for event
public export
primaryKey : IndexedEvent -> String
primaryKey e = "evt" ++ keySep ++
               padNum 4 e.chainId ++ keySep ++
               padNum 12 e.blockNum ++ keySep ++
               padNum 6 e.logIndex

||| Index key: by address
public export
indexByAddress : IndexedEvent -> String
indexByAddress e = "idx:addr" ++ keySep ++
                   e.address ++ keySep ++
                   padNum 4 e.chainId ++ keySep ++
                   padNum 12 e.blockNum ++ keySep ++
                   padNum 6 e.logIndex

||| Index key: by topic0 (event signature)
public export
indexByTopic : IndexedEvent -> String
indexByTopic e = "idx:topic" ++ keySep ++
                 e.topic0 ++ keySep ++
                 padNum 4 e.chainId ++ keySep ++
                 padNum 12 e.blockNum ++ keySep ++
                 padNum 6 e.logIndex

||| Index key: by chain + block (for range queries)
public export
indexByBlock : IndexedEvent -> String
indexByBlock e = "idx:block" ++ keySep ++
                 padNum 4 e.chainId ++ keySep ++
                 padNum 12 e.blockNum ++ keySep ++
                 padNum 6 e.logIndex

||| Index key: by address + topic (common query pattern)
public export
indexByAddressTopic : IndexedEvent -> String
indexByAddressTopic e = "idx:addr_topic" ++ keySep ++
                        e.address ++ keySep ++
                        e.topic0 ++ keySep ++
                        padNum 4 e.chainId ++ keySep ++
                        padNum 12 e.blockNum ++ keySep ++
                        padNum 6 e.logIndex

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

||| Serialize event to storage format
||| Format: chainId|blockNum|logIndex|address|topic0|topic1|topic2|topic3|data|txHash
public export
serializeEvent : IndexedEvent -> String
serializeEvent e =
  show e.chainId ++ "|" ++
  show e.blockNum ++ "|" ++
  show e.logIndex ++ "|" ++
  e.address ++ "|" ++
  e.topic0 ++ "|" ++
  fromMaybe "" e.topic1 ++ "|" ++
  fromMaybe "" e.topic2 ++ "|" ++
  fromMaybe "" e.topic3 ++ "|" ++
  e.data_ ++ "|" ++
  e.txHash

||| Parse event from storage format
public export
deserializeEvent : String -> Maybe IndexedEvent
deserializeEvent s =
  let parts = forget $ split (== '|') s
  in case parts of
    [cid, bn, li, addr, t0, t1, t2, t3, d, tx] =>
      Just $ MkIndexedEvent
        { chainId = cast cid
        , blockNum = cast bn
        , logIndex = cast li
        , address = addr
        , topic0 = t0
        , topic1 = if t1 == "" then Nothing else Just t1
        , topic2 = if t2 == "" then Nothing else Just t2
        , topic3 = if t3 == "" then Nothing else Just t3
        , data_ = d
        , txHash = tx
        }
    _ => Nothing

--------------------------------------------------------------------------------
-- Insert with Auto-Indexing
--------------------------------------------------------------------------------

||| Insert event with all secondary indexes
public export
insertEvent : IndexedEvent -> IO BTResult
insertEvent e = do
  let serialized = serializeEvent e

  -- Primary store
  r1 <- btPut (primaryKey e) serialized
  case r1 of
    BTSuccess => do
      -- Secondary indexes (store just the primary key as reference)
      let pk = primaryKey e
      _ <- btPut (indexByAddress e) pk
      _ <- btPut (indexByTopic e) pk
      _ <- btPut (indexByBlock e) pk
      _ <- btPut (indexByAddressTopic e) pk
      pure BTSuccess
    err => pure err

||| Batch insert events
public export
insertEvents : List IndexedEvent -> IO (Nat, Nat)  -- (success, failed)
insertEvents events = go events 0 0
  where
    go : List IndexedEvent -> Nat -> Nat -> IO (Nat, Nat)
    go [] succ fail = pure (succ, fail)
    go (e :: es) succ fail = do
      result <- insertEvent e
      case result of
        BTSuccess => go es (S succ) fail
        _ => go es succ (S fail)

--------------------------------------------------------------------------------
-- Query Builder
--------------------------------------------------------------------------------

||| Query filter
public export
data Filter : Type where
  ||| Filter by address
  ByAddress : Address -> Filter
  ||| Filter by topic0 (event signature)
  ByTopic : Topic -> Filter
  ||| Filter by address AND topic
  ByAddressTopic : Address -> Topic -> Filter
  ||| Filter by block range
  InBlockRange : ChainId -> BlockNum -> BlockNum -> Filter
  ||| Filter by chain
  ByChain : ChainId -> Filter

public export
Show Filter where
  show (ByAddress a) = "address=" ++ a
  show (ByTopic t) = "topic=" ++ t
  show (ByAddressTopic a t) = "address=" ++ a ++ ",topic=" ++ t
  show (InBlockRange c from to) = "chain=" ++ show c ++
                                   ",block=" ++ show from ++ "-" ++ show to
  show (ByChain c) = "chain=" ++ show c

||| Convert filter to prefix for scan
public export
filterToPrefix : Filter -> String
filterToPrefix (ByAddress addr) = "idx:addr:" ++ addr ++ ":"
filterToPrefix (ByTopic topic) = "idx:topic:" ++ topic ++ ":"
filterToPrefix (ByAddressTopic addr topic) = "idx:addr_topic:" ++ addr ++ ":" ++ topic ++ ":"
filterToPrefix (InBlockRange chainId fromBlock _) =
  "idx:block:" ++ padNum 4 chainId ++ ":" ++ padNum 12 fromBlock
filterToPrefix (ByChain chainId) = "idx:block:" ++ padNum 4 chainId ++ ":"

--------------------------------------------------------------------------------
-- Query Execution
--------------------------------------------------------------------------------

||| Scan keys by prefix (stub - real impl uses FFI to C BTree)
export
scanPrefix : String -> Nat -> IO (List String)
scanPrefix _ _ = pure []

||| Fetch a single event by primary key
fetchEvent : String -> IO (Maybe IndexedEvent)
fetchEvent pk = do
  mval <- btGet pk 4096
  pure $ mval >>= deserializeEvent

||| Execute query and return matching events
public export
query : Filter -> Nat -> IO (List IndexedEvent)
query flt maxResults =
  scanPrefix (filterToPrefix flt) maxResults >>= \pks =>
    traverse fetchEvent pks >>= \events =>
      pure (catMaybes events)

--------------------------------------------------------------------------------
-- Convenience Query Functions
--------------------------------------------------------------------------------

||| Get events by contract address
public export
getEventsByAddress : Address -> Nat -> IO (List IndexedEvent)
getEventsByAddress addr limit = query (ByAddress addr) limit

||| Get events by topic (event signature)
public export
getEventsByTopic : Topic -> Nat -> IO (List IndexedEvent)
getEventsByTopic topic limit = query (ByTopic topic) limit

||| Get events by address and topic
public export
getEventsByAddressTopic : Address -> Topic -> Nat -> IO (List IndexedEvent)
getEventsByAddressTopic addr topic limit = query (ByAddressTopic addr topic) limit

||| Get events in block range
public export
getEventsInBlockRange : ChainId -> BlockNum -> BlockNum -> Nat -> IO (List IndexedEvent)
getEventsInBlockRange chainId fromBlock toBlock limit =
  query (InBlockRange chainId fromBlock toBlock) limit

||| Get single event by primary key components
public export
getEvent : ChainId -> BlockNum -> LogIndex -> IO (Maybe IndexedEvent)
getEvent chainId blockNum logIdx = do
  let pk = "evt:" ++ padNum 4 chainId ++ ":" ++
           padNum 12 blockNum ++ ":" ++ padNum 6 logIdx
  mval <- btGet pk 4096
  case mval of
    Nothing => pure Nothing
    Just val => pure (deserializeEvent val)

--------------------------------------------------------------------------------
-- Cursor Management (for incremental sync)
--------------------------------------------------------------------------------

||| Get last synced block for a chain
public export
getLastSyncedBlock : ChainId -> IO (Maybe BlockNum)
getLastSyncedBlock chainId = do
  mval <- btGet ("cursor:" ++ show chainId) 32
  case mval of
    Nothing => pure Nothing
    Just val => pure (Just (cast val))

||| Set last synced block for a chain
public export
setLastSyncedBlock : ChainId -> BlockNum -> IO BTResult
setLastSyncedBlock chainId blockNum =
  btPut ("cursor:" ++ show chainId) (show blockNum)

--------------------------------------------------------------------------------
-- Stats
--------------------------------------------------------------------------------

||| Get total event count
public export
getEventCount : IO Nat
getEventCount = btCount

||| Get tree height (indicator of data volume)
public export
getTreeHeight : IO Nat
getTreeHeight = btHeight
