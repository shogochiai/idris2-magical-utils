||| Raw JSON-RPC handler for Redox path snapshot methods.
module Server.PathSnapshot.Handler

import Core.Core
import Core.Context
import Data.SortedMap
import Language.JSON
import Language.LSP.Message
import Language.LSP.Utils
import Server.Configuration
import Server.PathSnapshot.Protocol
import Server.PathSnapshot.Store

%default covering

versionEntry : (DocumentURI, (Int, String)) -> (String, JSON)
versionEntry (uri, (version, _)) = (show uri, JNumber (cast version))

documentEntry : (DocumentURI, (Int, String)) -> (String, JSON)
documentEntry (uri, (version, content)) =
  ( show uri
  , JObject [("version", JNumber (cast version)), ("content", JString content)]
  )

serverVersions : LSPConfiguration -> JSON
serverVersions conf = JObject (map versionEntry (Data.SortedMap.toList conf.virtualDocuments))

documentDigest : LSPConfiguration -> String
documentDigest conf = digestJSON (JObject (map documentEntry (Data.SortedMap.toList conf.virtualDocuments)))

toResponseError : SnapshotError -> ResponseError
toResponseError e =
  MkResponseError
    (Custom 4200)
    e.message
    (JObject [("redoxCode", JString e.code)])

runStore :
     Ref LSPConf LSPConfiguration
  => (PathSnapshotState -> Either SnapshotError (PathSnapshotState, JSON))
  -> Core (Either ResponseError JSON)
runStore action = do
  conf <- get LSPConf
  case action conf.pathSnapshots of
    Left e => pure (Left (toResponseError e))
    Right (nextState, result) => do
      update LSPConf ({ pathSnapshots := nextState })
      pure (Right result)

readStore :
     Ref LSPConf LSPConfiguration
  => (PathSnapshotState -> Either SnapshotError JSON)
  -> Core (Either ResponseError JSON)
readStore action = do
  conf <- get LSPConf
  case action conf.pathSnapshots of
    Left e => pure (Left (toResponseError e))
    Right result => pure (Right result)

paramsOrEmpty : Maybe JSON -> JSON
paramsOrEmpty Nothing = JObject []
paramsOrEmpty (Just params) = params

export
handlePathSnapshotRequest :
     Ref LSPConf LSPConfiguration
  => String
  -> Maybe JSON
  -> Core (Either ResponseError JSON)
handlePathSnapshotRequest method maybeParams = do
  conf <- get LSPConf
  let params = paramsOrEmpty maybeParams
  case method of
    "idris2/redox/capabilities" =>
      pure (Right redoxExperimentalCapability)
    "idris2/dumpPaths" =>
      runStore (\st => dumpPaths st (serverVersions conf) (documentDigest conf) params)
    "idris2/diffPaths" =>
      readStore
        (\st => do
          oldSnapshotId <-
            maybe (err "InvalidParams" "oldSnapshotId is required") Right
              (jsonStringField "oldSnapshotId" params)
          newSnapshotId <-
            maybe (err "InvalidParams" "newSnapshotId is required") Right
              (jsonStringField "newSnapshotId" params)
          diffSnapshots st oldSnapshotId newSnapshotId)
    "idris2/getPathSnapshot" =>
      readStore
        (\st => do
          snapshotId <-
            maybe (err "InvalidParams" "snapshotId is required") Right
              (jsonStringField "snapshotId" params)
          getSnapshot st snapshotId)
    "idris2/getCurrentVerified" =>
      readStore (\st => getCurrentVerified st params)
    "idris2/preparePathPromotion" =>
      runStore (\st => prepareSnapshotPromotion st params)
    "idris2/promotePathSnapshot" =>
      runStore (\st => promoteSnapshot st params)
    "idris2/rejectPathSnapshot" =>
      runStore (\st => rejectSnapshot st params)
    "idris2/exportPathSnapshotStore" =>
      readStore (\st => Right (exportSnapshotStore st))
    "idris2/importPathSnapshotStore" =>
      runStore (\_ => importSnapshotStore params)
    _ =>
      pure (Left (MkResponseError MethodNotFound "Method not implemented yet" JNull))
