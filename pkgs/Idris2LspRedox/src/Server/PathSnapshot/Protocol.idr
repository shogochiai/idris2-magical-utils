||| Redox path snapshot protocol constants and JSON helpers.
module Server.PathSnapshot.Protocol

import Data.List
import Language.JSON

%default total

public export
protocolVersion : String
protocolVersion = "1.1"

public export
schemaVersion : String
schemaVersion = "1.1"

public export
canonicalEncoding : String
canonicalEncoding = "redox-canonical-json-1"

public export
redoxPathSnapshotMethods : List String
redoxPathSnapshotMethods =
  [ "idris2/redox/capabilities"
  , "idris2/dumpPaths"
  , "idris2/diffPaths"
  , "idris2/getPathSnapshot"
  , "idris2/getCurrentVerified"
  , "idris2/preparePathPromotion"
  , "idris2/promotePathSnapshot"
  , "idris2/rejectPathSnapshot"
  , "idris2/exportPathSnapshotStore"
  , "idris2/importPathSnapshotStore"
  ]

export
isRedoxPathSnapshotMethod : String -> Bool
isRedoxPathSnapshotMethod method = method `elem` redoxPathSnapshotMethods

export
redoxExperimentalCapability : JSON
redoxExperimentalCapability =
  JObject
    [ ( "redoxPathSnapshots"
      , JObject
          [ ("protocolVersion", JString protocolVersion)
          , ("schemaVersion", JString schemaVersion)
          , ("methods", JArray (map JString redoxPathSnapshotMethods))
          , ("canonicalEncoding", JString canonicalEncoding)
          ]
      )
    ]
