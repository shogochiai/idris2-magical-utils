||| In-memory Redox path snapshot lifecycle store.
|||
||| The store deliberately keeps JSON payload bytes as the authority at the
||| LSP boundary. Typed Idris records can be layered on top, but the custom RPC
||| contract is a canonical finite JSON object with stable field ordering.
module Server.PathSnapshot.Store

import Data.List
import Data.Maybe
import Data.SortedMap
import Data.String
import Language.JSON
import Language.LSP.Utils
import Server.PathSnapshot.Protocol

%default total

public export
record SnapshotError where
  constructor MkSnapshotError
  code : String
  message : String

public export
record PathSnapshotState where
  constructor MkPathSnapshotState
  payloads : SortedMap String JSON
  life : SortedMap String String
  currentVerified : SortedMap String String
  currentWorking : SortedMap String String
  prepared : SortedMap String JSON
  witnesses : SortedMap String JSON

export
emptyPathSnapshotState : PathSnapshotState
emptyPathSnapshotState =
  MkPathSnapshotState
    { payloads = empty
    , life = empty
    , currentVerified = empty
    , currentWorking = empty
    , prepared = empty
    , witnesses = empty
    }

export
err : String -> String -> Either SnapshotError a
err code msg = Left (MkSnapshotError code msg)

export
jsonField : String -> JSON -> Maybe JSON
jsonField key (JObject fields) = lookup key fields
jsonField _ _ = Nothing

export
jsonStringField : String -> JSON -> Maybe String
jsonStringField key json = do
  JString value <- jsonField key json
    | _ => Nothing
  pure value

jsonArrayField : String -> JSON -> Maybe (List JSON)
jsonArrayField key json = do
  JArray values <- jsonField key json
    | _ => Nothing
  pure values

fieldJSON : String -> JSON -> JSON -> JSON
fieldJSON key fallback json = fromMaybe fallback (jsonField key json)

fieldString : String -> String -> JSON -> String
fieldString key fallback json = fromMaybe fallback (jsonStringField key json)

maybeToList : Maybe a -> List a
maybeToList Nothing = []
maybeToList (Just x) = [x]

mapMaybeLocal : (a -> Maybe b) -> List a -> List b
mapMaybeLocal f xs = concat (map (maybeToList . f) xs)

jsonStringValue : JSON -> Maybe String
jsonStringValue (JString value) = Just value
jsonStringValue _ = Nothing

jsonStringArray : JSON -> List String
jsonStringArray (JArray values) = mapMaybeLocal jsonStringValue values
jsonStringArray _ = []

jsonStringArrayField : String -> JSON -> Maybe (List String)
jsonStringArrayField key json = do
  values <- jsonArrayField key json
  pure (mapMaybeLocal jsonStringValue values)

stringsJSON : List String -> JSON
stringsJSON values = JArray (map JString values)

withoutField : String -> List (String, JSON) -> List (String, JSON)
withoutField key fields = filter (\(k, _) => k /= key) fields

setField : String -> JSON -> JSON -> JSON
setField key value (JObject fields) = JObject ((key, value) :: withoutField key fields)
setField key value _ = JObject [(key, value)]

boolFieldTrue : String -> JSON -> Bool
boolFieldTrue key json =
  case jsonField key json of
    Just (JBoolean True) => True
    Just (JNumber n) => n == 1
    _ => False

hashChars : Integer -> List Char -> Integer
hashChars acc [] = acc
hashChars acc (c :: cs) =
  hashChars (((acc * 131) + cast (ord c)) `mod` 1000000007) cs

export
stableDigest : String -> String
stableDigest value =
  "len-" ++ show (length value) ++ "-h-" ++ show (hashChars 5381 (unpack value))

export
digestJSON : JSON -> String
digestJSON = stableDigest . stringify

mapJSON : SortedMap String JSON -> JSON
mapJSON values = JObject (Data.SortedMap.toList values)

stringMapJSON : SortedMap String String -> JSON
stringMapJSON values = JObject (map (\(k, v) => (k, JString v)) (Data.SortedMap.toList values))

deleteJSONMap : String -> SortedMap String JSON -> SortedMap String JSON
deleteJSONMap = Data.SortedMap.delete

deleteStringMap : String -> SortedMap String String -> SortedMap String String
deleteStringMap = Data.SortedMap.delete

jsonMapFromObject : JSON -> SortedMap String JSON
jsonMapFromObject (JObject fields) = foldl (\acc, (k, v) => insert k v acc) empty fields
jsonMapFromObject _ = empty

stringMapFromObject : JSON -> SortedMap String String
stringMapFromObject (JObject fields) =
  foldl
    (\acc, (k, v) =>
       case v of
         JString s => insert k s acc
         _ => acc)
    empty
    fields
stringMapFromObject _ = empty

export
stateToJSON : PathSnapshotState -> JSON
stateToJSON state =
  JObject
    [ ("protocolVersion", JString protocolVersion)
    , ("schemaVersion", JString schemaVersion)
    , ("payloads", mapJSON state.payloads)
    , ("life", stringMapJSON state.life)
    , ("currentVerified", stringMapJSON state.currentVerified)
    , ("currentWorking", stringMapJSON state.currentWorking)
    , ("prepared", mapJSON state.prepared)
    , ("witnesses", mapJSON state.witnesses)
    ]

export
stateFromJSON : JSON -> Either SnapshotError PathSnapshotState
stateFromJSON json = do
  let payloads = jsonMapFromObject (fieldJSON "payloads" (JObject []) json)
  let life = stringMapFromObject (fieldJSON "life" (JObject []) json)
  let currentVerified = stringMapFromObject (fieldJSON "currentVerified" (JObject []) json)
  let currentWorking = stringMapFromObject (fieldJSON "currentWorking" (JObject []) json)
  let prepared = jsonMapFromObject (fieldJSON "prepared" (JObject []) json)
  let witnesses = jsonMapFromObject (fieldJSON "witnesses" (JObject []) json)
  pure (MkPathSnapshotState payloads life currentVerified currentWorking prepared witnesses)

pathCount : JSON -> Nat
pathCount (JArray xs) = length xs
pathCount _ = 0

pathLineage : JSON -> Maybe String
pathLineage fact =
  jsonStringField "lineageId" fact
    <|> jsonStringField "lineage" fact
    <|> jsonStringField "pathLineageId" fact

pathRevision : JSON -> String
pathRevision fact =
  fieldString "revisionId" (digestJSON fact) fact

pathSpec : JSON -> Maybe String
pathSpec fact =
  jsonStringField "specId" fact

pathModule : JSON -> Maybe String
pathModule fact =
  jsonStringField "moduleId" fact <|> jsonStringField "module" fact

pathRange : JSON -> Maybe JSON
pathRange fact =
  jsonField "sourceRange" fact <|> jsonField "range" fact

insertPath : SortedMap String JSON -> JSON -> SortedMap String JSON
insertPath acc fact =
  case pathLineage fact of
    Just lineage => insert lineage fact acc
    Nothing => acc

pathMapFromJSON : JSON -> SortedMap String JSON
pathMapFromJSON (JArray facts) = foldl insertPath empty facts
pathMapFromJSON _ = empty

pathsFromPayload : JSON -> SortedMap String JSON
pathsFromPayload payload = pathMapFromJSON (fieldJSON "paths" (JArray []) payload)

payloadStatus : JSON -> String
payloadStatus = fieldString "elabStatus" "Failed"

projectOfPayload : JSON -> String
projectOfPayload = fieldString "projectId" "default"

projectIdentityOfParams : JSON -> JSON
projectIdentityOfParams params = fieldJSON "projectIdentity" (JObject []) params

projectOfParams : JSON -> String
projectOfParams params =
  case jsonStringField "projectId" params of
    Just project => project
    Nothing =>
      case jsonField "projectIdentity" params of
        Just identity@(JObject _) => "redox-project-" ++ digestJSON identity
        _ => "default"

specRegistryDigestOfParams : JSON -> String
specRegistryDigestOfParams params =
  case jsonStringField "specRegistryDigest" params of
    Just digest => digest
    Nothing =>
      case jsonField "projectIdentity" params of
        Just identity => fieldString "specRegistryDigest" "unbound" identity
        Nothing => "unbound"

arrayHasValues : JSON -> Bool
arrayHasValues (JArray []) = False
arrayHasValues (JArray _) = True
arrayHasValues _ = False

impactPrecisionOfParams : String -> JSON -> String
impactPrecisionOfParams status params =
  case jsonStringField "impactPrecision" params of
    Just precision => precision
    Nothing =>
      if status == "Complete"
         then "Exact"
         else if arrayHasValues (fieldJSON "invalidDefinitions" (JArray []) params)
                 then "DefinitionFallback"
                 else if arrayHasValues (fieldJSON "dirtyModules" (JArray []) params)
                         then "ModuleFallback"
                         else "ProjectFallback"

payloadImpactPrecision : JSON -> String
payloadImpactPrecision payload =
  fieldString "impactPrecision"
    (if payloadStatus payload == "Complete" then "Exact" else "ProjectFallback")
    payload

worstImpactPrecision : String -> String -> String
worstImpactPrecision left right =
  if left == "ProjectFallback" || right == "ProjectFallback"
     then "ProjectFallback"
     else if left == "ModuleFallback" || right == "ModuleFallback"
             then "ModuleFallback"
             else if left == "DefinitionFallback" || right == "DefinitionFallback"
                     then "DefinitionFallback"
                     else "Exact"

addUnique : String -> List String -> List String
addUnique x xs = if (x `elem` xs) then xs else x :: xs

addFactSpec : JSON -> List String -> List String
addFactSpec fact acc =
  case pathSpec fact of
    Just spec => addUnique spec acc
    Nothing => acc

specsOfFacts : List JSON -> List String
specsOfFacts facts = foldr addFactSpec [] facts

factsObject : List (String, JSON) -> JSON
factsObject facts = JObject facts

lineagesArray : List String -> JSON
lineagesArray keys = JArray (map JString keys)

specsArray : List String -> JSON
specsArray specs = JArray (map JString (reverse specs))

mapPairs : SortedMap String JSON -> List (String, JSON)
mapPairs = Data.SortedMap.toList

mergeMaps : SortedMap String JSON -> SortedMap String JSON -> SortedMap String JSON
mergeMaps left right = foldl (\acc, (k, v) => insert k v acc) left (Data.SortedMap.toList right)

mapValues : List (String, JSON) -> List JSON
mapValues = map snd

sameRevision : JSON -> JSON -> Bool
sameRevision old new = pathRevision old == pathRevision new

relocated : JSON -> JSON -> Bool
relocated old new = (pathModule old /= pathModule new) || (pathRange old /= pathRange new)

modifiedPair : SortedMap String JSON -> (String, JSON) -> Maybe (String, JSON, JSON)
modifiedPair newMap (lineage, oldFact) =
  case lookup lineage newMap of
    Just newFact =>
      if sameRevision oldFact newFact
         then Nothing
         else Just (lineage, oldFact, newFact)
    Nothing => Nothing

unchangedLineage : SortedMap String JSON -> (String, JSON) -> Maybe String
unchangedLineage newMap (lineage, oldFact) =
  case lookup lineage newMap of
    Just newFact =>
      if sameRevision oldFact newFact
         then Just lineage
         else Nothing
    Nothing => Nothing

relocatedLineage : SortedMap String JSON -> (String, JSON) -> Maybe String
relocatedLineage newMap (lineage, oldFact) =
  case lookup lineage newMap of
    Just newFact =>
      if sameRevision oldFact newFact && relocated oldFact newFact
         then Just lineage
         else Nothing
    Nothing => Nothing

diffCompleteness : JSON -> JSON -> Bool -> String
diffCompleteness oldPayload newPayload complete =
  if complete then "Complete"
  else if payloadStatus oldPayload == "Failed" || payloadStatus newPayload == "Failed"
          then "Failed"
          else "Partial"

computeDiffJSON : JSON -> JSON -> JSON
computeDiffJSON oldPayload newPayload =
  let oldPaths = pathsFromPayload oldPayload
      newPaths = pathsFromPayload newPayload
      oldPairs = mapPairs oldPaths
      newPairs = mapPairs newPaths
      removedPairs = filter (\(lineage, _) => isNothing (lookup lineage newPaths)) oldPairs
      addedPairs = filter (\(lineage, _) => isNothing (lookup lineage oldPaths)) newPairs
      modifiedTriples = mapMaybeLocal (modifiedPair newPaths) oldPairs
      modifiedBeforePairs = map (\(lineage, oldFact, _) => (lineage, oldFact)) modifiedTriples
      modifiedAfterPairs = map (\(lineage, _, newFact) => (lineage, newFact)) modifiedTriples
      unchanged = mapMaybeLocal (unchangedLineage newPaths) oldPairs
      moved = mapMaybeLocal (relocatedLineage newPaths) oldPairs
      complete = payloadStatus oldPayload == "Complete" && payloadStatus newPayload == "Complete"
      fallbackMap = if complete then empty else mergeMaps oldPaths newPaths
      directSpecs =
        specsOfFacts
          (mapValues removedPairs
            ++ mapValues addedPairs
            ++ mapValues modifiedBeforePairs
            ++ mapValues modifiedAfterPairs)
      fallbackSpecs = specsOfFacts (mapValues (mapPairs fallbackMap))
      impactClosed = foldr addUnique directSpecs fallbackSpecs
      completeness = diffCompleteness oldPayload newPayload complete
      precision = worstImpactPrecision (payloadImpactPrecision oldPayload) (payloadImpactPrecision newPayload)
      directImpactJSON = specsArray directSpecs
      closedImpactJSON = specsArray impactClosed
      impactDigest = digestJSON closedImpactJSON
      baseDiff =
        JObject
          [ ("removedFacts", factsObject removedPairs)
          , ("addedFacts", factsObject addedPairs)
          , ("modifiedBefore", factsObject modifiedBeforePairs)
          , ("modifiedAfter", factsObject modifiedAfterPairs)
          , ("unchangedLineages", lineagesArray unchanged)
          , ("relocatedLineages", lineagesArray moved)
          , ("fallbackFacts", factsObject (mapPairs fallbackMap))
          , ("impactDirect", directImpactJSON)
          , ("impactClosed", closedImpactJSON)
          , ("pathImpactDirect", directImpactJSON)
          , ("pathImpactClosed", closedImpactJSON)
          , ("pathImpactDigest", JString impactDigest)
          , ("impactKind", JString "compiler-path")
          , ("impactPrecision", JString precision)
          , ("soundness", JString "over-approximation")
          , ("completeness", JString completeness)
          ]
   in case baseDiff of
        JObject fields =>
          let diffDigest = digestJSON baseDiff in
              JObject
                (fields ++
                  [ ("diffDigest", JString diffDigest)
                  , ("pathDiffDigest", JString diffDigest)
                  ])
        _ => baseDiff

export
diffSnapshots : PathSnapshotState -> String -> String -> Either SnapshotError JSON
diffSnapshots state oldSnapshotId newSnapshotId = do
  oldPayload <- maybe (err "SnapshotNotFound" "oldSnapshotId was not found") Right (lookup oldSnapshotId state.payloads)
  newPayload <- maybe (err "SnapshotNotFound" "newSnapshotId was not found") Right (lookup newSnapshotId state.payloads)
  pure (computeDiffJSON oldPayload newPayload)

makePayload : String -> JSON -> String -> JSON -> JSON
makePayload sourceDigest serverVersions documentsDigest params =
  let project = projectOfParams params
      paths = fieldJSON "paths" (JArray []) params
      status =
        fieldString "elabStatus"
          (case jsonField "paths" params of
             Just _ => "Complete"
             Nothing => "Failed")
          params
      precision = impactPrecisionOfParams status params
      parent = fieldJSON "parentVerified" JNull params
      identity = projectIdentityOfParams params
      registryDigest = specRegistryDigestOfParams params
   in JObject
        [ ("projectId", JString project)
        , ("projectIdentity", identity)
        , ("specRegistryDigest", JString registryDigest)
        , ("versions", serverVersions)
        , ("sourceDigest", JString sourceDigest)
        , ("sourceDocumentDigest", JString documentsDigest)
        , ("sourceTreeDigest", fieldJSON "sourceTreeDigest" (JString documentsDigest) params)
        , ("compilerVersion", fieldJSON "compilerVersion" (JString "unknown") params)
        , ("harnessVersion", fieldJSON "harnessVersion" (JString "idris2-lsp-redox-1.1") params)
        , ("schemaVersion", fieldJSON "requestedSchemaVersion" (JString schemaVersion) params)
        , ("elabStatus", JString status)
        , ("impactKind", JString "compiler-path")
        , ("impactPrecision", JString precision)
        , ("soundness", JString "over-approximation")
        , ("paths", paths)
        , ("invalidDefinitions", fieldJSON "invalidDefinitions" (JArray []) params)
        , ("dirtyModules", fieldJSON "dirtyModules" (JArray []) params)
        , ("diagnosticsDigest", fieldJSON "diagnosticsDigest" (JString (digestJSON (fieldJSON "diagnostics" (JArray []) params))) params)
        , ("parentVerified", parent)
        , ("origin", JString "idris2-lsp-redox")
        ]

export
dumpPaths :
     PathSnapshotState
  -> JSON
  -> String
  -> JSON
  -> Either SnapshotError (PathSnapshotState, JSON)
dumpPaths state serverVersions documentsDigest params = do
  case jsonField "expectedVersions" params of
    Just expected =>
      if expected == serverVersions
         then Right ()
         else err "StaleVersion" "expectedVersions did not match the synchronized server versions"
    Nothing => Right ()
  let project = projectOfParams params
  let parent = jsonStringField "parentVerified" params <|> lookup project state.currentVerified
  case parent of
    Just sid =>
      case lookup sid state.payloads of
        Just _ => Right ()
        Nothing => err "SnapshotNotFound" "parentVerified snapshot was not found"
    Nothing => Right ()
  let paramsWithParent =
        case parent of
          Just sid => setField "parentVerified" (JString sid) params
          Nothing => params
  let payload = makePayload documentsDigest serverVersions documentsDigest paramsWithParent
  let keyDigest = digestJSON payload
  let snapshotId = "redox-snapshot-" ++ keyDigest
  case lookup snapshotId state.payloads of
    Just previous =>
      if previous == payload
         then Right ()
         else err "DigestCollision" "snapshot digest collision with distinct canonical payload"
    Nothing => Right ()
  let lifeAfterOldWorking =
        case lookup project state.currentWorking of
          Just oldWorking =>
            if oldWorking == snapshotId
               then state.life
               else insert oldWorking "Rejected" state.life
          Nothing => state.life
  let nextState =
        ({ payloads := insert snapshotId payload state.payloads
         , life := insert snapshotId "Working" lifeAfterOldWorking
         , currentWorking := insert project snapshotId state.currentWorking
         } state)
  let result =
        JObject
          [ ("snapshotId", JString snapshotId)
          , ("keyDigest", JString keyDigest)
          , ("projectId", JString project)
          , ("specRegistryDigest", fieldJSON "specRegistryDigest" (JString "unbound") payload)
          , ("impactKind", fieldJSON "impactKind" (JString "compiler-path") payload)
          , ("impactPrecision", fieldJSON "impactPrecision" (JString "ProjectFallback") payload)
          , ("soundness", fieldJSON "soundness" (JString "over-approximation") payload)
          , ("elabStatus", fieldJSON "elabStatus" (JString "Failed") payload)
          , ("pathCount", JNumber (cast (pathCount (fieldJSON "paths" (JArray []) payload))))
          , ("invalidDefinitions", fieldJSON "invalidDefinitions" (JArray []) payload)
          , ("dirtyModules", fieldJSON "dirtyModules" (JArray []) payload)
          , ("diagnosticsDigest", fieldJSON "diagnosticsDigest" (JString "") payload)
          , ("parentVerified", fieldJSON "parentVerified" JNull payload)
          ]
  pure (nextState, result)

export
getSnapshot : PathSnapshotState -> String -> Either SnapshotError JSON
getSnapshot state snapshotId = do
  payload <- maybe (err "SnapshotNotFound" "snapshotId was not found") Right (lookup snapshotId state.payloads)
  let snapshotLife = fromMaybe "Archived" (lookup snapshotId state.life)
  pure (JObject [("snapshotId", JString snapshotId), ("life", JString snapshotLife), ("payload", payload)])

export
getCurrentVerified : PathSnapshotState -> JSON -> Either SnapshotError JSON
getCurrentVerified state params =
  let project = projectOfParams params in
  case lookup project state.currentVerified of
    Just snapshotId =>
      pure
        (JObject
          [ ("projectId", JString project)
          , ("snapshotId", JString snapshotId)
          , ("life", JString (fromMaybe "Archived" (lookup snapshotId state.life)))
          ])
    Nothing =>
      pure
        (JObject
          [ ("projectId", JString project)
          , ("snapshotId", JNull)
          , ("life", JString "Missing")
          ])

witnessResultOk : JSON -> Bool
witnessResultOk witness = boolFieldTrue "result" witness

snapshotIdFromParams : JSON -> Maybe String
snapshotIdFromParams params =
  jsonStringField "snapshotId" params <|> jsonStringField "workingSnapshotId" params

allIn : List String -> List String -> Bool
allIn [] _ = True
allIn (x :: xs) ys = (x `elem` ys) && allIn xs ys

sameStringSet : List String -> List String -> Bool
sameStringSet xs ys = allIn xs ys && allIn ys xs

requiredStringField : String -> JSON -> Either SnapshotError String
requiredStringField key json =
  maybe (err "PromotionWitnessInvalid" (key ++ " is required")) Right (jsonStringField key json)

requiredAnyStringField : List String -> JSON -> Either SnapshotError String
requiredAnyStringField [] _ = err "PromotionWitnessInvalid" "required string field was missing"
requiredAnyStringField (key :: keys) json =
  case jsonStringField key json of
    Just value => Right value
    Nothing => requiredAnyStringField keys json

requiredStringMatches : String -> String -> JSON -> Either SnapshotError ()
requiredStringMatches key expected json = do
  actual <- requiredStringField key json
  if actual == expected
     then Right ()
     else err "PromotionWitnessInvalid" (key ++ " did not match")

requiredAnyStringArrayField : List String -> JSON -> Either SnapshotError (List String)
requiredAnyStringArrayField [] _ =
  err "PromotionWitnessInvalid" "verifiedPathSpecIds is required"
requiredAnyStringArrayField (key :: keys) json =
  case jsonStringArrayField key json of
    Just values => Right values
    Nothing => requiredAnyStringArrayField keys json

payloadImpactSpecs : JSON -> List String
payloadImpactSpecs payload = specsOfFacts (mapValues (mapPairs (pathsFromPayload payload)))

diffImpactSpecs : JSON -> List String
diffImpactSpecs diff =
  case jsonStringArrayField "pathImpactClosed" diff of
    Just values => values
    Nothing =>
      case jsonStringArrayField "impactClosed" diff of
        Just values => values
        Nothing => []

impactProjectionForWitness :
     PathSnapshotState
  -> String
  -> JSON
  -> Either SnapshotError (Maybe String, List String, String)
impactProjectionForWitness state snapshotId payload =
  case jsonStringField "parentVerified" payload of
    Just parent => do
      diff <- diffSnapshots state parent snapshotId
      let impact = diffImpactSpecs diff
      pure (Just (fieldString "pathDiffDigest" (fieldString "diffDigest" "" diff) diff), impact, fieldString "pathImpactDigest" (digestJSON (specsArray impact)) diff)
    Nothing =>
      let impact = payloadImpactSpecs payload in
          pure (Nothing, impact, digestJSON (specsArray impact))

verifyPromotionWitness :
     PathSnapshotState
  -> String
  -> JSON
  -> JSON
  -> Either SnapshotError ()
verifyPromotionWitness state snapshotId payload witness = do
  if witnessResultOk witness
     then Right ()
     else err "PromotionWitnessInvalid" "promotion witness result was not accepted"
  requiredStringMatches "workingSnapshotId" snapshotId witness
  case jsonStringField "parentVerified" payload of
    Just parent =>
      requiredStringMatches "parentSnapshotId" parent witness
    Nothing => Right ()
  (maybeDiffDigest, actualImpact, actualImpactDigest) <- impactProjectionForWitness state snapshotId payload
  case maybeDiffDigest of
    Just actualDiffDigest => do
      expectedDiffDigest <- requiredStringField "pathDiffDigest" witness
      if actualDiffDigest == expectedDiffDigest
         then Right ()
         else err "PromotionWitnessInvalid" "promotion witness pathDiffDigest did not match"
    Nothing =>
      case jsonStringField "pathDiffDigest" witness of
        Just _ => err "PromotionWitnessInvalid" "pathDiffDigest requires a parent snapshot"
        Nothing => Right ()
  expectedImpactDigest <- requiredAnyStringField ["pathImpactDigest", "impactDigest"] witness
  if actualImpactDigest == expectedImpactDigest
     then Right ()
     else err "PromotionWitnessInvalid" "promotion witness pathImpactDigest did not match"
  verifiedImpact <- requiredAnyStringArrayField ["verifiedPathSpecIds", "verifiedSpecIds"] witness
  if sameStringSet actualImpact verifiedImpact
     then Right ()
     else err "PromotionWitnessInvalid" "verifiedPathSpecIds did not match path impact"
  _ <- requiredStringField "qeqVerifierVersion" witness
  let registryDigest = fieldString "specRegistryDigest" "unbound" payload
  if registryDigest == "unbound"
     then Right ()
     else requiredStringMatches "specRegistryDigest" registryDigest witness

preparedRecord : String -> JSON -> JSON -> JSON
preparedRecord snapshotId params witness =
  JObject
    [ ("snapshotId", JString snapshotId)
    , ("workingSnapshotId", JString snapshotId)
    , ("witness", witness)
    , ("luciCertificateDigest", fieldJSON "luciCertificateDigest" JNull params)
    , ("pathProjectionDigest", fieldJSON "pathProjectionDigest" (JString (digestJSON witness)) params)
    , ("protocolVersion", JString protocolVersion)
    , ("schemaVersion", JString schemaVersion)
    ]

dropPreparedForSnapshot : String -> SortedMap String JSON -> SortedMap String JSON
dropPreparedForSnapshot snapshotId prepared =
  foldl
    (\acc, (key, value) =>
       if fieldString "snapshotId" "" value == snapshotId
          then acc
          else insert key value acc)
    empty
    (Data.SortedMap.toList prepared)

resolvePreparedParams :
     PathSnapshotState
  -> JSON
  -> Either SnapshotError (Maybe String, JSON)
resolvePreparedParams state params =
  case jsonStringField "preparedDigest" params of
    Nothing => Right (Nothing, params)
    Just preparedDigest => do
      prepared <- maybe (err "PreparedPromotionNotFound" "preparedDigest was not found") Right (lookup preparedDigest state.prepared)
      case snapshotIdFromParams params of
        Just requested =>
          if fieldString "snapshotId" "" prepared == requested
             then Right ()
             else err "InvalidParams" "preparedDigest was bound to another snapshot"
        Nothing => Right ()
      Right (Just preparedDigest, prepared)

export
prepareSnapshotPromotion :
     PathSnapshotState
  -> JSON
  -> Either SnapshotError (PathSnapshotState, JSON)
prepareSnapshotPromotion state params = do
  snapshotId <-
    maybe (err "InvalidParams" "snapshotId is required") Right (snapshotIdFromParams params)
  payload <- maybe (err "SnapshotNotFound" "snapshotId was not found") Right (lookup snapshotId state.payloads)
  let currentLife = fromMaybe "Archived" (lookup snapshotId state.life)
  if currentLife == "Working" || currentLife == "Prepared"
     then Right ()
     else err "InvalidTransition" "only a Working or Prepared snapshot can be prepared"
  let project = projectOfPayload payload
  case lookup project state.currentWorking of
    Just current =>
      if current == snapshotId
         then Right ()
         else err "InvalidTransition" "snapshot was not the current working snapshot"
    Nothing => err "InvalidTransition" "project had no current working snapshot"
  witness <- maybe (err "PromotionWitnessInvalid" "witness is required") Right (jsonField "witness" params)
  verifyPromotionWitness state snapshotId payload witness
  let prepared = preparedRecord snapshotId params witness
  let preparedDigest = "redox-prepared-" ++ digestJSON prepared
  case lookup preparedDigest state.prepared of
    Just previous =>
      if previous == prepared
         then Right ()
         else err "DigestCollision" "prepared promotion digest collision with distinct payload"
    Nothing => Right ()
  let nextState =
        ({ life := insert snapshotId "Prepared" state.life
         , prepared := insert preparedDigest prepared state.prepared
         } state)
  pure
    ( nextState
    , JObject
        [ ("snapshotId", JString snapshotId)
        , ("preparedDigest", JString preparedDigest)
        , ("life", JString "Prepared")
        ])

export
promoteSnapshot :
     PathSnapshotState
  -> JSON
  -> Either SnapshotError (PathSnapshotState, JSON)
promoteSnapshot state params = do
  (maybePreparedDigest, promotionParams) <- resolvePreparedParams state params
  snapshotId <-
    maybe (err "InvalidParams" "snapshotId is required") Right
      (snapshotIdFromParams promotionParams)
  payload <- maybe (err "SnapshotNotFound" "snapshotId was not found") Right (lookup snapshotId state.payloads)
  let currentLife = fromMaybe "Archived" (lookup snapshotId state.life)
  witness <- maybe (err "PromotionWitnessInvalid" "witness is required") Right (jsonField "witness" promotionParams)
  verifyPromotionWitness state snapshotId payload witness
  if currentLife == "Verified"
     then
       let preparedAfter : SortedMap String JSON
           preparedAfter =
             case maybePreparedDigest of
               Just preparedDigest => deleteJSONMap preparedDigest (state.prepared)
               Nothing => state.prepared
           nextState : PathSnapshotState
           nextState = ({ prepared := preparedAfter } state)
        in pure
             ( nextState
             , JObject
                 [ ("snapshotId", JString snapshotId)
                 , ("life", JString "Verified")
                 , ("idempotent", JBoolean True)
                 ])
     else do
       if currentLife == "Working" || currentLife == "Prepared"
          then Right ()
          else err "InvalidTransition" "only a Working or Prepared snapshot can be promoted"
       let project = projectOfPayload payload
       case lookup project state.currentWorking of
         Just current =>
           if current == snapshotId
              then Right ()
              else err "InvalidTransition" "snapshot was not the current working snapshot"
         Nothing => err "InvalidTransition" "project had no current working snapshot"
       let archivedLife =
             case lookup project state.currentVerified of
               Just oldVerified =>
                 if oldVerified == snapshotId
                    then state.life
                    else insert oldVerified "Archived" state.life
               Nothing => state.life
       let preparedAfter : SortedMap String JSON
           preparedAfter =
             case maybePreparedDigest of
               Just preparedDigest => deleteJSONMap preparedDigest (dropPreparedForSnapshot snapshotId (state.prepared))
               Nothing => dropPreparedForSnapshot snapshotId state.prepared
       let nextState =
             ({ life := insert snapshotId "Verified" archivedLife
              , currentVerified := insert project snapshotId state.currentVerified
              , currentWorking := deleteStringMap project (state.currentWorking)
              , prepared := preparedAfter
              , witnesses := insert snapshotId witness state.witnesses
              } state)
       pure
         ( nextState
         , JObject
             [ ("snapshotId", JString snapshotId)
             , ("life", JString "Verified")
             , ("idempotent", JBoolean False)
             ])

export
rejectSnapshot :
     PathSnapshotState
  -> JSON
  -> Either SnapshotError (PathSnapshotState, JSON)
rejectSnapshot state params = do
  (maybePreparedDigest, rejectionParams) <- resolvePreparedParams state params
  snapshotId <-
    maybe (err "InvalidParams" "snapshotId is required") Right
      (snapshotIdFromParams rejectionParams)
  payload <- maybe (err "SnapshotNotFound" "snapshotId was not found") Right (lookup snapshotId state.payloads)
  let currentLife = fromMaybe "Archived" (lookup snapshotId state.life)
  if currentLife == "Rejected"
     then
       let preparedAfter : SortedMap String JSON
           preparedAfter =
             case maybePreparedDigest of
               Just preparedDigest => deleteJSONMap preparedDigest (state.prepared)
               Nothing => state.prepared
           nextState : PathSnapshotState
           nextState = ({ prepared := preparedAfter } state)
        in pure
             ( nextState
             , JObject
                 [ ("snapshotId", JString snapshotId)
                 , ("life", JString "Rejected")
                 , ("idempotent", JBoolean True)
                 ])
     else do
       if currentLife == "Working" || currentLife == "Prepared"
          then Right ()
          else err "InvalidTransition" "only a Working or Prepared snapshot can be rejected"
       let project = projectOfPayload payload
       let workingAfter =
             case lookup project state.currentWorking of
               Just current =>
                 if current == snapshotId then deleteStringMap project (state.currentWorking) else state.currentWorking
               Nothing => state.currentWorking
       let preparedAfter : SortedMap String JSON
           preparedAfter =
             case maybePreparedDigest of
               Just preparedDigest => deleteJSONMap preparedDigest (dropPreparedForSnapshot snapshotId (state.prepared))
               Nothing => dropPreparedForSnapshot snapshotId state.prepared
       let nextState =
             ({ life := insert snapshotId "Rejected" state.life
              , currentWorking := workingAfter
              , prepared := preparedAfter
              } state)
       pure
         ( nextState
         , JObject
             [ ("snapshotId", JString snapshotId)
             , ("life", JString "Rejected")
             , ("idempotent", JBoolean False)
             ])

export
exportSnapshotStore : PathSnapshotState -> JSON
exportSnapshotStore = stateToJSON

export
importSnapshotStore :
     JSON
  -> Either SnapshotError (PathSnapshotState, JSON)
importSnapshotStore params = do
  let stateJSON = fieldJSON "state" params params
  nextState <- stateFromJSON stateJSON
  pure
    ( nextState
    , JObject
        [ ("protocolVersion", JString protocolVersion)
        , ("schemaVersion", JString schemaVersion)
        , ("payloadCount", JNumber (cast (length (Data.SortedMap.toList nextState.payloads))))
        , ("preparedCount", JNumber (cast (length (Data.SortedMap.toList nextState.prepared))))
        ])
