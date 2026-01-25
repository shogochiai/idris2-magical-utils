||| Subcontract Core: Compile-Time Schema Upgrade Validation
|||
||| Uses Idris2's elaborator reflection to validate schema upgrades at compile time.
||| Prevents storage collisions by ensuring upgrades are append-only.
|||
||| Usage:
||| ```idris
||| %runElab checkUpgrade SchemaV1 SchemaV2
||| ```
|||
||| If the upgrade is unsafe, compilation fails with a detailed error message
||| showing which field caused the collision and why.
module Subcontract.Core.SchemaCheck

import public Language.Reflection
import public Subcontract.Core.Schema
import public Subcontract.Core.SchemaCompat

import Data.String

%default total
%language ElabReflection

-- =============================================================================
-- Error Formatting
-- =============================================================================

||| Format a list of collisions as a human-readable error message
formatCollisions : List SchemaCollision -> String
formatCollisions cs = unlines $
  [ ""
  , "SCHEMA UPGRADE VALIDATION FAILED"
  , "================================"
  , ""
  ] ++ map show cs ++
  [ ""
  , "Schema upgrades must be append-only:"
  , "  - Existing fields cannot be removed"
  , "  - Existing fields cannot be reordered"
  , "  - Field types cannot be changed"
  , "  - New fields must be appended at the end"
  ]

-- =============================================================================
-- Elab Macro
-- =============================================================================

||| Compile-time schema upgrade check.
|||
||| Validates that upgrading from `old` to `new` is safe:
||| - Same namespace ID
||| - Same root slot
||| - All fields from `old` preserved in `new` at same positions
||| - New fields only appended at the end
|||
||| Fails compilation with detailed error message if upgrade is unsafe.
|||
||| @ old The existing schema (V1)
||| @ new The proposed schema (V2)
|||
||| Usage:
||| ```idris
||| %runElab checkUpgrade SchemaV1 SchemaV2
||| ```
export
checkUpgrade : Schema -> Schema -> Elab ()
checkUpgrade old new =
  case checkSchemaCompat old new of
    Compatible => do
      let added = minus (length new.fields) (length old.fields)
      logMsg "schema" 1 $ "Schema '" ++ old.nsId ++ "' upgrade validated: " ++
                         show added ++ " field(s) added"
    Incompatible collisions =>
      fail $ formatCollisions collisions

||| Macro version for nicer syntax.
|||
||| Can be used inline in type signatures or definitions:
||| ```idris
||| validUpgrade : ()
||| validUpgrade = assertUpgradeSafe SchemaV1 SchemaV2
||| ```
export %macro
assertUpgradeSafe : Schema -> Schema -> Elab ()
assertUpgradeSafe = checkUpgrade
