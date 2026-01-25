||| ERC-7546 Upgrade Validation
|||
||| Integrates compile-time schema checking with OptimisticUpgrader
||| and Inception governance workflows.
|||
||| Provides governance-aware error messages that guide developers
||| on how to resolve schema incompatibilities within the ERC-7546 framework.
|||
||| Usage in upgrade proposals:
||| ```idris
||| import Subcontract.Standards.ERC7546.UpgradeValidation
|||
||| -- Compile-time check with governance guidance
||| %runElab validateUpgradeProposal TokenSchemaV1 TokenSchemaV2
||| ```
module Subcontract.Standards.ERC7546.UpgradeValidation

import public Language.Reflection
import public Subcontract.Core.Schema
import public Subcontract.Core.SchemaCompat
import public Subcontract.Core.SchemaCheck

import Data.String

%default total
%language ElabReflection

-- =============================================================================
-- Governance-Aware Error Messages
-- =============================================================================

||| Format collisions with ERC-7546 governance guidance
formatGovernanceError : List SchemaCollision -> String
formatGovernanceError cs = unlines $
  [ ""
  , "ERC-7546 UPGRADE BLOCKED"
  , "========================"
  , ""
  ] ++ map show cs ++
  [ ""
  , "Resolution Options:"
  , "  1. Fix schema to be append-only compatible"
  , "  2. Deploy migration contract to relocate data"
  , "  3. Create new namespace for breaking changes"
  , ""
  , "For OptimisticUpgrader: This proposal should be REJECTED"
  , "For Inception: This violates storage safety boundary"
  ]

-- =============================================================================
-- Governance Integration
-- =============================================================================

||| Validate upgrade proposal with governance-aware messages.
|||
||| Same validation as `checkUpgrade` but with error messages tailored
||| for ERC-7546 governance workflows (OptimisticUpgrader, Inception).
|||
||| @ old The existing schema (V1)
||| @ new The proposed schema (V2)
|||
||| Usage:
||| ```idris
||| %runElab validateUpgradeProposal TokenSchemaV1 TokenSchemaV2
||| ```
export
validateUpgradeProposal : Schema -> Schema -> Elab ()
validateUpgradeProposal old new =
  case checkSchemaCompat old new of
    Compatible => do
      let added = minus (length new.fields) (length old.fields)
      logMsg "upgrade" 1 $ "UPGRADE VALIDATED: " ++ show added ++
                          " new field(s). Safe for ERC-7546 dictionary update."
    Incompatible collisions =>
      fail $ formatGovernanceError collisions

||| Type alias for a validated schema pair.
|||
||| Used to prove at the type level that a schema upgrade has been validated.
public export
ValidatedUpgrade : Type
ValidatedUpgrade = (Schema, Schema)

||| Create a validated upgrade pair (compile-time checked).
|||
||| Returns the schema pair only if the upgrade is safe.
||| Fails compilation otherwise.
|||
||| @ v1 The existing schema
||| @ v2 The proposed schema
|||
||| Usage:
||| ```idris
||| upgrade : ValidatedUpgrade
||| upgrade = mkValidatedUpgrade TokenSchemaV1 TokenSchemaV2
||| ```
export %macro
mkValidatedUpgrade : (v1 : Schema) -> (v2 : Schema) -> Elab ValidatedUpgrade
mkValidatedUpgrade v1 v2 = do
  checkUpgrade v1 v2
  pure (v1, v2)

||| Get the old schema from a validated upgrade pair.
export
oldSchema : ValidatedUpgrade -> Schema
oldSchema = fst

||| Get the new schema from a validated upgrade pair.
export
newSchema : ValidatedUpgrade -> Schema
newSchema = snd

||| Count the number of fields added in the upgrade.
export
fieldsAdded : ValidatedUpgrade -> Nat
fieldsAdded (old, new) = minus (length new.fields) (length old.fields)
