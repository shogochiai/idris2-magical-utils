||| Linear Token Contract
||| Demonstrates linear types for safe asset management
||| This is a conceptual example showing how QTT prevents double-spending
module LinearToken

-- =============================================================================
-- Linear Resource Type
-- =============================================================================

||| A token that must be used exactly once (linear)
||| The `1` multiplicity ensures:
||| - Cannot be duplicated (no double-spend)
||| - Cannot be dropped (no lost tokens)
data Token : Type where
  MkToken : (1 _ : Bits256) -> Token

||| Proof that an account owns tokens
data Ownership : (account : Bits256) -> (amount : Bits256) -> Type where
  MkOwnership : (1 _ : Token) -> Ownership account amount

-- =============================================================================
-- Safe Transfer (Type-Level Guarantee)
-- =============================================================================

||| Transfer ownership from one account to another
||| The linear type ensures:
||| - The source ownership is consumed (cannot use twice)
||| - A new ownership proof is created for destination
|||
||| This signature makes double-spending a TYPE ERROR, not a runtime bug
transfer : (1 _ : Ownership from amount) ->
           (to : Bits256) ->
           Ownership to amount
transfer (MkOwnership tok) to = MkOwnership tok

-- =============================================================================
-- Split and Merge (Preserving Total)
-- =============================================================================

||| Split tokens (sum must equal original)
||| Type system ensures: amount1 + amount2 = original
split : (1 _ : Ownership acc total) ->
        (amount1 : Bits256) ->
        (amount2 : Bits256) ->
        {auto prf : amount1 + amount2 = total} ->
        (Ownership acc amount1, Ownership acc amount2)
split (MkOwnership (MkToken v)) a1 a2 =
  (MkOwnership (MkToken a1), MkOwnership (MkToken a2))

||| Merge tokens into one
merge : (1 _ : Ownership acc amount1) ->
        (1 _ : Ownership acc amount2) ->
        Ownership acc (amount1 + amount2)
merge (MkOwnership (MkToken v1)) (MkOwnership (MkToken v2)) =
  MkOwnership (MkToken (v1 + v2))

-- =============================================================================
-- Example: Safe Multi-Hop Transfer
-- =============================================================================

||| Transfer through intermediate (still type-safe)
||| The linear chain ensures no duplication at any step
transferVia : (1 _ : Ownership alice amount) ->
              (bob : Bits256) ->
              (charlie : Bits256) ->
              Ownership charlie amount
transferVia aliceOwns bob charlie =
  let bobOwns = transfer aliceOwns bob
      charlieOwns = transfer bobOwns charlie
  in charlieOwns

-- =============================================================================
-- Compile-Time Safety Demonstration
-- =============================================================================

-- This would be a TYPE ERROR (using ownership twice):
--
-- doublespend : (1 _ : Ownership alice 100) -> (Ownership bob 100, Ownership charlie 100)
-- doublespend owns = (transfer owns bob, transfer owns charlie)
--                                 ^^^^           ^^^^
--                     Error: Linear variable 'owns' used more than once

-- This would also be a TYPE ERROR (dropping ownership):
--
-- dropTokens : (1 _ : Ownership alice 100) -> ()
-- dropTokens owns = ()
--             ^^^^
-- Error: Linear variable 'owns' is not used
