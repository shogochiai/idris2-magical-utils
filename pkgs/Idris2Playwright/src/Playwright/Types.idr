||| Playwright Types
|||
||| Minimal type definitions for Playwright browser automation.
module Playwright.Types

%default total

-- =============================================================================
-- Opaque Handle Types
-- =============================================================================

||| Browser instance handle (opaque pointer to JS object)
export
data Browser : Type where [external]

||| Page instance handle (opaque pointer to JS object)
export
data Page : Type where [external]

-- =============================================================================
-- Coverage Data Types
-- =============================================================================

||| V8 coverage range (byte offsets with execution count)
public export
record CoverageRange where
  constructor MkCoverageRange
  startOffset : Nat
  endOffset : Nat
  count : Nat

public export
Show CoverageRange where
  show r = show r.startOffset ++ "-" ++ show r.endOffset ++ ":" ++ show r.count

||| V8 coverage function entry
public export
record CoverageFunction where
  constructor MkCoverageFunction
  functionName : String
  ranges : List CoverageRange
  isBlockCoverage : Bool

||| V8 coverage entry for a script
public export
record CoverageEntry where
  constructor MkCoverageEntry
  url : String
  scriptId : String
  functions : List CoverageFunction

public export
Show CoverageEntry where
  show e = "CoverageEntry(" ++ e.url ++ ", " ++ show (length e.functions) ++ " funcs)"

-- =============================================================================
-- Result Types
-- =============================================================================

||| Playwright operation result
public export
data PlaywrightResult : Type -> Type where
  Success : a -> PlaywrightResult a
  Failure : String -> PlaywrightResult a

public export
Functor PlaywrightResult where
  map f (Success x) = Success (f x)
  map _ (Failure e) = Failure e

public export
Applicative PlaywrightResult where
  pure = Success
  (Success f) <*> (Success x) = Success (f x)
  (Failure e) <*> _ = Failure e
  _ <*> (Failure e) = Failure e

public export
Monad PlaywrightResult where
  (Success x) >>= f = f x
  (Failure e) >>= _ = Failure e
