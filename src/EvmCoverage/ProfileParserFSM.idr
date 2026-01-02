||| FSM-based Profile HTML Parser (V2)
||| Single-pass finite state machine, minimal allocations
module EvmCoverage.ProfileParserFSM

import Data.List
import Data.Maybe

%default covering

-- =============================================================================
-- Minimal coverage data (no content field)
-- =============================================================================

public export
record CoverageHit where
  constructor MkCoverageHit
  lineNum : Nat
  hitCount : Nat

public export
Show CoverageHit where
  show h = "Hit(line=" ++ show h.lineNum ++ ",count=" ++ show h.hitCount ++ ")"

-- =============================================================================
-- FSM States (carry line value through transitions)
-- =============================================================================

data State
  = Scanning
  | MatchSpan Nat              -- matching "<span class=pc", position
  | InSpan                     -- inside span, before title
  | MatchTitle Nat             -- matching "title=\"", position
  | InTitle Nat                -- inside title, carrying line value
  | MatchLine Nat Nat          -- matching "line ", position, current line
  | ReadLine Nat               -- reading line digits, accumulator
  | MatchCount Nat Nat         -- matching "count ", position, line value
  | ReadCount Nat Nat          -- reading count digits, line value, accumulator
  | SkipToClose Nat Nat        -- waiting for </span>, line, count
  | MatchClose Nat Nat Nat     -- matching "</span>", position, line, count

-- =============================================================================
-- Pattern matching helpers
-- =============================================================================

spanPat : List Char
spanPat = unpack "<span class=pc"

titlePat : List Char
titlePat = unpack "title=\""

linePat : List Char
linePat = unpack "line "

countPat : List Char
countPat = unpack "count "

closePat : List Char
closePat = unpack "</span>"

matchAt : List Char -> Nat -> Char -> Bool
matchAt pat pos c = case drop pos pat of
  (x :: _) => x == c
  [] => False

patLen : List Char -> Nat
patLen = length

toDigit : Char -> Maybe Nat
toDigit c = if isDigit c then Just (cast (ord c - ord '0')) else Nothing

-- =============================================================================
-- FSM Transitions
-- =============================================================================

step : State -> Char -> (State, Maybe CoverageHit)

-- Scanning for <span class=pc
step Scanning '<' = (MatchSpan 1, Nothing)
step Scanning _ = (Scanning, Nothing)

step (MatchSpan pos) c =
  if matchAt spanPat pos c
    then if S pos >= patLen spanPat
           then (InSpan, Nothing)
           else (MatchSpan (S pos), Nothing)
    else if c == '<' then (MatchSpan 1, Nothing)
    else (Scanning, Nothing)

-- Inside span, looking for title="
step InSpan 't' = (MatchTitle 1, Nothing)
step InSpan '>' = (Scanning, Nothing)  -- malformed
step InSpan _ = (InSpan, Nothing)

step (MatchTitle pos) c =
  if matchAt titlePat pos c
    then if S pos >= patLen titlePat
           then (InTitle 0, Nothing)  -- start with line=0
           else (MatchTitle (S pos), Nothing)
    else (InSpan, Nothing)

-- Inside title attribute, looking for line/count
step (InTitle lineVal) 'l' = (MatchLine 1 lineVal, Nothing)
step (InTitle lineVal) 'c' = (MatchCount 1 lineVal, Nothing)
step (InTitle lineVal) '"' = (SkipToClose lineVal 0, Nothing)  -- end title, default count=0
step (InTitle lineVal) _ = (InTitle lineVal, Nothing)

-- Matching "line "
step (MatchLine pos lineVal) c =
  if matchAt linePat pos c
    then if S pos >= patLen linePat
           then (ReadLine 0, Nothing)
           else (MatchLine (S pos) lineVal, Nothing)
    else (InTitle lineVal, Nothing)

-- Reading line number
step (ReadLine acc) c = case toDigit c of
  Just d => (ReadLine (acc * 10 + d), Nothing)
  Nothing => (InTitle acc, Nothing)  -- done reading, store line value

-- Matching "count "
step (MatchCount pos lineVal) c =
  if matchAt countPat pos c
    then if S pos >= patLen countPat
           then (ReadCount lineVal 0, Nothing)
           else (MatchCount (S pos) lineVal, Nothing)
    else (InTitle lineVal, Nothing)

-- Reading count number
step (ReadCount lineVal acc) c = case toDigit c of
  Just d => (ReadCount lineVal (acc * 10 + d), Nothing)
  Nothing => (SkipToClose lineVal acc, Nothing)

-- Skip to </span>
step (SkipToClose lineVal countVal) '<' = (MatchClose 1 lineVal countVal, Nothing)
step (SkipToClose lineVal countVal) _ = (SkipToClose lineVal countVal, Nothing)

-- Matching </span>
step (MatchClose pos lineVal countVal) c =
  if matchAt closePat pos c
    then if S pos >= patLen closePat
           then (Scanning, Just (MkCoverageHit lineVal countVal))
           else (MatchClose (S pos) lineVal countVal, Nothing)
    else (SkipToClose lineVal countVal, Nothing)

-- =============================================================================
-- Main Parser
-- =============================================================================

runFSM : List Char -> List CoverageHit
runFSM = go Scanning []
  where
    go : State -> List CoverageHit -> List Char -> List CoverageHit
    go _ acc [] = reverse acc
    go st acc (c :: cs) =
      let (st', mhit) = step st c
          acc' = case mhit of
                   Nothing => acc
                   Just h => h :: acc
      in go st' acc' cs

||| Extract coverage hits from HTML (FSM version)
export
extractHitsFSM : String -> List CoverageHit
extractHitsFSM html = runFSM (unpack html)

||| Count total and covered expressions
export
countCoverage : String -> (Nat, Nat)
countCoverage html =
  let hits = extractHitsFSM html in
  let totalCount = length hits in
  let coveredCount = length (filter (\h => h.hitCount > 0) hits) in
  (totalCount, coveredCount)
