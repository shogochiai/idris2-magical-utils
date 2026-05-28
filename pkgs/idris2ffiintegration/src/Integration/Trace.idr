module Integration.Trace

import Integration.Model
import Data.List
import Data.List1
import Data.Maybe
import Data.String

%default covering

public export
record ParsedTraceEvent where
  constructor MkParsedTraceEvent
  schema : String
  opId : String
  trust : Maybe TrustLevel
  outcome : String
  evidenceIds : List String
  rawLine : String

public export
prefixChars : List Char -> List Char -> Bool
prefixChars [] _ = True
prefixChars _ [] = False
prefixChars (x :: xs) (y :: ys) = x == y && prefixChars xs ys

public export
dropChars : Nat -> List Char -> List Char
dropChars Z xs = xs
dropChars (S k) [] = []
dropChars (S k) (_ :: xs) = dropChars k xs

public export
findAfterChars : List Char -> List Char -> Maybe (List Char)
findAfterChars needle [] = if null needle then Just [] else Nothing
findAfterChars needle haystack@(_ :: rest) =
  if prefixChars needle haystack
    then Just (dropChars (length needle) haystack)
    else findAfterChars needle rest

public export
takeJsonStringChars : List Char -> List Char
takeJsonStringChars [] = []
takeJsonStringChars ('\\' :: c :: rest) = c :: takeJsonStringChars rest
takeJsonStringChars ('"' :: _) = []
takeJsonStringChars (c :: rest) = c :: takeJsonStringChars rest

public export
takeUntilChar : Char -> List Char -> List Char
takeUntilChar stop [] = []
takeUntilChar stop (c :: rest) =
  if c == stop then [] else c :: takeUntilChar stop rest

public export
splitComma : String -> List String
splitComma value = forget (split (== ',') value)

public export
stripJsonStringToken : String -> Maybe String
stripJsonStringToken raw =
  let token = trim raw in
  case unpack token of
    '"' :: rest => Just (pack (takeJsonStringChars rest))
    _ => Nothing

public export
parseJsonStringField : String -> String -> Maybe String
parseJsonStringField field line =
  case findAfterChars (unpack ("\"" ++ field ++ "\":\"")) (unpack line) of
    Nothing => Nothing
    Just rest => Just (pack (takeJsonStringChars rest))

public export
parseJsonStringArrayField : String -> String -> Maybe (List String)
parseJsonStringArrayField field line =
  case findAfterChars (unpack ("\"" ++ field ++ "\":[")) (unpack line) of
    Nothing => Nothing
    Just rest =>
      let segment = pack (takeUntilChar ']' rest) in
      Just (mapMaybe stripJsonStringToken (splitComma segment))

public export
parseTrustLevel : String -> Maybe TrustLevel
parseTrustLevel "unsafe-diagnostic" = Just UnsafeDiagnostic
parseTrustLevel "candidate" = Just Candidate
parseTrustLevel "proof-grade" = Just ProofGrade
parseTrustLevel _ = Nothing

public export
parseTraceEventLine : String -> Maybe ParsedTraceEvent
parseTraceEventLine line = do
  schema <- parseJsonStringField "schema" line
  opId <- parseJsonStringField "opId" line
  outcome <- parseJsonStringField "outcome" line
  evidenceIds <- parseJsonStringArrayField "evidenceIds" line
  let trust = parseJsonStringField "trust" line >>= parseTrustLevel
  Just (MkParsedTraceEvent schema opId trust outcome evidenceIds line)

public export
parsedTraceCovers : SomeIntegrationOp -> ParsedTraceEvent -> Bool
parsedTraceCovers op event =
  event.schema == "etherclaw.integration.trace.v1"
    && event.opId == somePathId op
    && someTrust op == ProofGrade
    && maybe True (== ProofGrade) event.trust
    && event.outcome == "pass"
    && all (\evidenceId => elem evidenceId event.evidenceIds) (someEvidenceIds op)

public export
traceLineCovers : SomeIntegrationOp -> String -> Bool
traceLineCovers op line =
  case parseTraceEventLine line of
    Nothing => False
    Just event => parsedTraceCovers op event
