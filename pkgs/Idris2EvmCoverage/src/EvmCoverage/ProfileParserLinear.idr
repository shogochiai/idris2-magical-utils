module EvmCoverage.ProfileParserLinear

import EvmCoverage.Types
import Data.List
import Data.Maybe
import Data.String

%default covering

public export
record ExprHitL where
  constructor MkExprHitL
  line : Nat
  char : Nat
  count : Nat
  content : String

matchAt : String -> String -> Nat -> Nat -> Bool
matchAt needle haystack nLen idx = substr idx nLen haystack == needle

searchGo : String -> String -> Nat -> Nat -> Nat -> Maybe Nat
searchGo needle haystack nLen hLen idx =
  if idx + nLen > hLen then Nothing
  else if matchAt needle haystack nLen idx then Just idx
  else searchGo needle haystack nLen hLen (S idx)

strSearch : String -> String -> Nat -> Maybe Nat
strSearch needle haystack start =
  let nLen = length needle
      hLen = length haystack
  in case (nLen == 0, start + nLen > hLen) of
       (True, _) => Just start
       (_, True) => Nothing
       _ => searchGo needle haystack nLen hLen start

extractNum : String -> String -> Nat
extractNum pfx str =
  case strSearch pfx str 0 of
    Nothing => 0
    Just idx =>
      let afterPfx = substr (idx + length pfx) (length str) str
          digits = takeWhile isDigit (unpack afterPfx)
      in fromMaybe 0 (parsePositive (pack digits))

record SpanPos where
  constructor MkSpanPos
  tStart : Nat
  tEnd : Nat
  cStart : Nat
  cEnd : Nat

findSpansGo : String -> Nat -> List SpanPos -> List SpanPos
findSpansGo html idx acc =
  case strSearch "<span class=pc" html idx of
    Nothing => reverse acc
    Just sIdx =>
      case strSearch "title=\"" html sIdx of
        Nothing => findSpansGo html (sIdx + 14) acc
        Just tIdx =>
          let tStart = tIdx + 7
          in case strSearch "\">" html tStart of
               Nothing => findSpansGo html (sIdx + 14) acc
               Just tEnd =>
                 case strSearch "</span>" html (tEnd + 2) of
                   Nothing => findSpansGo html (sIdx + 14) acc
                   Just cEnd =>
                     findSpansGo html (cEnd + 7)
                       (MkSpanPos tStart tEnd (tEnd + 2) cEnd :: acc)

posToHit : String -> SpanPos -> ExprHitL
posToHit html p =
  let title = substr p.tStart (minus p.tEnd p.tStart) html
      content = substr p.cStart (minus p.cEnd p.cStart) html
  in MkExprHitL
       (extractNum "line " title)
       (extractNum "char " title)
       (extractNum "count " title)
       content

export
extractSpansLinear : String -> List ExprHitL
extractSpansLinear html = map (posToHit html) (findSpansGo html 0 [])

toProfileHit : String -> ExprHitL -> ProfileHit
toProfileHit fp h = MkProfileHit "" h.count fp h.line

export
extractProfileHitsLinear : String -> String -> List ProfileHit
extractProfileHitsLinear fp html = map (toProfileHit fp) (extractSpansLinear html)
