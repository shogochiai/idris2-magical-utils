||| Synthetic HTML fixture generator for benchmarking
||| Generates profile HTML with configurable span count
module GenFixtures

import Data.List
import Data.String
import System
import System.File

%default total

-- Helper functions for Nat division/modulo
divN : Nat -> Nat -> Nat
divN n d = cast $ (cast {to=Integer} n) `div` (cast {to=Integer} d)

modN : Nat -> Nat -> Nat
modN n d = cast $ (cast {to=Integer} n) `mod` (cast {to=Integer} d)

||| Generate a single span element
||| pcClass alternates 0-7, count varies by pattern
genSpan : Nat -> String
genSpan idx =
  let lineNum = divN idx 10 + 1
      charNum = modN idx 80 + 1
      pcClass = modN idx 8
      -- Vary count: ~30% zero (uncovered), rest 1-100
      countVal = if modN idx 3 == 0 then 0 else modN idx 100 + 1
      content = "expr_" ++ show idx
  in "<span class=pc" ++ show pcClass
     ++ " title=\"line " ++ show lineNum
     ++ " char " ++ show charNum
     ++ " count " ++ show countVal
     ++ "\">" ++ content ++ "</span>\n"

||| Generate HTML header
htmlHeader : String
htmlHeader = """
<!DOCTYPE html>
<html>
<head><title>Profile Output</title></head>
<body>
<pre>
"""

||| Generate HTML footer
htmlFooter : String
htmlFooter = """
</pre>
</body>
</html>
"""

||| Generate spans in chunks to avoid stack overflow
partial
genSpansChunk : Nat -> Nat -> List String -> List String
genSpansChunk start 0 acc = acc
genSpansChunk start (S n) acc = genSpansChunk (S start) n (genSpan start :: acc)

||| Generate full HTML with N spans
partial
generateHtml : Nat -> String
generateHtml n =
  let chunkSize = 1000
      chunks = divN n chunkSize
      remainder = modN n chunkSize
      -- Generate in reverse, then reverse at end
      go : Nat -> Nat -> List String -> List String
      go _ 0 acc = acc
      go offset (S c) acc =
        let chunk = genSpansChunk offset chunkSize []
        in go (offset + chunkSize) c (chunk ++ acc)
      allSpans = go 0 chunks []
      finalSpans = genSpansChunk (chunks * chunkSize) remainder [] ++ allSpans
  in htmlHeader ++ concat (reverse finalSpans) ++ htmlFooter

||| Parse command line for span count
parseArgs : List String -> Maybe Nat
parseArgs (_ :: countStr :: _) = parsePositive countStr
parseArgs _ = Nothing

partial
main : IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Nothing => do
      putStrLn "Usage: gen-fixtures <span_count>"
      putStrLn "Example: gen-fixtures 50000 > fixtures/sample-50k.html"
    Just n => do
      let html = generateHtml n
      putStr html
