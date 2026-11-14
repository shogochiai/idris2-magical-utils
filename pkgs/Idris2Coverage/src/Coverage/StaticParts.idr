||| REQ_COV_STATIC_PARTS_001: the static path universe of a large package, read from
||| the compiler's per-module entries instead of from its assembled JSON.
|||
||| Why. `--dumppaths-json F --build P.ipkg` appends one function entry per line to
||| `F.parts` after every module it compiles, and only at the executable stage reads
||| them back, keeps the FIRST entry of every function name, and writes `F`. That
||| last step joins the entries with `x ++ ", " ++ rest`, which is quadratic.
||| Measured 2026-11-14 on luci pkgs/Luci (289 modules, 29139 entries, 31.5 MB of
||| parts, compiler ddcef732): 27 min to compile the modules, then 73 min in the
||| stage that ends in that join; 49 s with a linear join. The chunked path avoided
||| the long join by never building the package in one go, at the price of compiling
||| every module's dependants again per chunk (about 95 min).
|||
||| Here the compiler stops before the executable stage (the package is built as a
||| library in a fresh build dir, see `staticPartsIpkg`) and this module does the
||| assembly: the same first-entry-per-name rule, one entry at a time, so neither
||| the 26 MB JSON nor its parse tree is ever held in one piece.
|||
||| WHAT IT REPRODUCES. For that tree the entries kept here are exactly the function
||| objects of the compiler's own JSON (25168 of 29139; header + kept entries joined
||| by ", " + footer was byte-identical to the file the compiler wrote).
|||
||| WHAT IT DOES NOT CLAIM. Two different functions can carry one display name
||| (several `Eq` implementations of a module are all `M.==`, nested case blocks are
||| all `M.case block in f`): 1074 names on that tree. The compiler keeps the first
||| and the others' paths are not in the universe (40550 of 46086 paths). That is
||| the compiler's rule and it is kept unchanged here, so that a package measured
||| this way and one measured through the compiler's own JSON mean the same thing;
||| `repeated` and `repeatedDifferent` say how often it happened.
module Coverage.StaticParts

import Data.List
import Data.SortedMap
import Data.String
import Coverage.Core.DumppathsJson
import Coverage.Core.PathCoverage
import Coverage.Standardization.Types

%default covering

||| What has been read so far. `seen` maps a function name to the entry kept for it.
public export
record PartsFold where
  constructor MkPartsFold
  seen              : SortedMap String String
  ||| obligations of the kept entries, newest first
  keptRev           : List PathObligation
  ||| function entries read (blank lines are not entries)
  entries           : Nat
  ||| entries dropped because an earlier entry has the same function name
  repeated          : Nat
  ||| of those, the ones whose text differs from the kept entry: another function
  ||| under the same name, whose paths are therefore NOT in the universe
  repeatedDifferent : Nat

export
emptyPartsFold : PartsFold
emptyPartsFold = MkPartsFold empty [] 0 0 0

functionNameField : List Char
functionNameField = unpack "\"function_name\": \""

||| The name the compiler dedupes an entry by: the text between the first
||| `"function_name": "` and the next `"`. An entry without one has no name and is
||| always kept (the compiler's `extractFunctionNameEntry`, rule for rule).
export
partEntryName : String -> Maybe String
partEntryName entry = go (unpack entry)
  where
    go : List Char -> Maybe String
    go [] = Nothing
    go s@(_ :: rest) =
      if isPrefixOf functionNameField s
         then let name = pack (takeWhile (/= '"') (drop (length functionNameField) s))
              in if name == "" then Nothing else Just name
         else go rest

parseEntry : List EffectBoundarySpec -> String -> Either String (List PathObligation)
parseEntry specs entry =
  parseDumppathsJsonWithSpecs specs ("{\"functions\":[" ++ entry ++ "]}")

revOnto : List a -> List a -> List a
revOnto acc [] = acc
revOnto acc (x :: xs) = revOnto (x :: acc) xs

||| Read one line of the parts file. An entry that cannot be parsed is an error,
||| never a skipped line: a universe with a hole in it is not a smaller universe.
export
stepPart : List EffectBoundarySpec -> PartsFold -> (line : String) -> Either String PartsFold
stepPart specs st line =
  let entry = trim line in
  if entry == "" then Right st
  else case partEntryName entry of
    Nothing => do
      paths <- parseEntry specs entry
      Right ({ keptRev := revOnto st.keptRev paths, entries $= S } st)
    Just name => case lookup name st.seen of
      Just kept =>
        Right ({ entries $= S, repeated $= S
               , repeatedDifferent $= (if kept == entry then id else S) } st)
      Nothing => case parseEntry specs entry of
        Left err => Left (err ++ " (entry of " ++ name ++ ")")
        Right paths =>
          Right ({ seen $= insert name entry
                 , keptRev := revOnto st.keptRev paths
                 , entries $= S } st)

||| The obligations of the kept entries, in the order the compiler wrote them.
export
partsObligations : PartsFold -> List PathObligation
partsObligations st = reverse st.keptRev

||| Function entries kept (one per name, plus the nameless ones).
export
partsKept : PartsFold -> Nat
partsKept st = minus st.entries st.repeated

||| All lines at once (tests, small inputs). The runner feeds `stepPart` line by line.
export
foldParts : List EffectBoundarySpec -> List String -> Either String PartsFold
foldParts specs = go emptyPartsFold
  where
    go : PartsFold -> List String -> Either String PartsFold
    go st [] = Right st
    go st (l :: ls) = case stepPart specs st l of
                        Left err => Left err
                        Right st' => go st' ls

||| The ipkg the compiler is given: every module of the package, the package's own
||| dependencies, a build dir of its own — and no `main` / `executable`, so the build
||| ends after the last module and the parts stay on disk.
|||
||| The build dir must be one no earlier build wrote to: a module that is loaded from
||| a .ttc writes no entry at all (and REQ_COV_STATIC_UNIVERSE_001: where the compiler
||| does list a loaded module, it lists other paths).
export
staticPartsIpkg : (pkgName : String) -> (builddir : String) -> (sourcedir : String)
               -> (depends : List String) -> (modules : List String) -> String
staticPartsIpkg pkgName builddir sourcedir depends modules =
  unlines
    [ "package " ++ pkgName
    , "builddir = \"" ++ builddir ++ "\""
    , "sourcedir = \"" ++ sourcedir ++ "\""
    , "depends = " ++ joinBy ", " (if null depends then ["base"] else depends)
    , "modules = " ++ joinBy ", " modules
    ]

||| How many modules the compiler says it built (`3/289: Building M (src/M.idr)`).
export
modulesBuilt : (buildLog : String) -> Nat
modulesBuilt buildLog = length (filter (isInfixOf ": Building ") (lines buildLog))

||| REQ_COV_STATIC_PARTS_001: the parts are a universe only if the build that wrote
||| them finished and compiled every module in this process. A build that stopped
||| half way (out of memory, a compile error) leaves a parts file that looks exactly
||| like a smaller package.
export
partsBuildComplete : (rc : Int) -> (modules : Nat) -> (built : Nat) -> Either String ()
partsBuildComplete rc modules built =
  if rc /= 0
     then Left ("the build ended with rc=" ++ show rc
                ++ (if rc == 137 then " (killed, likely out of memory)" else "")
                ++ " after " ++ show built ++ " of " ++ show modules ++ " modules")
  else if built < modules
     then Left ("the build compiled " ++ show built ++ " of " ++ show modules
                ++ " modules in this process; a module that was not compiled wrote no entries")
  else Right ()
