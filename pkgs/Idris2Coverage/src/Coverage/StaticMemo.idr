||| Per-module memo of the chunked static denominator (luci feat/step4-diffscope,
||| stage 2).
|||
||| A chunk of the static denominator is one fork `--build` over a temp ipkg
||| whose `modules` are the chunk's members. The JSON it dumps holds the paths of
||| those members and of loaded modules whose NAME EXTENDS a member's (measured
||| 2026-09-30: a chunk of Luci.Boundary.DaemonOps, which imports four other
||| project modules, dumped DaemonOps's 37 functions only; a chunk of
||| Luci.Preflight also dumped the 15 paths of Luci.Preflight.ToolHome, which it
||| imports). Paths are therefore attributed to the LONGEST module prefix and a
||| row holds only the paths of its own module, taken from that module's own
||| chunk, so a row is served again while nothing it depends on has changed.
|||
||| A row's key covers everything its paths are made from: the module's source,
||| the source of every project module in its transitive import closure (path
||| ids embed compile-order counters, which move when an imported module
||| changes — measured on luci pkgs/Luci: adding one unreferenced export to
||| Luci.Workspace moved 686 ids, all in its 11 importers, and none in the 168
||| other modules), and the tools: this idris2-cov's own `.so`, the fork
||| compiler's `.so`, and the content of every installed library `.ttc`.
|||
||| Rows are written to a staging directory and moved into the memo only when
||| the whole run succeeded, so a failed or interrupted run never becomes a row.
module Coverage.StaticMemo

import Data.List
import Data.List1
import Data.Either
import Data.Maybe
import Data.SortedMap
import Data.String
import System
import System.File
import System.Directory
import Coverage.Core.PathCoverage
import Coverage.Core.OrdNub

%default covering

-- =============================================================================
-- Pure parts
-- =============================================================================

||| The project modules a source file imports (`import M`, `import public M`),
||| restricted to `known`. Library imports never enter a key through this list:
||| they are covered by the installed `.ttc` digest in the tool key.
public export
importsOfSource : (known : SortedSet String) -> (source : String) -> List String
importsOfSource known src =
  nubOrd (mapMaybe importName (map trim (lines src)))
  where
    importName : String -> Maybe String
    importName l =
      case words l of
        ("import" :: "public" :: m :: _) => keep m
        ("import" :: m :: _) => keep m
        _ => Nothing
      where
        keep : String -> Maybe String
        keep m = if contains m known then Just m else Nothing

||| Every project module `m` depends on through imports, not including `m`,
||| sorted. A cycle cannot loop: a module is expanded once.
public export
importClosure : SortedMap String (List String) -> String -> List String
importClosure imports m =
  sort (filter (/= m) (Prelude.toList (go [m] (singleton m))))
  where
    go : List String -> SortedSet String -> SortedSet String
    go [] seen = seen
    go (x :: xs) seen =
      let next = filter (\d => not (contains d seen)) (fromMaybe [] (lookup x imports))
      in assert_total (go (next ++ xs) (foldl (flip insert) seen next))

||| The text a row key is the digest of. One line per ingredient, so two
||| different ingredient lists cannot run together into the same text.
public export
moduleKeyMaterial : (toolKey : String) -> (srcHash : SortedMap String String)
                 -> (closure : List String) -> (m : String) -> String
moduleKeyMaterial tool hashes closure m =
  unlines $
    [ "static-memo-v1"
    , "tool " ++ tool
    , "module " ++ m ++ " " ++ fromMaybe "?" (lookup m hashes) ]
    ++ map (\d => "dep " ++ d ++ " " ++ fromMaybe "?" (lookup d hashes)) closure

||| The project module a function belongs to: the longest module name that
||| prefixes it with a dot. `modulesLongestFirst` must be sorted by descending
||| length, so `Luci.Types.Sub.f` goes to `Luci.Types.Sub` and not `Luci.Types`.
public export
moduleOfFunction : (modulesLongestFirst : List String) -> String -> Maybe String
moduleOfFunction ms fn = find (\m => isPrefixOf (m ++ ".") fn) ms

||| Obligations grouped by their project module, each group in input order.
||| Obligations of no listed module (a chunk's generated wrapper) are returned
||| apart: they stay in the chunk's result but are never stored as a row.
public export
groupByModule : (modulesLongestFirst : List String) -> List PathObligation
             -> (SortedMap String (List PathObligation), List PathObligation)
groupByModule ms paths =
  let (grouped, other) = foldl step (empty, []) paths
  in (map reverse grouped, reverse other)
  where
    step : (SortedMap String (List PathObligation), List PathObligation) -> PathObligation
        -> (SortedMap String (List PathObligation), List PathObligation)
    step (g, o) p =
      case moduleOfFunction ms p.functionName of
        Just m => (insert m (p :: fromMaybe [] (lookup m g)) g, o)
        Nothing => (g, p :: o)

||| A path id with the compiler's generated-name counter removed:
||| `M.12766:734:firstJust#p0` becomes `M.#:734:firstJust#p0`. The counter is
||| the part that moves when something imported changes; the rest does not.
public export
stripGenCounter : String -> String
stripGenCounter s = pack (go (unpack s))
  where
    digits : List Char -> (List Char, List Char)
    digits = span isDigit
    go : List Char -> List Char
    go [] = []
    go ('.' :: rest) =
      let (d1, r1) = digits rest in
      case (d1, r1) of
        (_ :: _, ':' :: r2) =>
          let (d2, r3) = digits r2 in
          case (d2, r3) of
            (_ :: _, ':' :: r4) => '.' :: '#' :: ':' :: d2 ++ ':' :: go r4
            _ => '.' :: go rest
        _ => '.' :: go rest
    go (c :: rest) = c :: go rest

||| Hit ids that are not in the universe but whose counter-stripped form is:
||| the signature of a row served from memo after its counters moved. A fresh
||| universe gives none (measured on luci pkgs/Luci: 178 hit ids outside the
||| universe, all record projections, `/=` defaults and interface wrappers the
||| name filter drops, and 0 of them counter-collide). A stale one gives many
||| (the same hits against the universe of the previous tree: 578).
public export
staleHitIds : (universe : List String) -> (hits : List String) -> List String
staleHitIds universe hits =
  let u = fromList universe
      stripped = fromList (map stripGenCounter universe)
      outside = filter (\h => not (inSet u h)) (nubOrd hits)
  in filter (\h => inSet stripped (stripGenCounter h)) outside

||| The `path_id` values of a dumppaths JSON text, in order, without parsing
||| the rest of it.
public export
extractPathIds : String -> List String
extractPathIds s = go (unpack s)
  where
    needle : List Char
    needle = unpack "\"path_id\":"
    skipSpace : List Char -> List Char
    skipSpace = dropWhile (== ' ')
    readStr : List Char -> (List Char, List Char)
    readStr [] = ([], [])
    readStr ('\\' :: c :: rest) = let (a, b) = readStr rest in (c :: a, b)
    readStr ('"' :: rest) = ([], rest)
    readStr (c :: rest) = let (a, b) = readStr rest in (c :: a, b)
    go : List Char -> List String
    go [] = []
    go cs@(_ :: rest) =
      if isPrefixOf needle cs
         then case skipSpace (drop (length needle) cs) of
                ('"' :: r) => let (idc, r') = readStr r in pack idc :: go r'
                r => go r
         else go rest

||| A module name as a directory name: dots kept, nothing else is special.
public export
rowDirName : String -> String
rowDirName = id

-- =============================================================================
-- Files
-- =============================================================================

public export
rowPath : (memoDir, m, key : String) -> String
rowPath dir m key = dir ++ "/rows/" ++ rowDirName m ++ "/" ++ key ++ ".json"

public export
stagingRowPath : (memoDir, m, key : String) -> String
stagingRowPath dir m key = dir ++ "/staging/" ++ rowDirName m ++ "/" ++ key ++ ".json"

shq : String -> String
shq s = "'" ++ fastConcat (map esc (unpack s)) ++ "'"
  where
    esc : Char -> String
    esc '\'' = "'\\''"
    esc c = singleton c

readTrimmed : String -> IO String
readTrimmed p = do
  Right c <- readFile p
    | Left _ => pure ""
  pure (trim c)

||| The tool part of every key: this idris2-cov's `.so` (IDRIS2_INC_SRC, set by
||| its launcher), the fork compiler's `.so`, and every installed library `.ttc`.
||| "" when any part cannot be read, which the caller treats as "no memo".
export
staticMemoToolKey : (scratch : String) -> IO String
staticMemoToolKey scratch = do
  let out = scratch ++ "/toolkey.txt"
  let cmd = "set -o pipefail 2>/dev/null; { "
         ++ "[ -n \"${IDRIS2_INC_SRC:-}\" ] && ls \"$IDRIS2_INC_SRC\"/*.so >/dev/null 2>&1 && sha256sum \"$IDRIS2_INC_SRC\"/*.so && "
         ++ "b=\"$(command -v \"${IDRIS2_BIN:-idris2}\" || echo \"${IDRIS2_BIN:-idris2}\")\" && "
         ++ "r=\"$(readlink -f \"$b\")\" && n=\"$(basename \"$r\")\" && "
         ++ "if [ -f \"$(dirname \"$r\")/${n}_app/$n.so\" ]; then sha256sum \"$(dirname \"$r\")/${n}_app/$n.so\"; else sha256sum \"$r\"; fi && "
         ++ "for p in \"$HOME/.idris2/idris2-0.8.0\" $(printf %s \"${IDRIS2_PACKAGE_PATH:-}\" | tr ':' ' '); do "
         ++ "[ -d \"$p\" ] && find \"$p\" -name '*.ttc' -print0 | sort -z | xargs -0 sha256sum; done; "
         ++ "} | sha256sum | cut -d' ' -f1 > " ++ shq out ++ " 2>/dev/null || : > " ++ shq out
  _ <- system cmd
  readTrimmed out

||| sha256 of each named file, keyed by the name it was given. A file that
||| cannot be hashed is simply absent from the map.
export
sha256Files : (scratch : String) -> List (String, String) -> IO (SortedMap String String)
sha256Files scratch named = do
  let listFile = scratch ++ "/hash-list.txt"
  let out = scratch ++ "/hash-out.txt"
  Right () <- writeFile listFile (unlines (map snd named))
    | Left _ => pure empty
  _ <- system ("tr '\\n' '\\0' < " ++ shq listFile ++ " | xargs -0 sha256sum > " ++ shq out ++ " 2>/dev/null")
  Right txt <- readFile out
    | Left _ => pure empty
  let byPath : SortedMap String String = fromList (mapMaybe parseLine (lines txt))
  pure (fromList (mapMaybe (\(k, p) => map (\h => (k, h)) (Data.SortedMap.lookup p byPath)) named))
  where
    parseLine : String -> Maybe (String, String)
    parseLine l = case words l of
                    (h :: p :: _) => Just (p, h)
                    _ => Nothing

||| Store one row in staging. False when it could not be written.
export
stageRow : (memoDir, m, key, json : String) -> IO Bool
stageRow dir m key json = do
  _ <- system ("mkdir -p " ++ shq (dir ++ "/staging/" ++ rowDirName m))
  r <- writeFile (stagingRowPath dir m key) json
  pure (either (const False) (const True) r)

||| A committed row's JSON, if there is one.
export
loadRow : (memoDir, m, key : String) -> IO (Maybe String)
loadRow dir m key = do
  Right c <- readFile (rowPath dir m key)
    | Left _ => pure Nothing
  pure (Just c)

||| Start a run: drop whatever an earlier, unfinished run left in staging.
export
clearStaging : (memoDir : String) -> IO ()
clearStaging dir = ignore $ system ("rm -rf " ++ shq (dir ++ "/staging") ++ " && mkdir -p " ++ shq dir)

||| Move the staged rows into the memo. Called only after the whole run succeeded.
export
commitStaging : (memoDir : String) -> IO Bool
commitStaging dir = do
  rc <- system ("cd " ++ shq dir ++ " && if [ -d staging ]; then mkdir -p rows && cp -R staging/. rows/ && rm -rf staging; fi")
  pure (rc == 0)

||| Forget every row (a stale row was detected).
export
discardMemo : (memoDir : String) -> IO ()
discardMemo dir = ignore $ system ("rm -rf " ++ shq (dir ++ "/rows") ++ " " ++ shq (dir ++ "/staging"))

||| A module's source text, "" when unreadable (its hash is then already missing).
readSource : (String -> String) -> String -> IO (String, String)
readSource srcPathOf m = do
  r <- readFile (srcPathOf m)
  pure (m, either (const "") id r)

||| The row keys of `modules`, from their sources and import closures.
||| Nothing when a source or the tool key could not be read: a key that lost an
||| ingredient still looks like a key, and would serve rows it does not cover.
export
staticMemoKeys : (memoDir : String) -> (srcPathOf : String -> String) -> (modules : List String)
              -> IO (Either String (String, SortedMap String String))
staticMemoKeys dir srcPathOf modules = do
  let scratch = dir ++ "/scratch"
  _ <- system ("rm -rf " ++ shq scratch ++ " && mkdir -p " ++ shq scratch)
  tool <- staticMemoToolKey scratch
  if tool == ""
     then pure (Left "the tool key could not be computed")
     else do
       srcHash <- sha256Files scratch (map (\m => (m, srcPathOf m)) modules)
       let missing = filter (\m => isNothing (lookup m srcHash)) modules
       case missing of
         (m :: _) => pure (Left ("no source hash for " ++ m ++ " (" ++ srcPathOf m ++ ")"))
         [] => do
           let known = fromList modules
           sources <- traverse (readSource srcPathOf) modules
           let imports = fromList (map (\(m, s) => (m, importsOfSource known s)) sources)
           let materials = map (\m => (m, scratch ++ "/key-" ++ m ++ ".txt")) modules
           writes <- traverse (\(m, p) => writeFile p (moduleKeyMaterial tool srcHash (importClosure imports m) m)) materials
           if any isLeft writes
              then pure (Left "a key material file could not be written")
              else do
                keys <- sha256Files scratch materials
                let noKey = filter (\m => isNothing (lookup m keys)) modules
                case noKey of
                  (m :: _) => pure (Left ("no key for " ++ m))
                  [] => pure (Right (tool, keys))
