||| Deployed-site link/render integrity crawler.
|||
||| This measures something DIFFERENT from V8/path coverage: a generated
||| static site (e.g. EtherClaw's `site-gen`) has no executable code in the
||| deployed artifact itself, so "coverage" (which branch executed) doesn't
||| apply to it at all. What DOES apply -- and was previously only checked by
||| hand (curl + eyeballing) -- is: does every internal link on the deployed
||| site actually resolve to a real page, and does every page load
||| successfully (as opposed to a silent 404-fallback serving the wrong
||| content, which is a real bug this project hit once: an IC asset
||| canister's 404 fallback served the landing page instead of erroring, so
||| every dead link LOOKED like it worked).
|||
||| Generic by design: it discovers the page graph by crawling from the given
||| entry URLs, rather than hardcoding an expected page list -- the deployed
||| site's exact set of pages changes every release, but the crawler needs no
||| changes to keep working, matching a static-site-generator's structure
||| being stable release over release.
module HtmlVerifier.Crawl

import Playwright.Sync
import Data.String
import Data.List
import Data.Maybe

%default covering

public export
record PageResult where
  constructor MkPageResult
  url    : String
  status : Int
  title  : String

||| One WebsiteRender `.entry` block as seen live in the deployed page, plus
||| the RESULT of independently fetching and re-digesting whatever its
||| download button links to. `href = Nothing` for a `.entry.pending` block
||| (NotBuilt/Unpinned/DigestMismatch -- WebsiteRender's `unitStatusHtml`
||| never emits a download button for those) and for a Ready conduit with no
||| download surface at all (dfx/evm show a controller/contract reference,
||| not a link). `declaredDigest`/`actualDigest`/`fetchOk` are meaningless
||| (empty/False) when `href` is Nothing -- there is nothing to verify.
||| `headingEn` is the conduit's English family label (e.g. "Android app",
||| "CLI install") -- present for both Ready and pending entries, since
||| WebsiteRender always names the family regardless of status; the ONLY way
||| to identify WHICH family a pending entry belongs to (its CSS class is the
||| generic "entry pending", carrying no family tag).
public export
record ConduitCheck where
  constructor MkConduitCheck
  pending        : Bool
  href           : Maybe String
  declaredDigest : String
  actualDigest   : String
  fetchOk        : Bool
  headingEn      : String

||| A Ready conduit's page text CLAIMS its download resolves to bytes with
||| `declaredDigest` (GlobalRegistry's own sha256(base64(bytes)) convention,
||| computed identically here). It is a LIE iff it has a download link but
||| that link either failed to fetch, or fetched bytes whose real digest
||| disagrees with the claim -- drift the page itself has no way to
||| self-detect, since it only ever prints what it was told to print.
export
isConduitLie : ConduitCheck -> Bool
isConduitLie c = isJust c.href && (not c.fetchOk || c.declaredDigest /= c.actualDigest)

||| Given the family labels a caller REQUIRES to be Ready (e.g. ["Android
||| app", "CLI install"] because those artifacts genuinely exist on disk),
||| return exactly the labels for which no visited page showed a
||| NON-pending conduit with that heading -- covering both "found, but still
||| pending" and "never found at all" in one check. This is the "no silent
||| gaps" check: a real bug this session found live -- a project can have a
||| genuinely BUILT artifact that was simply never included in a release's
||| unit list, so the deployed page shows "coming soon" forever even though
||| nothing is actually missing except the release step, and the earlier
||| conduit-digest-lie check (which only inspects READY conduits) has no way
||| to catch this: a pending conduit makes no claim to verify, so it never
||| "lies" -- it just silently omits something that should be there.
export
missingReadyFamilies : (requiredReadyLabels : List String) -> (allConduits : List ConduitCheck) -> List String
missingReadyFamilies requiredReadyLabels allConduits =
  filter (\lbl => not (any (\c => c.headingEn == lbl && not c.pending) allConduits)) requiredReadyLabels

public export
record CrawlReport where
  constructor MkCrawlReport
  visited          : List PageResult
  brokenLinks      : List (String, String)  -- (fromUrl, targetUrl) where targetUrl's status was an error
  requiredMissing  : List String  -- required URLs never DISCOVERED while crawling from the real
                                   -- entry points -- distinct from brokenLinks: a page can be
                                   -- perfectly loadable yet unreachable because nothing on the
                                   -- site actually links to it. This is the check that catches
                                   -- "the page exists but the homepage never introduces it" --
                                   -- seeding a page directly as a crawl entry point (instead of
                                   -- discovering it via navigation) would make this check
                                   -- vacuously pass, so callers needing this guarantee MUST NOT
                                   -- also list the same path as an entry URL.
  conduitLies      : List (String, String, String, String)  -- (pageUrl, href, declaredDigest, actualDigest)
                                                             -- for every isConduitLie across every
                                                             -- visited page -- the "no lies" check:
                                                             -- a Ready download button's digest claim
                                                             -- must match what's actually served.
  notReadyFamilies : List String  -- missingReadyFamilies' result -- a family the caller declared
                                   -- MUST be Ready (a genuinely available artifact), but the
                                   -- deployed page shows it pending (or not present at all).

||| An internal link is either a same-origin absolute URL or a relative path
||| (SiteGen's own pages are flat filenames like "docs__docs_README.html",
||| never absolute). External absolute links to a DIFFERENT origin, mailto:,
||| and pure in-page "#anchor" links are never crawled.
export
isInternalLink : (baseUrl : String) -> (href : String) -> Bool
isInternalLink baseUrl href =
  if href == "" || isPrefixOf "#" href || isPrefixOf "mailto:" href
    then False
    else if isPrefixOf "http://" href || isPrefixOf "https://" href
      then isPrefixOf baseUrl href
      else True

||| Resolve a possibly-relative href against baseUrl, stripping any
||| "#fragment" (the crawler treats a page as a unit; fragments don't change
||| which page loads).
export
resolveUrl : (baseUrl : String) -> (href : String) -> String
resolveUrl baseUrl href =
  let hrefNoFrag = pack (fst (break (== '#') (unpack href)))
  in if isPrefixOf "http://" hrefNoFrag || isPrefixOf "https://" hrefNoFrag
       then hrefNoFrag
       else if isPrefixOf "/" hrefNoFrag
         then baseUrl ++ hrefNoFrag
         else baseUrl ++ "/" ++ hrefNoFrag

||| Minimal parser for a JSON array-of-strings (the shape
||| `JSON.stringify(Array.from(...).map(a=>a.getAttribute('href')))`
||| always produces). Does not handle escaped quotes inside an href -- hrefs
||| are URLs and never legitimately contain a literal '"'.
export
parseJsonStringArray : String -> List String
parseJsonStringArray s = go (unpack (trim s))
  where
    go : List Char -> List String
    go [] = []
    go ('"' :: rest) =
      case break (== '"') rest of
        (content, _ :: rest') => pack content :: go rest'
        (content, [])         => [pack content]
    go (_ :: rest) = go rest

||| A visited page's status counts as ok iff it's a genuine HTTP response in
||| the 2xx/3xx range (a 4xx/5xx, or -1 for a navigation-level failure like
||| DNS/connection refused, is broken).
export
isOkStatus : Int -> Bool
isOkStatus status = status >= 200 && status < 400

||| Find the first occurrence of `needle` in `haystack`, returning everything
||| immediately after it, or Nothing if not found.
afterSubstr : (needle, haystack : String) -> Maybe String
afterSubstr needle haystack = go (unpack haystack)
  where
    go : List Char -> Maybe String
    go [] = Nothing
    go cs@(_ :: rest) =
      if isPrefixOf needle (pack cs)
        then Just (pack (drop (length (unpack needle)) cs))
        else go rest

||| Brace-depth-tracking split of a flat JSON array's inner content into its
||| individual "{...}" object substrings -- a comma inside an object's own
||| string values never causes a spurious split. Mirrors the identical
||| pattern in EtherClaw.Commands.WebsiteDeploy.splitJsonObjects (a different
||| repo/package, so duplicated rather than cross-imported: small,
||| self-contained, no shared abstraction worth the coupling).
export
splitJsonObjects : String -> List String
splitJsonObjects s = reverse (go (unpack s) 0 [] [])
  where
    go : List Char -> Nat -> List Char -> List String -> List String
    go [] _ acc objs = objs
    go ('{' :: rest) depth acc objs = go rest (S depth) ('{' :: acc) objs
    go ('}' :: rest) (S Z) acc objs = go rest 0 [] (pack (reverse ('}' :: acc)) :: objs)
    go ('}' :: rest) (S depth) acc objs = go rest depth ('}' :: acc) objs
    go ('}' :: rest) Z acc objs = go rest Z acc objs
    go (c :: rest) depth acc objs = go rest depth (if depth == 0 then acc else c :: acc) objs

||| Everything after `"href":`, unquoted, or Nothing for a JSON `null` (the
||| shape visit-page.mjs emits for a `.entry.pending` block with no button).
parseHrefField : String -> Maybe String
parseHrefField obj =
  case afterSubstr "\"href\":" obj of
    Nothing => Nothing
    Just after =>
      let t = trim after in
      if isPrefixOf "null" t then Nothing
      else case unpack t of
             ('"' :: rest) => case break (== '"') rest of
                                 (content, _) => Just (pack content)
             _ => Nothing

||| A string field's bare value (unquoted), "" if absent.
extractStringField : (key : String) -> String -> String
extractStringField key obj =
  maybe "" (\after => pack (fst (break (== '"') (unpack after))))
        (afterSubstr ("\"" ++ key ++ "\":\"") obj)

||| A JSON bare-word boolean field (`true`/`false`, unquoted), False if absent
||| or malformed.
extractBoolField : (key : String) -> String -> Bool
extractBoolField key obj =
  maybe False (isPrefixOf "true" . trim) (afterSubstr ("\"" ++ key ++ "\":") obj)

||| Parse one conduit object's fields into a ConduitCheck.
export
parseConduitCheck : String -> ConduitCheck
parseConduitCheck obj =
  MkConduitCheck (extractBoolField "pending" obj)
                 (parseHrefField obj)
                 (extractStringField "declaredDigest" obj)
                 (extractStringField "actualDigest" obj)
                 (extractBoolField "fetchOk" obj)
                 (extractStringField "headingEn" obj)

||| Parse visit-page.mjs's `"conduits":[{...},...]` array into its
||| ConduitCheck rows, [] if the page had no `.entry` blocks at all (every
||| page except the derived landing page).
export
parseConduitChecks : String -> List ConduitCheck
parseConduitChecks s =
  maybe [] (map parseConduitCheck . splitJsonObjects) (afterSubstr "\"conduits\":[" s)

||| Parse visit-page.mjs's fixed JSON shape:
||| {"status":<int>,"title":<string>,"links":[<string>,...],"conduits":[...]}.
||| A malformed or unexpected response (should never happen -- the helper
||| always emits valid JSON) degrades to status=-1, matching a navigation
||| failure.
export
parseVisitResult : String -> (Int, String, List String)
parseVisitResult s =
  let statusText = maybe "" id (afterSubstr "\"status\":" s)
      status = fromMaybe (-1) (parsePositive (pack (takeWhile (\c => isDigit c || c == '-') (unpack statusText))))
      title  = maybe "" (\after => pack (fst (break (== '"') (unpack after)))) (afterSubstr "\"title\":\"" s)
      links  = maybe [] parseJsonStringArray (afterSubstr "\"links\":" s)
  in (status, title, links)
  where
    parsePositive : String -> Maybe Int
    parsePositive t = if t == "" then Nothing else Just (cast t)

visitOne : (baseUrl, url : String) -> IO (PageResult, List String, List ConduitCheck)
visitOne baseUrl url = do
  raw <- visitPageSync url
  let (status, title, hrefs) = parseVisitResult raw
  let internalTargets = map (resolveUrl baseUrl) (filter (isInternalLink baseUrl) hrefs)
  let conduits = parseConduitChecks raw
  pure (MkPageResult url status title, internalTargets, conduits)

||| An edge (fromUrl, targetUrl) is broken iff no visited PageResult with
||| url == targetUrl has an ok status -- covering both "target returned an
||| error status" and "target was never reached at all". Pure and directly
||| testable without a real Page (unlike the IO-driven crawl loop itself).
export
deriveBrokenLinks : (results : List PageResult) -> (edges : List (String, String)) -> List (String, String)
deriveBrokenLinks results edges =
  filter (\(_, tgt) => not (any (\r => r.url == tgt && isOkStatus r.status) results)) edges

||| Whether `url` should be skipped as already-visited/already-queued. Pure
||| and directly testable without a real Page.
export
alreadySeen : (visitedUrls : List String) -> (url : String) -> Bool
alreadySeen visitedUrls url = elem url visitedUrls

||| Turn one page's ConduitCheck rows into its conduitLies rows, tagged with
||| the page they were seen on.
lieRowsFor : (pageUrl : String) -> List ConduitCheck -> List (String, String, String, String)
lieRowsFor pageUrl checks =
  [ (pageUrl, href, c.declaredDigest, c.actualDigest)
  | c <- checks, isConduitLie c
  , let href = fromMaybe "" c.href ]

||| BFS crawl starting from `entryUrls`, following every internal link
||| discovered, visiting each distinct URL exactly once. `edges` accumulates
||| every (fromUrl, targetUrl) discovery so brokenLinks can name WHICH page(s)
||| referenced a target that turned out to error; `lies` accumulates every
||| conduit whose declared download digest disagreed with its actual bytes;
||| `allConduits` accumulates every ConduitCheck seen on any visited page, the
||| raw material missingReadyFamilies checks against.
crawlLoop : (baseUrl : String) -> (queue : List String) ->
            (visitedUrls : List String) -> (results : List PageResult) ->
            (edges : List (String, String)) ->
            (lies : List (String, String, String, String)) ->
            (allConduits : List ConduitCheck) -> IO (CrawlReport, List ConduitCheck)
crawlLoop baseUrl [] visitedUrls results edges lies allConduits =
  pure (MkCrawlReport (reverse results) (deriveBrokenLinks results edges) [] lies [], allConduits)
crawlLoop baseUrl (url :: rest) visitedUrls results edges lies allConduits =
  if elem url visitedUrls
    then crawlLoop baseUrl rest visitedUrls results edges lies allConduits
    else do
      (result, targets, conduits) <- visitOne baseUrl url
      let newEdges = map (\t => (url, t)) targets
      let newLies = lieRowsFor url conduits
      let newQueue = rest ++ filter (\t => not (elem t visitedUrls)) targets
      crawlLoop baseUrl newQueue (url :: visitedUrls) (result :: results)
                (edges ++ newEdges) (lies ++ newLies) (allConduits ++ conduits)

||| Crawl a deployed site starting from `entryUrls` (already-absolute URLs --
||| normally just the real homepage, so the crawl reflects what a real
||| visitor can actually reach by clicking), visiting every internally-linked
||| page exactly once and reporting:
|||   - brokenLinks: a target whose page never loaded successfully (a genuine
|||     broken link, or the "silent 404-fallback" failure mode)
|||   - requiredMissing: a path in `requiredPaths` that crawling never
|||     discovered -- the site may serve it fine at that URL, but nothing
|||     reachable from `entryUrls` links to it, so a real visitor would never
|||     find it. Resolved against `baseUrl` the same way a discovered href is.
|||   - notReadyFamilies: missingReadyFamilies over `requiredReadyLabels` (see
|||     its own doc comment) -- a family the caller declares MUST be Ready
|||     (because it genuinely has a built artifact), but the deployed page
|||     shows pending or absent.
export
crawlSite : (baseUrl : String) -> (entryUrls : List String) -> (requiredPaths : List String) ->
            (requiredReadyLabels : List String) -> IO CrawlReport
crawlSite baseUrl entryUrls requiredPaths requiredReadyLabels = do
  (report, allConduits) <- crawlLoop baseUrl entryUrls [] [] [] [] []
  let visitedUrls = map url report.visited
      requiredUrls = map (resolveUrl baseUrl) requiredPaths
      missing = filter (\u => not (elem u visitedUrls)) requiredUrls
      notReady = missingReadyFamilies requiredReadyLabels allConduits
  pure ({ requiredMissing := missing, notReadyFamilies := notReady } report)

||| Human-readable report. Never claims success while brokenLinks,
||| requiredMissing, conduitLies, or notReadyFamilies is non-empty (all four
||| are structurally tied to the same fields the verdict reads, not asserted
||| independently).
export
renderCrawlReport : CrawlReport -> String
renderCrawlReport report =
  let header = "# Site crawl report\n\nvisited " ++ show (length report.visited) ++ " page(s)\n"
      brokenLines = map (\(from, tgt) => "  BROKEN: " ++ from ++ " -> " ++ tgt) report.brokenLinks
      missingLines = map (\u => "  UNREACHABLE (required, but no crawled page links to it): " ++ u) report.requiredMissing
      lieLines = map (\(pg, href, decl, act) =>
                        "  DIGEST LIE on " ++ pg ++ " -> " ++ href
                          ++ " (declared=" ++ decl ++ " actual=" ++ act ++ ")")
                      report.conduitLies
      notReadyLines = map (\lbl => "  NOT READY (required to be Ready, but pending or absent): " ++ lbl) report.notReadyFamilies
      ok = null report.brokenLinks && null report.requiredMissing && null report.conduitLies && null report.notReadyFamilies
      verdict = if ok
                  then "PASS: every discovered internal link resolved to a successfully-loaded page, every required page was reachable by crawling, every download conduit's declared digest matched its actual bytes, and every required-ready family is genuinely Ready."
                  else "FAIL: " ++ show (length report.brokenLinks) ++ " broken internal link(s), "
                       ++ show (length report.requiredMissing) ++ " required page(s) unreachable, "
                       ++ show (length report.conduitLies) ++ " conduit digest lie(s), "
                       ++ show (length report.notReadyFamilies) ++ " family(ies) not ready:"
  in header ++ "\n" ++ verdict ++ "\n" ++ unlines (brokenLines ++ missingLines ++ lieLines ++ notReadyLines)
