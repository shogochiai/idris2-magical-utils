||| html-verifier <baseUrl> <entryPath> [<entryPath> ...]
|||   [--require <path> [<path> ...]] [--require-ready <label> [<label> ...]]
|||
||| Crawls a deployed static site starting from the given entry paths
||| (resolved against baseUrl), following every internal link discovered.
||| Any path after a "--require" separator is NOT a crawl seed -- it's a page
||| that must be DISCOVERED via crawling from the real entry points, catching
||| "the page loads fine but nothing on the site actually links to it" (a
||| seed-as-entry-point would make that check vacuously pass).
||| Any label after a "--require-ready" separator (e.g. "Android app", "CLI
||| install" -- WebsiteRender's own English conduit heading text) asserts
||| that family's conduit MUST be Ready (a working download, not a "coming
||| soon" placeholder) on some crawled page -- catching a genuinely BUILT
||| artifact that was simply never included in a release's unit list, which
||| the digest-lie check alone cannot see (a pending conduit makes no claim
||| to verify, so it never "lies" -- it just silently omits something).
||| "--require-ready", if given, MUST come AFTER "--require" (fixed order,
||| both optional).
||| Exits 0 iff every discovered internal link resolved to a page that loaded
||| successfully, every required path was reached, every download conduit's
||| declared digest matched its actual bytes, AND every required-ready family
||| is genuinely Ready; exits 1 otherwise.
module Main

import HtmlVerifier.Crawl
import System
import Data.String
import Data.List

usage : IO ()
usage = putStrLn "usage: html-verifier <baseUrl> <entryPath> [<entryPath> ...] [--require <path> [<path> ...]] [--require-ready <label> [<label> ...]]"

||| Splits the positional arg list into (entrySeeds, requiredPaths,
||| requiredReadyLabels) on "--require" then "--require-ready", in that FIXED
||| order (either or both may be absent).
splitArgs : List String -> (List String, List String, List String)
splitArgs args =
  case break (== "--require") args of
    (seeds, _ :: afterRequire) =>
      case break (== "--require-ready") afterRequire of
        (requiredPaths, _ :: readyLabels) => (seeds, requiredPaths, readyLabels)
        (requiredPaths, [])                => (seeds, requiredPaths, [])
    (seeds, []) =>
      -- No "--require" at all: "--require-ready" (if present) still applies,
      -- with an empty requiredPaths list.
      case break (== "--require-ready") seeds of
        (onlySeeds, _ :: readyLabels) => (onlySeeds, [], readyLabels)
        (onlySeeds, [])                => (onlySeeds, [], [])

main : IO ()
main = do
  args <- getArgs
  case args of
    (_ :: baseUrl :: rest@(_ :: _)) => do
      let (entryPaths, requiredPaths, requiredReadyLabels) = splitArgs rest
      if null entryPaths
        then do usage; exitFailure
        else do
          let entryUrls = map (\p => baseUrl ++ "/" ++ p) entryPaths
          report <- crawlSite baseUrl entryUrls requiredPaths requiredReadyLabels
          putStrLn (renderCrawlReport report)
          if null report.brokenLinks && null report.requiredMissing
             && null report.conduitLies && null report.notReadyFamilies
            then exitSuccess
            else exitFailure
    _ => do usage; exitFailure
