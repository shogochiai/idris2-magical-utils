||| Configuration file parser for idris2-coverage
||| Supports minimal TOML subset for .idris2-cov.toml
module Coverage.Config

import Coverage.DumpcasesParser
import Data.List
import Data.List1
import Data.String
import System.File

%default partial

-- =============================================================================
-- TOML Subset Parser
-- =============================================================================

||| Parse a simple array like ["foo", "bar", "baz"]
parseStringArray : String -> List String
parseStringArray s =
  let trimmed = trim s
      -- Remove [ and ]
      inner = if isPrefixOf "[" trimmed && isSuffixOf "]" trimmed
              then substr 1 (length trimmed `minus` 2) trimmed
              else trimmed
      -- Split by comma
      parts = forget $ split (== ',') inner
      -- Extract quoted strings
      extractQuoted : String -> Maybe String
      extractQuoted part =
        let t = trim part
        in if isPrefixOf "\"" t && isSuffixOf "\"" t
           then Just $ substr 1 (length t `minus` 2) t
           else if isPrefixOf "'" t && isSuffixOf "'" t
                then Just $ substr 1 (length t `minus` 2) t
                else if t == "" then Nothing else Just t
  in mapMaybe extractQuoted parts

||| Parse config file content
||| Format:
||| ```toml
||| [exclusions]
||| module_prefixes = ["Foo.Internal", "Generated"]
||| packages = ["mylib", "otherlib"]
||| functions = ["Module.untestableFunc", "Other.sideEffectOnly"]
||| ```
covering
public export
parseConfigFile : String -> ExclusionConfig
parseConfigFile content =
  let ls = lines content
      prefixes = findArrayValue "module_prefixes" ls
      packages = findArrayValue "packages" ls
      functions = findArrayValue "functions" ls
  in MkExclusionConfig prefixes packages functions
  where
    -- Drop leading '=' and whitespace
    dropEqualsAndTrim : String -> String
    dropEqualsAndTrim s =
      let trimmed = trim s
      in if isPrefixOf "=" trimmed
         then trim $ pack $ drop 1 $ unpack trimmed  -- safe alternative to strTail
         else trimmed

    findArrayValue : String -> List String -> List String
    findArrayValue key lns =
      let matching = filter (isPrefixOf (key ++ " ")) (map trim lns)
                  ++ filter (isPrefixOf (key ++ "=")) (map trim lns)
      in case matching of
           [] => []
           (line :: _) =>
             let afterKey = snd $ break (== '=') line
                 valueStr = dropEqualsAndTrim afterKey
             in parseStringArray valueStr

-- =============================================================================
-- Config File Loading
-- =============================================================================

||| Default config file name
public export
defaultConfigFile : String
defaultConfigFile = ".idris2-cov.toml"

||| Try to load config from project directory
covering
public export
loadConfig : String -> IO ExclusionConfig
loadConfig projectDir = do
  let configPath = projectDir ++ "/" ++ defaultConfigFile
  Right content <- readFile configPath
    | Left _ => pure emptyExclusionConfig
  pure $ parseConfigFile content

||| Load config and merge with ipkg depends
||| Package names from ipkg are added to the config's package list
covering
public export
loadConfigWithDepends : String -> List String -> IO ExclusionConfig
loadConfigWithDepends projectDir ipkgDepends = do
  baseConfig <- loadConfig projectDir
  -- Merge ipkg depends with config packages (dedup)
  let allPackages = nub (baseConfig.packageNames ++ ipkgDepends)
  pure $ { packageNames := allPackages } baseConfig
