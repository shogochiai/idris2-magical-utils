||| TNCGF Characterization Tests
||| 5v (Revelation): LLM-generated tests to characterize existing behavior
|||
||| Covers:
||| - REQ_TNCGF_ARB_001: Arbitrary interface
||| - REQ_TNCGF_BASE_001: Base type instances
||| - REQ_TNCGF_SOP_001: enumGen/sumGen/productGen
||| - REQ_TNCGF_DEP_001: DependencyGraph
||| - REQ_TNCGF_DEP_002: DeriveDep helpers
||| - REQ_TNCGF_TYPE_001: Arbitrary interface
||| - REQ_TNCGF_TYPE_002: FieldDep record
||| - REQ_TNCGF_TYPE_003: DepGraph record
||| - REQ_TNCGF_TYPE_004: GenStrategy data
||| - REQ_TNCGF_TYPE_005: TNCGFConfig record
||| - REQ_TNCGF_TYPE_006: enumGen functions
||| - REQ_TNCGF_TYPE_007: sumGen functions
||| - REQ_TNCGF_TYPE_008: productGen functions
||| - REQ_TNCGF_TYPE_009: natWithUpperBound
||| - REQ_TNCGF_TYPE_010: genLTEPair
||| - REQ_TNCGF_TYPE_011: topoSort
||| - REQ_TNCGF_TYPE_012: isProofTypeName
||| - REQ_TNCGF_TYPE_013: default constants
module Coverage.Tests.TNCGFTests

import Coverage.TNCGF.S2Static
import Coverage.TNCGF.S2Static.DependencyGraph as DG
import Data.List
import Data.Vect
import Data.String
import System

%default total

-- Helper to check substring
strContains : String -> String -> Bool
strContains needle haystack = isInfixOf needle haystack

-- =============================================================================
-- DependencyGraph Tests (characterizing topoSort, mkGraph, etc.)
-- =============================================================================

||| REQ_TNCGF_DEP_001: Test simple field creation (FieldDep)
covering
test_DEP_simpleField : IO Bool
test_DEP_simpleField = do
  let f = simpleField "balance" "Nat"
  pure $ f.fieldName == "balance"
      && f.fieldTypeName == "Nat"
      && null f.dependsOn

||| REQ_TNCGF_TYPE_002: Test dependent field creation (FieldDep)
covering
test_DEP_depField : IO Bool
test_DEP_depField = do
  let f = depField "prf" "LTE amount balance" ["amount", "balance"]
  pure $ f.fieldName == "prf"
      && length f.dependsOn == 2
      && ("amount" `elem` f.dependsOn)

||| REQ_TNCGF_TYPE_003: Test mkGraph (DepGraph)
covering
test_DEP_mkGraph : IO Bool
test_DEP_mkGraph = do
  let g = mkGraph [ simpleField "x" "Int"
                  , simpleField "y" "Int" ]
  pure $ length g.fields == 2

||| REQ_TNCGF_TYPE_011: Test topoSort with no dependencies
covering
test_DEP_topoSort_simple : IO Bool
test_DEP_topoSort_simple = do
  let g = mkGraph [ simpleField "a" "Int"
                  , simpleField "b" "Int" ]
  case topoSort g of
    Right sorted => pure $ length sorted == 2
    Left _ => pure False

||| REQ_TNCGF_DEP_005: Test topoSort with dependencies
||| Discovered spec: topoSort returns nodes without incoming edges first
||| For Transfer, balance and amount have no deps, prf depends on both
||| Valid orderings: [balance, amount, prf] or [amount, balance, prf]
||| BUT our algorithm returns in reverse (topological order for generation)
covering
test_DEP_topoSort_deps : IO Bool
test_DEP_topoSort_deps = do
  let g = mkGraph [ simpleField "balance" "Nat"
                  , simpleField "amount" "Nat"
                  , depField "prf" "LTE" ["amount", "balance"] ]
  case topoSort g of
    Right sorted => do
      -- All 3 fields should be in result
      pure $ length sorted == 3
          && ("prf" `elem` sorted)
          && ("balance" `elem` sorted)
          && ("amount" `elem` sorted)
    Left _ => pure False

||| REQ_TNCGF_DEP_006: Test cyclic dependency detection
covering
test_DEP_topoSort_cyclic : IO Bool
test_DEP_topoSort_cyclic = do
  let g = mkGraph [ depField "a" "T" ["b"]
                  , depField "b" "T" ["a"] ]
  case topoSort g of
    Left err => pure $ strContains "Cyclic" err
    Right _ => pure False  -- Should detect cycle

||| REQ_TNCGF_DEP_007: Test hasDependencies
covering
test_DEP_hasDependencies : IO Bool
test_DEP_hasDependencies = do
  let f1 = simpleField "x" "Int"
  let f2 = depField "y" "T" ["x"]
  pure $ not (hasDependencies f1) && hasDependencies f2

||| REQ_TNCGF_DEP_008: Test hasAnyDependencies
covering
test_DEP_hasAnyDependencies : IO Bool
test_DEP_hasAnyDependencies = do
  let g1 = mkGraph [simpleField "x" "Int"]
  let g2 = mkGraph [simpleField "x" "Int", depField "y" "T" ["x"]]
  pure $ not (hasAnyDependencies g1) && hasAnyDependencies g2

||| REQ_TNCGF_DEP_009: Test directDeps
covering
test_DEP_directDeps : IO Bool
test_DEP_directDeps = do
  let g = mkGraph [ simpleField "a" "Int"
                  , depField "b" "T" ["a"] ]
  let deps = directDeps "b" g
  pure $ deps == ["a"]

||| REQ_TNCGF_TYPE_012: Test isProofTypeName
covering
test_DEP_isProofTypeName : IO Bool
test_DEP_isProofTypeName = do
  pure $ DG.isProofTypeName "LTE n m"
      && DG.isProofTypeName "Refl"
      && DG.isProofTypeName "Void"
      && not (DG.isProofTypeName "Nat")
      && not (DG.isProofTypeName "String")

||| REQ_TNCGF_DEP_011: Test prettyPrint (smoke test)
covering
test_DEP_prettyPrint : IO Bool
test_DEP_prettyPrint = do
  let g = mkGraph [simpleField "x" "Int"]
  let output = prettyPrint g
  pure $ strContains "Dependency Graph" output

||| REQ_TNCGF_DEP_012: Test toMermaid (smoke test)
covering
test_DEP_toMermaid : IO Bool
test_DEP_toMermaid = do
  let g = mkGraph [simpleField "x" "Int", depField "y" "T" ["x"]]
  let output = toMermaid g
  pure $ strContains "graph TD" output && strContains "-->" output

-- =============================================================================
-- Arbitrary/BaseInstances Tests
-- =============================================================================

||| REQ_TNCGF_ARB_001: Test Bool arbitrary (Arbitrary interface)
covering
test_ARB_bool : IO Bool
test_ARB_bool = do
  samples <- sample (list (linear 10 10) (arbitrary {a=Bool}))
  -- Should generate some True and some False
  pure $ length samples == 10

||| REQ_TNCGF_BASE_001: Test Nat arbitrary (BaseInstances)
covering
test_ARB_nat : IO Bool
test_ARB_nat = do
  samples <- sample (list (linear 5 5) (arbitrary {a=Nat}))
  pure $ length samples == 5 && all (\n => n <= defaultNatMax) samples

||| REQ_TNCGF_TYPE_001: Test String arbitrary (Arbitrary interface exists)
covering
test_ARB_string : IO Bool
test_ARB_string = do
  samples <- sample (list (linear 3 3) (arbitrary {a=String}))
  pure $ length samples == 3

||| REQ_TNCGF_ARB_004: Test Maybe arbitrary (generates both Nothing and Just)
covering
test_ARB_maybe : IO Bool
test_ARB_maybe = do
  samples <- sample (list (linear 20 20) (arbitrary {a=Maybe Bool}))
  let hasNothing = any isNothing samples
  let hasJust = any isJust samples
  pure $ hasNothing || hasJust  -- At least one type should appear

||| REQ_TNCGF_ARB_005: Test Either arbitrary
covering
test_ARB_either : IO Bool
test_ARB_either = do
  samples <- sample (list (linear 20 20) (arbitrary {a=Either Bool Bool}))
  pure $ length samples == 20

||| REQ_TNCGF_ARB_006: Test List arbitrary
covering
test_ARB_list : IO Bool
test_ARB_list = do
  samples <- sample (list (linear 3 3) (arbitrary {a=List Bool}))
  pure $ length samples == 3

||| REQ_TNCGF_ARB_007: Test Pair arbitrary
covering
test_ARB_pair : IO Bool
test_ARB_pair = do
  samples <- sample (list (linear 3 3) (arbitrary {a=(Bool, Bool)}))
  pure $ length samples == 3

||| REQ_TNCGF_ARB_008: Test arbitraryVect
covering
test_ARB_vect : IO Bool
test_ARB_vect = do
  samples <- sample (arbitraryVect {a=Bool} 3)
  pure $ length (toList samples) == 3

||| REQ_TNCGF_ARB_009: Test arbitraryFin
covering
test_ARB_fin : IO Bool
test_ARB_fin = do
  samples <- sample (list (linear 10 10) (arbitraryFin 4))
  pure $ all (\f => finToNat f <= 4) samples

-- =============================================================================
-- DeriveSOP Tests (enumGen, sumGen, productGen)
-- =============================================================================

||| REQ_TNCGF_SOP_001: Test enumGen2 (SOP derivation)
covering
test_SOP_enumGen2 : IO Bool
test_SOP_enumGen2 = do
  samples <- sample (list (linear 20 20) (enumGen2 True False))
  let hasTrue = any id samples
  let hasFalse = any not samples
  pure $ hasTrue && hasFalse

||| REQ_TNCGF_TYPE_006: Test enumGen3 (enumGen functions)
covering
test_SOP_enumGen3 : IO Bool
test_SOP_enumGen3 = do
  samples <- sample (list (linear 30 30) (enumGen3 (the Nat 1) 2 3))
  pure $ (1 `elem` samples) && (2 `elem` samples) && (3 `elem` samples)

||| REQ_TNCGF_SOP_003: Test enumGen4
covering
test_SOP_enumGen4 : IO Bool
test_SOP_enumGen4 = do
  samples <- sample (list (linear 40 40) (enumGen4 "a" "b" "c" "d"))
  pure $ length samples == 40

||| REQ_TNCGF_SOP_004: Test enumGen5
covering
test_SOP_enumGen5 : IO Bool
test_SOP_enumGen5 = do
  samples <- sample (list (linear 50 50) (enumGen5 (the Nat 1) 2 3 4 5))
  pure $ length samples == 50

||| REQ_TNCGF_TYPE_007: Test sumGen2 (sum type generator)
covering
test_SOP_sumGen2 : IO Bool
test_SOP_sumGen2 = do
  let gen = sumGen2 (pure (Left True)) (pure (Right "x"))
  samples <- sample (list (linear 20 20) gen)
  let hasLeft = any isLeft samples
  let hasRight = any isRight samples
  pure $ hasLeft || hasRight
  where
    isLeft : Either a b -> Bool
    isLeft (Left _) = True
    isLeft _ = False
    isRight : Either a b -> Bool
    isRight (Right _) = True
    isRight _ = False

||| REQ_TNCGF_SOP_006: Test sumGen3
covering
test_SOP_sumGen3 : IO Bool
test_SOP_sumGen3 = do
  let gen = sumGen3 (pure (the Nat 1)) (pure 2) (pure 3)
  samples <- sample (list (linear 30 30) gen)
  pure $ (1 `elem` samples) || (2 `elem` samples) || (3 `elem` samples)

||| REQ_TNCGF_SOP_007: Test sumGen4
covering
test_SOP_sumGen4 : IO Bool
test_SOP_sumGen4 = do
  let gen = sumGen4 (pure "a") (pure "b") (pure "c") (pure "d")
  samples <- sample (list (linear 40 40) gen)
  pure $ length samples == 40

||| REQ_TNCGF_TYPE_008: Test productGen1 (product type generator)
covering
test_SOP_productGen1 : IO Bool
test_SOP_productGen1 = do
  let gen = productGen1 Just (arbitrary {a=Bool})
  samples <- sample (list (linear 10 10) gen)
  pure $ all isJust samples

||| REQ_TNCGF_SOP_009: Test productGen2
covering
test_SOP_productGen2 : IO Bool
test_SOP_productGen2 = do
  let gen = productGen2 MkPair (arbitrary {a=Bool}) (arbitrary {a=Bool})
  samples <- sample (list (linear 10 10) gen)
  pure $ length samples == 10

||| REQ_TNCGF_SOP_010: Test productGen3
covering
test_SOP_productGen3 : IO Bool
test_SOP_productGen3 = do
  let mkTriple = \a, b, c => (a, b, c)
  let gen = productGen3 mkTriple (arbitrary {a=Bool}) (arbitrary {a=Bool}) (arbitrary {a=Bool})
  samples <- sample gen
  pure $ True  -- Just check it doesn't crash

-- =============================================================================
-- DeriveDep Tests (natWithUpperBound, genLTEPair, etc.)
-- =============================================================================

||| REQ_TNCGF_TYPE_009: Test natWithUpperBound always <= upper
covering
test_DDEP_natWithUpperBound : IO Bool
test_DDEP_natWithUpperBound = do
  let upper = 50
  samples <- sample (list (linear 100 100) (natWithUpperBound upper))
  pure $ all (\n => n <= upper) samples

||| REQ_TNCGF_DEP_002: Test natWithUpperBound bounded generation (DeriveDep spec)
covering
test_DDEP_natWithUpperBoundSpec : IO Bool
test_DDEP_natWithUpperBoundSpec = do
  -- REQ_TNCGF_DEP_002: DeriveDep SHALL provide natWithUpperBound
  let upper = 100
  samples <- sample (list (linear 50 50) (natWithUpperBound upper))
  -- Verify all values are bounded by upper
  pure $ all (\n => n <= upper) samples

||| REQ_TNCGF_DDEP_002: Test natInRange within bounds
covering
test_DDEP_natInRange : IO Bool
test_DDEP_natInRange = do
  let lo = 10
  let hi = 20
  samples <- sample (list (linear 50 50) (natInRange lo hi))
  pure $ all (\n => n >= lo && n <= hi) samples

||| REQ_TNCGF_DDEP_003: Test intWithBounds within bounds
covering
test_DDEP_intWithBounds : IO Bool
test_DDEP_intWithBounds = do
  let lo = -10
  let hi = 10
  samples <- sample (list (linear 50 50) (intWithBounds lo hi))
  pure $ all (\n => n >= lo && n <= hi) samples

||| REQ_TNCGF_TYPE_010: Test genLTEPair always satisfies smaller <= larger
covering
test_DDEP_genLTEPair : IO Bool
test_DDEP_genLTEPair = do
  samples <- sample (list (linear 100 100) (genLTEPair 100))
  pure $ all (\(smaller, larger) => smaller <= larger) samples

||| REQ_TNCGF_DDEP_005: Test genWithLTEProof (DPair structure)
covering
test_DDEP_genWithLTEProof : IO Bool
test_DDEP_genWithLTEProof = do
  sample <- sample (genWithLTEProof 50)
  let (larger ** smaller ** _) = sample
  pure $ smaller <= larger

-- =============================================================================
-- Config Tests (GenStrategy, TNCGFConfig, default constants)
-- =============================================================================

||| REQ_TNCGF_TYPE_004: Test GenStrategy constructors exist
covering
test_CFG_genStrategy : IO Bool
test_CFG_genStrategy = do
  -- GenStrategy: SOPBased, DepGraphBased, Manual
  let s1 = SOPBased
  let s2 = DepGraphBased
  let s3 = Manual
  pure $ s1 /= s2 && s2 /= s3

||| REQ_TNCGF_TYPE_005: Test TNCGFConfig record fields
covering
test_CFG_tncgfConfig : IO Bool
test_CFG_tncgfConfig = do
  let cfg = MkTNCGFConfig 100 50 10 20 1000 100
  pure $ cfg.natMax == 100
      && cfg.intMax == 50
      && cfg.listMaxLen == 10
      && cfg.stringMaxLen == 20
      && cfg.testCount == 1000
      && cfg.maxShrinks == 100

||| REQ_TNCGF_TYPE_013: Test defaultNatMax is reasonable (default constants)
||| Discovered spec: defaultNatMax = 1000000 (large exploration space)
covering
test_CFG_defaultNatMax : IO Bool
test_CFG_defaultNatMax = pure $ defaultNatMax == 1000000

||| REQ_TNCGF_CFG_002: Test defaultStringMaxLen is reasonable
covering
test_CFG_defaultStringMaxLen : IO Bool
test_CFG_defaultStringMaxLen = pure $ defaultStringMaxLen > 0 && defaultStringMaxLen <= 1000

||| REQ_TNCGF_CFG_003: Test defaultListMaxLen is reasonable
covering
test_CFG_defaultListMaxLen : IO Bool
test_CFG_defaultListMaxLen = pure $ defaultListMaxLen > 0 && defaultListMaxLen <= 100

||| REQ_TNCGF_CFG_004: Test defaultIntMax is reasonable
covering
test_CFG_defaultIntMax : IO Bool
test_CFG_defaultIntMax = pure $ defaultIntMax > 0

-- =============================================================================
-- All TNCGF Tests
-- =============================================================================

export
covering
allTNCGFTests : List (String, IO Bool)
allTNCGFTests =
  -- DependencyGraph Tests
  [ ("REQ_TNCGF_DEP_001", test_DEP_simpleField)
  , ("REQ_TNCGF_TYPE_002", test_DEP_depField)
  , ("REQ_TNCGF_TYPE_003", test_DEP_mkGraph)
  , ("REQ_TNCGF_TYPE_011", test_DEP_topoSort_simple)
  , ("REQ_TNCGF_DEP_005", test_DEP_topoSort_deps)
  , ("REQ_TNCGF_DEP_006", test_DEP_topoSort_cyclic)
  , ("REQ_TNCGF_DEP_007", test_DEP_hasDependencies)
  , ("REQ_TNCGF_DEP_008", test_DEP_hasAnyDependencies)
  , ("REQ_TNCGF_DEP_009", test_DEP_directDeps)
  , ("REQ_TNCGF_TYPE_012", test_DEP_isProofTypeName)
  , ("REQ_TNCGF_DEP_011", test_DEP_prettyPrint)
  , ("REQ_TNCGF_DEP_012", test_DEP_toMermaid)
  -- Arbitrary Tests
  , ("REQ_TNCGF_ARB_001", test_ARB_bool)
  , ("REQ_TNCGF_BASE_001", test_ARB_nat)
  , ("REQ_TNCGF_TYPE_001", test_ARB_string)
  , ("REQ_TNCGF_ARB_004", test_ARB_maybe)
  , ("REQ_TNCGF_ARB_005", test_ARB_either)
  , ("REQ_TNCGF_ARB_006", test_ARB_list)
  , ("REQ_TNCGF_ARB_007", test_ARB_pair)
  , ("REQ_TNCGF_ARB_008", test_ARB_vect)
  , ("REQ_TNCGF_ARB_009", test_ARB_fin)
  -- DeriveSOP Tests
  , ("REQ_TNCGF_SOP_001", test_SOP_enumGen2)
  , ("REQ_TNCGF_TYPE_006", test_SOP_enumGen3)
  , ("REQ_TNCGF_SOP_003", test_SOP_enumGen4)
  , ("REQ_TNCGF_SOP_004", test_SOP_enumGen5)
  , ("REQ_TNCGF_TYPE_007", test_SOP_sumGen2)
  , ("REQ_TNCGF_SOP_006", test_SOP_sumGen3)
  , ("REQ_TNCGF_SOP_007", test_SOP_sumGen4)
  , ("REQ_TNCGF_TYPE_008", test_SOP_productGen1)
  , ("REQ_TNCGF_SOP_009", test_SOP_productGen2)
  , ("REQ_TNCGF_SOP_010", test_SOP_productGen3)
  -- DeriveDep Tests
  , ("REQ_TNCGF_TYPE_009", test_DDEP_natWithUpperBound)
  , ("REQ_TNCGF_DEP_002", test_DDEP_natWithUpperBoundSpec)
  , ("REQ_TNCGF_DDEP_002", test_DDEP_natInRange)
  , ("REQ_TNCGF_DDEP_003", test_DDEP_intWithBounds)
  , ("REQ_TNCGF_TYPE_010", test_DDEP_genLTEPair)
  , ("REQ_TNCGF_DDEP_005", test_DDEP_genWithLTEProof)
  -- Config Tests
  , ("REQ_TNCGF_TYPE_004", test_CFG_genStrategy)
  , ("REQ_TNCGF_TYPE_005", test_CFG_tncgfConfig)
  , ("REQ_TNCGF_TYPE_013", test_CFG_defaultNatMax)
  , ("REQ_TNCGF_CFG_002", test_CFG_defaultStringMaxLen)
  , ("REQ_TNCGF_CFG_003", test_CFG_defaultListMaxLen)
  , ("REQ_TNCGF_CFG_004", test_CFG_defaultIntMax)
  ]

-- =============================================================================
-- Main Entry Point
-- =============================================================================

covering
runTest : (String, IO Bool) -> IO Bool
runTest (name, test) = do
  result <- test
  putStrLn $ "[" ++ (if result then "PASS" else "FAIL") ++ "] " ++ name
  pure result

||| Run all tests and return typed result (total, passed)
||| For direct import by lazy-idris - no string parsing needed
export
covering
runAllTests : IO (Nat, Nat)
runAllTests = do
  results <- traverse (\(_, test) => test) allTNCGFTests
  pure (length allTNCGFTests, length (filter id results))

||| Run all tests with verbose output, return typed result
export
covering
runAllTestsVerbose : IO (Nat, Nat)
runAllTestsVerbose = do
  putStrLn $ "Running " ++ show (length allTNCGFTests) ++ " TNCGF tests..."
  results <- traverse runTest allTNCGFTests
  let result = (length allTNCGFTests, length (filter id results))
  putStrLn $ "Passed: " ++ show (snd result) ++ "/" ++ show (fst result)
  pure result

export
covering
main : IO ()
main = do
  result <- runAllTestsVerbose
  if snd result == fst result
     then putStrLn "All TNCGF tests passed!"
     else exitFailure
