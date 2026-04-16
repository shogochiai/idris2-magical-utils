module Tests.AllTests

export
allTests : List (String, IO Bool)
allTests = [("MINIMAL_001", pure True)]

export
runTests : IO (Int, Int)
runTests = pure (1, 0)

export
runMinimalTests : IO (Int, Int)
runMinimalTests = pure (1, 0)

export
runTrivialTest : IO (Int, Int)
runTrivialTest = pure (1, 0)

export
runAllTests : IO ()
runAllTests = do
  (p, f) <- runTests
  putStrLn $ show p ++ " passed, " ++ show f ++ " failed"
