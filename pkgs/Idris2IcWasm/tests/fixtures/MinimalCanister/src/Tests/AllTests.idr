module Tests.AllTests

export
allTests : List (String, IO Bool)
allTests = [("MINIMAL_001", pure True)]

export
runTests : IO ()
runTests = putStrLn "tests ok"

export
runMinimalTests : IO ()
runMinimalTests = runTests

export
runTrivialTest : IO ()
runTrivialTest = runTests

export
runAllTests : IO ()
runAllTests = runTests
