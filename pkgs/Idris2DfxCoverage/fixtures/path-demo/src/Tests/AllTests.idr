module Tests.AllTests

import PathDemo

public export
coveredSafeHead : IO Bool
coveredSafeHead = pure (useSafeHead == Just 42)

public export
allTests : List (String, IO Bool)
allTests = [("covers cons branch", coveredSafeHead)]

main : IO ()
main = pure ()
