module PathDemo

safeHead : List a -> Maybe a
safeHead [] = Nothing
safeHead (x :: xs) = Just x

public export
useSafeHead : Maybe Int
useSafeHead = safeHead [42]

export
runTests : IO (Int, Int)
runTests =
  pure $
    if useSafeHead == Just 42
       then (1, 0)
       else (0, 1)

export
runMinimalTests : IO (Int, Int)
runMinimalTests = runTests

export
runTrivialTest : IO (Int, Int)
runTrivialTest = runTests

main : IO ()
main = pure ()
