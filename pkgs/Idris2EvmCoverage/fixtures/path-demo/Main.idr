module Main

partial
partialMaybe : Maybe Int -> Int
partialMaybe (Just x) = x

safeHead : List a -> Maybe a
safeHead [] = Nothing
safeHead (x :: xs) = Just x

useSafeHead : Maybe Int
useSafeHead = safeHead [1]

partial
usePartialMaybe : Int
usePartialMaybe = partialMaybe (Just 1)

public export
runCovered : IO ()
runCovered =
  if assert_total (partialMaybe (Just 1)) == 1 then
    case safeHead [1] of
      Just _ => pure ()
      Nothing => pure ()
  else pure ()

main : IO ()
main = runCovered
