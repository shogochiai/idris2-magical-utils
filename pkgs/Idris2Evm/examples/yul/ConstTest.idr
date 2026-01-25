module Main

import EVM.Primitives

-- Constants
MAGIC : Integer
MAGIC = 42

THRESHOLD : Integer
THRESHOLD = 100

-- Use constants in computation
check : Integer -> Integer
check x = if x > THRESHOLD then MAGIC else x

main : IO ()
main = do
  let result = check 50
  sstore 0 result
