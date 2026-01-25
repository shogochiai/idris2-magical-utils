||| Simple test for coverage with case expressions
module Main

import EVM.Primitives

-- Simple function with case
processValue : Integer -> Integer
processValue x =
  case x of
    0 => 100
    1 => 200
    _ => 300

-- Entry point
main : IO ()
main = do
  sel <- getSelector
  let result = processValue sel
  returnUint result
