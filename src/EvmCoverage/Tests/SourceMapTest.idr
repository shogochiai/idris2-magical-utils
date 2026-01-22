module EvmCoverage.Tests.SourceMapTest

import EvmCoverage.SourceMap

export
testParseComments : IO ()
testParseComments = do
  let yul = "/* Main:5:2--5:10 */ let x := 42 /* Main:6:2--6:15 */ let y := add(x, 1)"
  let comments = parseYulComments yul
  putStrLn $ "Found " ++ show (length comments) ++ " comments"
  for_ comments $ \c => 
    putStrLn $ "  Offset " ++ show c.yulOffset ++ ": " ++ show c.idrisLoc
