module MinimalPkg.Tests.AllTests

import MinimalPkg

export
runAllTests : IO ()
runAllTests = do
  let r1 = add 1 2 == 3
  let r2 = isPositive 5
  let r3 = not (isPositive (-1))
  putStrLn $ if r1 && r2 && r3 then "ALL PASS" else "FAIL"
