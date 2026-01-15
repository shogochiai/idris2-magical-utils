module TempTestRunner_test_1_75000

import Coverage.Tests.AllTests


main : IO ()
main = do
  Coverage.Tests.AllTests.runAllTests

