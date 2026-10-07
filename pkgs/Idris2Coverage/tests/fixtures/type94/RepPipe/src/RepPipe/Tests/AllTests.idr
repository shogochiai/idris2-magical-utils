module RepPipe.Tests.AllTests

import RepPipe.Pipe

export
runAllTests : IO ()
runAllTests = putStrLn (if pipe 1 == 1 then "ALL PASS" else "FAIL")

main : IO ()
main = runAllTests
