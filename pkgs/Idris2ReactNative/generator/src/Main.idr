module Main

import RN.Scaffold
import System
import System.Directory

main : IO ()
main = do
  args <- getArgs
  let dir = case args of
              (_ :: d :: _) => d
              _             => "poc-app"
  ignore $ createDir dir
  Right () <- writeScaffold dir appNameDefault
    | Left e => do putStrLn ("scaffold error: " ++ show e); exitFailure
  putStrLn ("scaffold written to " ++ dir)
