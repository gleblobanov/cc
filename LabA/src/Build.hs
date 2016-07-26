module Build where

import Prelude
import System.Exit (ExitCode)
import System.Process (runCommand,
                       waitForProcess)

progLL    :: FilePath
progLL    = "myFile.ll"
progBC    :: FilePath
progBC    = "myFile.bc"

runtimeLL :: FilePath
runtimeLL = "lib/runtime.ll"
runtimeBC :: FilePath
runtimeBC = "lib/runtime.bc"

mainBC    :: FilePath
mainBC    = "main.bc"

mainO     :: FilePath
mainO     = "main.o"


produceAssembly :: String
produceAssembly =
  "llvm-as" ++ " "   ++
  progLL    ++ " ; " ++
  "llvm-as" ++ " "   ++
  runtimeLL


produceLink :: String
produceLink =
  "llvm-link" ++ " " ++
  progBC      ++ " " ++
  runtimeBC   ++ " " ++
  "-o "       ++ " " ++
  mainBC


produceObject :: String
produceObject =
  "llc -filetype=obj" ++ " " ++
  mainBC

produceExec :: String
produceExec =
  "gcc" ++ " " ++
  mainO


linkCommands :: [String]
linkCommands = [produceAssembly,
                produceLink]



buildCommands :: [String]
buildCommands = [produceAssembly,
                 produceLink,
                 produceObject,
                 produceExec]


runCmd :: String -> IO ExitCode
runCmd cmd = do h <- runCommand cmd
                waitForProcess h


build :: IO ()
build = mapM_ runCmd buildCommands


link :: IO ()
link = mapM_ runCmd linkCommands

