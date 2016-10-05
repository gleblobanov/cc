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


produceAssembly :: String -> String
produceAssembly fn =
  "llvm-as" ++ " "   ++
  fn ++ ".ll"    ++ " ; " ++
  "llvm-as" ++ " "   ++
  runtimeLL


produceLink :: String -> String
produceLink fn =
  "llvm-link" ++ " " ++
  fn ++  ".bc"    ++ " " ++
  runtimeBC ++ " "
  ++ "-o " ++  "a.out"


produceObject :: String -> String
produceObject fn =
  "llc -filetype=obj" ++ " " ++
  "a.out"

produceExec :: String -> String
produceExec fn =
  "gcc" ++ " " ++
  "-w"  ++ " " ++
  "a.out.o"



linkCommands :: String -> [String]
linkCommands fn = [produceAssembly fn,
                   produceLink fn]



buildCommands :: String -> [String]
buildCommands fn = [produceAssembly fn,
                 produceLink fn,
                 produceObject fn, produceExec fn]


runCmd :: String -> IO ExitCode
runCmd cmd = do h <- runCommand cmd
                waitForProcess h


build :: String -> IO ()
build fn = mapM_ runCmd (buildCommands fn)


link :: String -> IO ()
link fn = mapM_ runCmd (linkCommands fn)

