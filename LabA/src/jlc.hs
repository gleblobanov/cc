import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import System.Process
import System.FilePath

import AbsJL
import LexJL
import ErrM
import ParJL

import TypeChecker
import CodeGenerator
import Build


typeCheck :: String -> IO Program
typeCheck s = case pProgram (myLexer s) of
  Bad err  -> do
     print err
     hPutStrLn stderr "ERROR"
     return $ PDefs []
  Ok tree -> do
    hPutStrLn stderr "OK"
    return $ PDefs []
    {- case typecheck tree of
    Bad err -> do
      print err
      hPutStrLn stderr "ERROR"
      return $ PDefs []
    Ok _ -> do
      hPutStrLn stderr "OK"
      return tree -}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do content <- readFile file
                 program <- typeCheck content
                 let code = generateCode program
                     bn   = dropExtension file
                 writeFile (bn ++ ".ll") code
                 link bn
    ["-b", file] -> do content <- readFile file
                       program <- typeCheck content
                       let code = generateCode program
                           bn   = dropExtension file
                       writeFile (bn ++ ".ll") code
                       build bn

    _      -> do
      putStrLn "Usage: lab2 <SourceFile>"
      exitFailure
