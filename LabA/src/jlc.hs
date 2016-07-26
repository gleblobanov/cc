import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import System.Process

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
    hPutStrLn stderr "ERROR"
    -- putStrLn err
    fail "Typecheck Error"
  Ok tree -> case typecheck tree of
    Bad err -> do
      hPutStrLn stderr "ERROR"
      fail "Typecheck Error"
    Ok _ -> return tree


main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do content <- readFile file
                 program <- typeCheck content
                 let code = generateCode program
                 writeFile progLL code
                 link
    _      -> do
      putStrLn "Usage: lab2 <SourceFile>"
      exitFailure
