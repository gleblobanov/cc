module Strings where

import AbsJL
import qualified Data.Map as Map
import Data.Map (Map)
import Environment
import LLVMSyntax

findStrings :: [Def] -> EnvState Env [Def]
findStrings defs = EnvState (\(ls, sc, fs, _, t) ->
  let gs = foldl findStringsFun [] defs
  in (defs, (ls, sc, fs, gs, t)))

findStringsFun :: GlobalStrings -> Def -> GlobalStrings
findStringsFun gs (DFun _ _ _ stms) = foldl findStringsStm gs stms

findStringsStm :: GlobalStrings -> Stm -> GlobalStrings
findStringsStm gs stm =
  case stm of
    SExp e        -> findStringsExp gs e
    SInit _ _ e   -> findStringsExp gs e
    SWhile _ stm' -> findStringsStm gs stm'
    SBlock stms   -> foldl findStringsStm gs stms
    SIf _ ifRest  ->
      case ifRest of
        IfR stm' ifRestRest ->
          let gs' = findStringsStm gs stm'
          in case ifRestRest of
            IfRREl stm'' -> findStringsStm gs' stm''
            IfRRE       -> gs'
        IfRE               -> gs
    SDecls  _ _ -> gs
    SReturn r -> case r of
      ReturnRest e -> findStringsExp gs e
      ReturnRestEmpt -> gs

findStringsExp :: GlobalStrings -> Exp -> GlobalStrings
findStringsExp gs (EString str) =  (str, (strId, length str + 1)) : gs
  where strId = "str" ++ show (length gs)
findStringsExp gs _             = gs


toGlobalVars :: GlobalStrings -> String
toGlobalVars = concatMap toVar

toVar :: (String, (GlobalId, Int)) -> String
toVar (str,(GlobalId strId, strLen)) =
  "@" ++ strId ++ " = internal constant [" ++ show strLen ++
  " x i8] c\"" ++ str ++ "\00\"\n"
