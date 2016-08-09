module Functions where

import AbsJL
import LLVMSyntax
import Environment
import LLVMStms
import Control.Monad


declPrintRead :: String
declPrintRead = "declare void @printInt(i32)\n\
                \declare void @printDouble(double)\n\
                \declare void @printString(i8*)\n\
                \declare i32 @readInt()\n\
                \declare double @readDouble()\n"

putStmsToFun :: LLVMFunction -> [LLVMStm] -> LLVMFunction
putStmsToFun (LLVMFunction t fid args _) = LLVMFunction t fid args


putFunToTree :: LLVMTree -> LLVMFunction -> EnvState Env LLVMTree
putFunToTree tree fun =
  EnvState (\env -> (tree ++ [fun], env))


transFun :: LLVMTree -> Def -> EnvState Env LLVMTree
transFun tree (DFun t (Id fid) args stms) =
  do cnt <- getCounter
     fns <- getFuns
     let t' = transType t
     putType t'
     case lookup (Id fid) fns of
       Just (_, _, LLVMArgs args') ->
        do extendArgs $ zip args args'
           stms' <- transStms stms
           let fun = LLVMFunction t' (Global fid) (LLVMArgs args') []
               fun' = putStmsToFun fun stms'
           putFunToTree tree fun'

       Nothing -> fail $ "Function isn't found " ++ show fid
