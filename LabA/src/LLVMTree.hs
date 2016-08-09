module LLVMTree where

import AbsJL
import LLVMSyntax
import Environment
import LLVMStms
import Control.Monad

type LLVMTree = [LLVMFunction]

linearise :: LLVMTree -> String
linearise = concatMap show

-- | Transform JL abstract syntax tree to LLVM abstract syntax tree.
transform ::  LLVMTree -> [Def] -> EnvState Env LLVMTree
--transform tree [] = return tree
-- transform tree (d:ds) = do tree' <- transFun tree d
--                            transform tree' ds
transform = foldM transFun

putStmsToFun :: LLVMFunction -> [LLVMStm] -> LLVMFunction
putStmsToFun (LLVMFunction t fid args _) = LLVMFunction t fid args


putFunToTree :: LLVMTree -> LLVMFunction -> EnvState Env LLVMTree
putFunToTree tree fun =
  EnvState (\env -> (tree ++ [fun], env))


transFun :: LLVMTree -> Def -> EnvState Env LLVMTree
transFun tree (DFun t (Id fid) args stms) =
     let t' = transType t
         args' = transArgs args
         fun = LLVMFunction t' (Global fid) args' []
     in do putType t'
           stms' <- transStms stms
           let fun' = putStmsToFun fun stms'
           putFunToTree tree fun'
