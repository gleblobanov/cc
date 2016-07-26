module LLVMTree where

import AbsJL
import LLVMSyntax
import LLVMTypes
import Environment
import LLVMStms
import LLVMExps

type LLVMTree = [LLVMFunction]

linearise :: LLVMTree -> String
linearise = concatMap show

-- | Transforms JL abstract syntax tree to LLVM abstract syntax tree.
transform :: [Def] -> LLVMTree -> EnvState Env LLVMTree
transform []     tree = return tree
transform (d:ds) tree = transFun d tree >>= transform ds


putStmsToFun :: LLVMFunction -> [LLVMStm] -> EnvState Env LLVMFunction
putStmsToFun (LLVMFunction t fid args _) stms =
  EnvState (\env -> (LLVMFunction t fid args stms,env))


putFunToTree :: LLVMTree -> LLVMFunction -> EnvState Env LLVMTree
putFunToTree tree fun =
  EnvState (\env -> (tree ++ [fun],env))


transFun :: Def -> LLVMTree -> EnvState Env LLVMTree
transFun (DFun t (Id fid) args stms) tree =
     let t' = transType t
         args' = transArgs args
         fun = LLVMFunction t' (GlobalId fid) args' []
     in do putType t'
           stms' <- transStms stms
           fun'  <- putStmsToFun fun stms'
           putFunToTree tree fun'






