module LLVMTree where

import AbsJL
import Environment
import LLVMStms
import LLVMSyntax
import Control.Monad
import Functions


linearise :: LLVMTree -> String
linearise = concatMap show

-- | Transform JL abstract syntax tree to LLVM abstract syntax tree.
transform ::  LLVMTree -> [Def] -> EnvState Env LLVMTree
transform = foldM transFun


