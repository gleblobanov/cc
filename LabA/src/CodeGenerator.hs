module CodeGenerator (generateCode) where

import AbsJL
import Environment
import Functions
import LLVMTree
import Strings

generateCode :: Program -> String
generateCode (PDefs defs)
  = globalStrings ++
    declPrintRead ++
    code
  where p = findStrings defs >>=
            collectFuns      >>=
            transform []
        (llvmTree, env) = runEnvState p
        globalStrings = toGlobalVars $ getGlobalStrings env
        code =  linearise llvmTree
