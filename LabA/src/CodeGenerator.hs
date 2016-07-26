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
  where code = linearise llvmTree
        globalStrings = toGlobalVars $ getGlobalStrings env
        (llvmTree, env) = runEnvState p
        p = findStrings defs >>=
            collectFuns      >>=
            flip transform []
