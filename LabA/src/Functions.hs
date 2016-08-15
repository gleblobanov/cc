module Functions where

import qualified AbsJL as JL
import LLVMSyntax
import Environment
import LLVMStms
import Control.Monad
import LLVMTypes

putStmsToFun :: LLVMFunction -> [LLVMStm] -> LLVMFunction
putStmsToFun (LLVMFunction t fid args _) = LLVMFunction t fid args


putFunToTree :: LLVMTree -> LLVMFunction -> EnvState Env LLVMTree
putFunToTree tree fun =
  EnvState (\env -> (tree ++ [fun], env))


transFun :: LLVMTree -> JL.Def -> EnvState Env LLVMTree
transFun tree (JL.DFun t (JL.Id fid) args stms) =
  do cnt <- getCounter
     fns <- getFuns
     let t' = transType t
     putType t'
     case lookup (JL.Id fid) fns of
       Just (_, _, LLVMArgs args') ->
        do allocStms <- mapM allocateArg $ zip args args'
           stms' <- transStms stms
           let fun = LLVMFunction t' (Global fid) (LLVMArgs args') []
               entryStm = LLVMStmLabel $ LLVMLabel "entry"
               retStm   = LLVMStmInstr $ case t' of
                 TypeVoid -> ReturnVoid
                 TypeInteger -> Return t' (OC $ ConstInteger 0)
                 TypeDouble   -> Return t' (OC $ ConstDouble 0.0)
               stmsToAdd = concat allocStms ++ stms'
                 ++ if null stms'
                    then [retStm]
                    else case last stms' of
                 LLVMStmInstr (Return _ _) -> []
                 LLVMStmInstr ReturnVoid -> []
                 _ -> [retStm]
               fun' = putStmsToFun fun $ entryStm : stmsToAdd
           putFunToTree tree fun'

       Nothing -> fail $ "Function isn't found " ++ show fid


-- extendArgs :: [(Arg, LLVMArg)] -> EnvState Env [LLVMStm]
-- extendArgs = do
--   cn
--   in foldr ((>>). (\(ADecl _ aid, LLVMArg t val) ->
--                             extendVar aid val t)) (return ())

allocateArg :: (JL.Arg, LLVMArg) -> EnvState Env [LLVMStm]
allocateArg (JL.ADecl typ aid, LLVMArg typ' op) =
  do ptr <- genLocal
     -- let stm1 = LLVMStmAssgn ptr (Allocate typ')
     --     stm2 = LLVMStmInstr $ Store typ' op typ' ptr
     -- extendVar aid (OI ptr) typ'
     extendVar aid op typ'
     return []
     -- return [stm1, stm2]

declPrintRead :: String
declPrintRead = "declare void @printInt(i32)\n\
                \declare void @printDouble(double)\n\
                \declare void @printString(i8*)\n\
                \declare i32 @readInt()\n\
                \declare double @readDouble()\n\
                \declare i32* @calloc(i32, i32)\n"

