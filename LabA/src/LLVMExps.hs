module LLVMExps where

import AbsJL
import LLVMSyntax
import LLVMTypes
import Environment

transExp :: Exp -> EnvState Env (Instruction, [LLVMStm])
transExp exp =   case exp of
 ETrue        -> return (ConstInstr ConstTrue, [])
 EFalse       -> return (ConstInstr ConstFalse, [])
 EString s    -> transString s
 EInt i       -> return (ConstInstr (ConstInteger i), [])
 EDouble d    -> return (ConstInstr (ConstDouble d), [])
 EId id       -> transId id
 EApp fid es  -> transApp fid es
 ENeg e       -> transNeg e
 ENot e       -> transNot e
 EPostIncr e  -> transPostIncr e
 EPostDecr e  -> transPostDecr e
 EPreIncr e   -> transPreIncr e
 EPreDecr e   -> transPreDecr e
 ETimes e1 e2  -> transTimes e1 e2
 EDiv e1 e2    -> transDiv e1 e2
 EMod e1 e2    -> transMod e1 e2
 EPlus e1 e2   -> transPlus e1 e2
 EMinus e1 e2  -> transMinus e1 e2
 ELt e1 e2     -> transLt e1 e2
 EGt e1 e2     -> transGt e1 e2
 ELtEq e1 e2   -> transLtEq e1 e2
 EGtEq e1 e2   -> transGtEq e1 e2
 EEq e1 e2     -> transEq e1 e2
 ENEq e1 e2    -> transNEq e1 e2
 EAnd e1 e2    -> transAnd e1 e2
 EOr e1 e2     -> transOr e1 e2
 EAss (EId vid) e -> transAss vid e
 _                -> return (EmptyInstr, [])



transString :: String -> EnvState Env (Instruction, [LLVMStm])
transString s = do r <- lookupGS s
                   case r of
                     Nothing -> return (EmptyInstr, [])
                     Just (sid, len) -> 

transId :: Id -> EnvState Env (Instruction, [LLVMStm])
transId s = undefined

transApp :: Id -> [Exp] -> EnvState Env (Instruction, [LLVMStm])
transApp = undefined

transNeg :: Exp -> EnvState Env (Instruction, [LLVMStm])
transNeg e = undefined

transNot :: Exp -> EnvState Env (Instruction, [LLVMStm])
transNot e = undefined

transPostIncr :: Exp -> EnvState Env (Instruction, [LLVMStm])
transPostIncr = undefined

transPostDecr :: Exp -> EnvState Env (Instruction, [LLVMStm])
transPostDecr = undefined

transPreIncr  :: Exp -> EnvState Env (Instruction, [LLVMStm])
transPreIncr = undefined

transPreDecr  :: Exp -> EnvState Env (Instruction, [LLVMStm])
transPreDecr = undefined

transTimes :: Exp -> Exp -> EnvState Env (Instruction, [LLVMStm])
transTimes e1 e2 = undefined

transDiv :: Exp -> Exp -> EnvState Env (Instruction, [LLVMStm])
transDiv e1 e2 = undefined

transMod :: Exp -> Exp -> EnvState Env (Instruction, [LLVMStm])
transMod e1 e2 = undefined

transPlus :: Exp -> Exp -> EnvState Env (Instruction, [LLVMStm])
transPlus e1 e2 = undefined

transMinus :: Exp -> Exp -> EnvState Env (Instruction, [LLVMStm])
transMinus e1 e2 = undefined

transLt :: Exp -> Exp -> EnvState Env (Instruction, [LLVMStm])
transLt e1 e2 = undefined

transGt :: Exp -> Exp -> EnvState Env (Instruction, [LLVMStm])
transGt e1 e2 = undefined

transLtEq :: Exp -> Exp -> EnvState Env (Instruction, [LLVMStm])
transLtEq e1 e2 = undefined

transGtEq :: Exp -> Exp -> EnvState Env (Instruction, [LLVMStm])
transGtEq e1 e2 = undefined

transEq :: Exp -> Exp -> EnvState Env (Instruction, [LLVMStm])
transEq e1 e2 = undefined

transNEq :: Exp -> Exp -> EnvState Env (Instruction, [LLVMStm])
transNEq e1 e2 = undefined

transAnd :: Exp -> Exp -> EnvState Env (Instruction, [LLVMStm])
transAnd e1 e2 = undefined

transOr :: Exp -> Exp -> EnvState Env (Instruction, [LLVMStm])
transOr e1 e2 = undefined

transAss :: Id -> Exp -> EnvState Env (Instruction, [LLVMStm])
transAss vid e = undefined
  -- do
  -- res <- lookupVar vid
  -- case res of
  --   Nothing     -> fail $ "Variable is not defined: " ++ show vid
  --   Just (n, _) -> do (instr, ss) <- transExp e'
  --                     let s = LLVMStmAssgn (LocalId (getIdentStr n)) instr
  --                     return (instr, [s])



