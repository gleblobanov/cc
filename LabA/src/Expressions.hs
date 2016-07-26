module Expressions where

import Environment


compileExp :: Env -> Exp -> (Code, Env)
compileExp env exp = case exp of
                       ETimes exp1 exp2 -> compileArithm env exp1 exp2 Mul
                       EPlus  exp1 exp2 -> compileArithm env exp1 exp2 Add
                       EMinus exp1 exp2 -> compileArithm env exp1 exp2 Sub
                       EDiv   exp1 exp2 -> compileArithm env exp1 exp2 Div

                       EInt i -> (show (PushInt i), enlargeStack env 1)

                       EId id -> compileExpId env id

                       EAss e1 e2 -> compileExpAss env e1 e2

                       ETrue  -> (show (PushInt 1), enlargeStack env 1)
                       EFalse -> (show (PushInt 0), enlargeStack env 1)

                       EApp id exps -> compileExpApp env id exps

                       EPostIncr exp -> compileExpPostIncr env exp
                       EPostDecr exp -> compileExpPostDecr env exp
                       EPreIncr  exp -> compileExpPreIncr  env exp
                       EPreDecr  exp -> compileExpPreDecr  env exp

                       ELt   exp1 exp2 -> compileExpLt   env exp1 exp2
                       EGt   exp1 exp2 -> compileExpGt   env exp1 exp2
                       ELtEq exp1 exp2 -> compileExpLtEq env exp1 exp2
                       EGtEq exp1 exp2 -> compileExpGtEq env exp1 exp2
                       EEq   exp1 exp2 -> compileExpEq   env exp1 exp2
                       ENEq  exp1 exp2 -> compileExpNEq  env exp1 exp2
                       EAnd  exp1 exp2 -> compileExpAnd  env exp1 exp2
                       EOr   exp1 exp2 -> compileExpOr   env exp1 exp2


compileArithm :: Env -> Exp -> Exp -> Instr -> (Code, Env)
compileArithm env exp1 exp2 instr = (code, env2)
    where (code1, env1) = compileExp env exp1
          (code2, env2) = compileExp env1 exp2
          code = code1 ++
                 code2 ++
                 show instr


compileExpId :: Env -> Id -> (Code, Env)
compileExpId env id =
    case (lookupVar env id) of
      Just addr -> (show (Load addr), (enlargeStack env 1))
      Nothing   -> ("", env)

compileExpAss :: Env -> Exp -> Exp -> (Code, Env)
compileExpAss env (EId id) exp2 =
    case (lookupVar env id) of
      Just addr -> (code, (enlargeStack env2 2))
          where (code2, env2) = compileExp env exp2
                codeStore = show $ Store addr
                codeLoad  = show $ Load addr
                code = code2 ++ codeStore ++ codeLoad
      Nothing   -> ("", env)



compileExpApp :: Env -> Id -> [Exp] -> (Code, Env)
compileExpApp env (Id "printInt") exps = (code, env')
    where (code', env') = compileExps env exps
          code'' = "   invokestatic runtime/printInt(I)V\n"
          code   = code' ++ code''
compileExpApp env (Id "readInt") _ =
    ("   invokestatic runtime/readInt()I\n", env)
compileExpApp env (Id id) exps = 
    case (lookupFun env (Id id)) of
      Just (t, _, args) -> (code, env')
          where (code', env') = compileExps env exps
                code'' = "   invokestatic "
                         ++ (getClassName env) ++ "/" ++ id
                         ++ "(" ++ targs ++ ")" ++ t' ++"\n"
                code   = code' ++ code''
                t'     = typeToLetter t
                targs  = argsToString args
      Nothing           -> ("", env)


compileExps :: Env -> [Exp] -> (Code, Env)
compileExps env [] = ("", env)
compileExps env (e:es) = (code, env'')
    where (code', env') = compileExp env e
          (code'', env'') = compileExps env' es
          code = code' ++ code''


compileExpPostIncr :: Env -> Exp -> (Code, Env)
compileExpPostIncr env (EId id) =
    case (lookupVar env id) of
      Just addr -> (code, (enlargeStack env 3))
          where code = show (Load addr)  ++
                       show (Load addr)  ++
                       show (PushInt 1)  ++
                       show Add          ++
                       show (Store addr)
      Nothing   -> ("", env)


compileExpPostDecr :: Env -> Exp -> (Code, Env)
compileExpPostDecr env (EId id) =
    case (lookupVar env id) of
      Just addr -> (code, (enlargeStack env 3))
          where code = show (Load addr)  ++
                       show (Load addr)  ++
                       show (PushInt 1)  ++
                       show Sub          ++
                       show (Store addr)
      Nothing   -> ("", env)


compileExpPreIncr :: Env -> Exp -> (Code, Env)
compileExpPreIncr env (EId id)=
    case (lookupVar env id) of
      Just addr -> (code, (enlargeStack env 3))
          where code = show (Load addr)  ++
                       show (PushInt 1)  ++
                       show Add          ++
                       show (Store addr) ++
                       show (Load addr)
      Nothing   -> ("", env) 


compileExpPreDecr :: Env -> Exp -> (Code, Env)
compileExpPreDecr env (EId id) =
    case (lookupVar env id) of
      Just addr -> (code, (enlargeStack env 3))
          where code = show (Load addr)  ++
                       show (PushInt 1)  ++
                       show Sub          ++
                       show (Store addr) ++
                       show (Load addr)
      Nothing   -> ("", env) 


compileExpLt :: Env -> Exp -> Exp -> (Code, Env)
compileExpLt env exp1 exp2 = (code, (enlargeStack env3 2))
    where (code1, env1) = compileExp env exp1
          (code2, env2) = compileExp env1 exp2
          (tr, env3) = newLabel env2
          trLbl = "l" ++ (show tr)
          code = show (PushInt 1)   ++
                 code1              ++
                 code2              ++
                 show (CmpLt trLbl) ++
                 show Pop           ++
                 show (PushInt 0)   ++
                 show (Label trLbl) 


compileExpGt :: Env -> Exp -> Exp -> (Code, Env)
compileExpGt   env exp1 exp2 = (code, (enlargeStack env3 2))
    where (code1, env1) = compileExp env exp1
          (code2, env2) = compileExp env1 exp2
          (tr, env3) = newLabel env2
          trLbl = "l" ++ (show tr)
          code = show (PushInt 1)   ++
                 code1              ++
                 code2              ++
                 show (CmpGt trLbl) ++
                 show Pop           ++
                 show (PushInt 0)   ++
                 show (Label trLbl) 


compileExpLtEq :: Env -> Exp -> Exp -> (Code, Env)
compileExpLtEq env exp1 exp2 = (code, (enlargeStack env3 2))
    where (code1, env1) = compileExp env exp1
          (code2, env2) = compileExp env1 exp2
          (tr, env3) = newLabel env2
          trLbl = "l" ++ (show tr)
          code = show (PushInt 1)   ++
                 code1              ++
                 code2              ++
                 show (CmpLe trLbl) ++
                 show Pop           ++
                 show (PushInt 0)   ++
                 show (Label trLbl) 


compileExpGtEq :: Env -> Exp -> Exp -> (Code, Env)
compileExpGtEq env exp1 exp2 = (code, (enlargeStack env3 2))
    where (code1, env1) = compileExp env exp1
          (code2, env2) = compileExp env1 exp2
          (tr, env3) = newLabel env2
          trLbl = "l" ++ (show tr)
          code = show (PushInt 1)   ++
                 code1              ++
                 code2              ++
                 show (CmpGe trLbl) ++
                 show Pop           ++
                 show (PushInt 0)   ++
                 show (Label trLbl) 


compileExpEq :: Env -> Exp -> Exp -> (Code, Env)
compileExpEq   env exp1 exp2 = (code, (enlargeStack env3 2))
    where (code1, env1) = compileExp env exp1
          (code2, env2) = compileExp env1 exp2
          (tr, env3) = newLabel env2
          trLbl = "l" ++ (show tr)
          code = show (PushInt 1)   ++
                 code1              ++
                 code2              ++
                 show (CmpEq trLbl) ++
                 show Pop           ++
                 show (PushInt 0)   ++
                 show (Label trLbl) 


compileExpNEq :: Env -> Exp -> Exp -> (Code, Env)
compileExpNEq  env exp1 exp2 = (code, (enlargeStack env3 2))
    where (code1, env1) = compileExp env exp1
          (code2, env2) = compileExp env1 exp2
          (tr, env3) = newLabel env2
          trLbl = "l" ++ (show tr)
          code = show (PushInt 1)   ++
                 code1              ++
                 code2              ++
                 show (CmpNe trLbl) ++
                 show Pop           ++
                 show (PushInt 0)   ++
                 show (Label trLbl) 


compileExpAnd :: Env -> Exp -> Exp -> (Code, Env)
compileExpAnd  env exp1 exp2 = (code, (enlargeStack env3 2))
    where (code1, env1) = compileExp env exp1
          (code2, env2) = compileExp env1 exp2
          (end, env3) = newLabel env2
          endLbl = "l" ++ (show end)
          code = show (PushInt 0)    ++
                 show (PushInt 0)    ++
                 code1               ++
                 show (CmpEq endLbl) ++
                 show (PushInt 0)    ++
                 code2               ++
                 show (CmpEq endLbl) ++
                 show (Pop)          ++
                 show (PushInt 1)    ++
                 show (Label endLbl)


compileExpOr :: Env -> Exp -> Exp -> (Code, Env)
compileExpOr   env exp1 exp2 = (code, (enlargeStack env3 2))
    where (code1, env1) = compileExp env exp1
          (code2, env2) = compileExp env1 exp2
          (end, env3) = newLabel env2
          endLbl = "l" ++ (show end)
          code = show (PushInt 1)    ++
                 show (PushInt 1)    ++
                 code1               ++
                 show (CmpEq endLbl) ++
                 show (PushInt 1)    ++
                 code2               ++
                 show (CmpEq endLbl) ++
                 show (Pop)          ++
                 show (PushInt 0)    ++
                 s
