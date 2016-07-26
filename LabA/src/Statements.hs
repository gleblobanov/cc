module Statements where

compileStms :: Env -> [Stm] -> (Code, Env)
compileStms env [] = ("\n", env)
compileStms env (stm:stms) = (code, env'')
    where (code', env') = compileStm env stm
          (code'', env'') = compileStms env' stms
          code = "\n" ++ code' ++ code''


compileStm :: Env -> Stm -> (Code, Env)
compileStm env stm =
    case stm of
      (SExp (EAss exp1 exp2)) -> compileStmAss env exp1 exp2
      (SExp exp)              -> compileExp env exp
      (SDecls t ids)          -> compileDecl env ids
      (SInit t id exp)        -> compileStm (extendVar env id) (SExp (EAss (EId id) exp))
      (SBlock stms)           -> compileBlock (newBlock env) stms
      (SWhile exp stm)        -> compileWhile env exp stm
      (SIfElse exp stm1 stm2) -> compileIf env exp stm1 stm2
      (SReturn exp)           -> compileReturn env exp


compileStmAss :: Env -> Exp -> Exp -> (Code, Env)
compileStmAss env (EId id) e2 =
    case lookupVar env id of
      Just addr -> (code, env2)
          where (code2, env2) = compileExp env e2
                codeStore = show $ Store addr
                code = code2 ++ codeStore
      Nothing   -> ("", env)


compileDecl :: Env -> [Id] -> (Code, Env)
compileDecl env [] = ("", env)
compileDecl env (id:ids) = ("", env'')
    where (code, env'') = compileDecl env' ids
          env' = extendVar env id


compileBlock :: Env -> [Stm] -> (Code, Env)
compileBlock  env [] = ("", exitBlock env)
compileBlock  env (stm:stms) = (code, env2)
    where (code1, env1) = compileStm env stm
          (code2, env2) = compileBlock env1 stms
          code = code1 ++ code2


compileWhile :: Env -> Exp -> Stm -> (Code, Env)
compileWhile env exp stm = (code, exitBlock env4)
    where (test, env1) = newLabel env
          testLabel = "l" ++ show test
          (end, env2) = newLabel env1
          endLabel = "l" ++ show end
          (code1, env3) = compileExp env2 exp
          (code2, env4) = compileStm (newBlock env3) stm
          code = show (Label testLabel) ++
                 code1 ++
                 show (IfEq endLabel) ++
                 code2 ++
                 show (GoTo testLabel) ++
                 show (Label endLabel)


compileIf :: Env -> Exp -> Stm -> Stm -> (Code, Env)
compileIf env exp stm1 stm2 = (code, env5)
    where (tr, env1) = newLabel env
          trLabel = "l" ++ show tr
          (fls, env2) = newLabel env1
          flsLabel = "l" ++ show fls
          (code1, env3) = compileExp env2 exp
          (code2, env4) = compileStm env3 stm1
          (code3, env5) = compileStm env4 stm2
          code = code1 ++
                 show (IfEq flsLabel) ++
                 code2 ++
                 show (GoTo trLabel) ++
                 show (Label flsLabel) ++
                 code3 ++
                 show (Label trLabel)


compileReturn :: Env -> Exp -> (Code, Env)
compileReturn env exp = (code, env')
    where (code', env') = compileExp env exp
          code = code' ++ (show IReturn)


