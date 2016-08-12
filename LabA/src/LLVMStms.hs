module LLVMStms where

import AbsJL
import LLVMSyntax
import LLVMTypes
import LLVMExps
import Environment

transStms :: [Stm] -> EnvState Env [LLVMStm]
transStms []     = return []
transStms (s:ss) =
  case s of
    SExp e         -> transSExp e ss
    SDecls t ids   -> transSDecls t ids ss
    SInit t ids e  -> transSInit t ids e ss
    SReturn rst    -> transSReturn rst
    SWhile e stm   -> transSWhile e stm ss
    SBlock stms    -> transSBlock stms ss
    SIf e rst      -> transSIf e rst ss



transSExp :: Exp -> [Stm] -> EnvState Env [LLVMStm]
transSExp e ss = do (_, expStms) <- transExp e
                    restStms <- transStms ss
                    return $ expStms ++ restStms



transSDecls :: Type -> [Id] -> [Stm] -> EnvState Env [LLVMStm]
transSDecls t ids ss = do declStms <- transDecl t ids
                          restStms <- transStms ss
                          return $ declStms ++ restStms

transDecl :: Type -> [Id] -> EnvState Env [LLVMStm]
transDecl _ []  = return []
transDecl t (vid:vids) = do stms  <- mkDeclStm t vid
                            stms' <- transDecl t vids
                            return $ stms ++ stms'

mkDeclStm :: Type -> Id -> EnvState Env [LLVMStm]
mkDeclStm t vid = do (OI ptr, t') <- extendVarDecl vid t
                     let allInstr = Allocate t'
                         allstm   = LLVMStmAssgn ptr allInstr
                         val = OC $ case t' of
                           TypeInteger -> ConstInteger 0
                           TypeDouble  -> ConstDouble 0.0
                           TypeBoolean -> ConstFalse
                         sStore   = LLVMStmInstr (Store t' val t' ptr)
                     return [allstm, sStore]



transSInit :: Type -> [Id] -> Exp -> [Stm] -> EnvState Env [LLVMStm]
transSInit t ids e ss = do ((val, _), expStms) <- transExp e
                           cnt                 <- getCounter
                           initStms            <- mkInitStms t ids val
                           restStms            <- transStms ss
                           return $ expStms ++ initStms ++ restStms

mkInitStms :: Type -> [Id] -> Operand -> EnvState Env [LLVMStm]
mkInitStms _ [] _ = return []
mkInitStms t (vid:vids) tmp = do stms     <- mkInitStm t vid tmp
                                 stmsRest <- mkInitStms t vids tmp
                                 return (stms ++ stmsRest)

mkInitStm :: Type -> Id -> Operand -> EnvState Env [LLVMStm]
mkInitStm t vid val = do (OI ptr, t') <- extendVarDecl vid t
                         let allInstr = Allocate t'
                             sAlloc   = LLVMStmAssgn ptr allInstr
                             tPtr'    = t'
                             sStore   = LLVMStmInstr (Store t' val tPtr' ptr)
                         return [sAlloc, sStore]




transSReturn :: ReturnRest ->  EnvState Env [LLVMStm]
transSReturn rest = case rest of
  ReturnRest e -> do ((val, _), expStms) <- transExp e
                     t <- getType
                     let retStm = LLVMStmInstr (Return t val)
                     return $ expStms ++ [retStm]
  ReturnRestEmpt -> return [LLVMStmInstr ReturnVoid]





transSWhile :: Exp -> Stm -> [Stm] -> EnvState Env [LLVMStm]
transSWhile e stm ss = do whileStms <- transWhile e stm
                          restStms <- transStms ss
                          return $ whileStms ++ restStms

transWhile :: Exp -> Stm -> EnvState Env [LLVMStm]
transWhile e stm = do ((val, t), expStms) <- transExp e
                      stms <- transStms [stm]
                      c1 <- getCounter
                      c2 <- getCounter
                      c3 <- getCounter
                      ct <- getCounter
                      let l1  = LLVMLabel $ "lab" ++ show c1
                          sl1 = LLVMStmLabel l1
                          l2  = LLVMLabel $ "lab" ++ show c2
                          sl2 = LLVMStmLabel l2
                          l3  = LLVMLabel $ "lab" ++ show c3
                          sl3 = LLVMStmLabel l3
                          condStm   = LLVMStmInstr (CondBranch val (show l2) (show l3))
                          uncondStm  = LLVMStmInstr (UncondBranch (show l1))
                          res =
                                [uncondStm] ++ -- TODO Remove it and look what happens
                                [sl1]       ++
                                 expStms    ++
                                [condStm]   ++
                                [sl2]       ++
                                 stms       ++
                                [uncondStm] ++
                                [sl3]
                      return res




transSBlock :: [Stm] -> [Stm] -> EnvState Env [LLVMStm]
transSBlock stms ss = do blockStms <- transBlock stms
                         restStms  <- transStms ss
                         return $ blockStms ++ restStms

transBlock :: [Stm] -> EnvState Env [LLVMStm]
transBlock stms = do newBlock
                     stms' <- transStms stms
                     exitBlock
                     return stms'







transSIf :: Exp -> IfRest -> [Stm] -> EnvState Env [LLVMStm]
transSIf e r ss = do ifStms <- transIf e r
                     restStms  <- transStms ss
                     return $ ifStms ++ restStms

-- TODO Merge two branches effectively
transIf :: Exp -> IfRest -> EnvState Env [LLVMStm]
transIf e r = case r of
  IfR stm rr -> case rr of
    IfRREl stmEl -> do ((val, t), expStms) <- transExp e
                       c1 <- getCounter
                       c2 <- getCounter
                       c3 <- getCounter
                       ct <- getCounter
                       stms   <- transStms [stm]
                       stmsEl <- transStms [stmEl]
                       let l1  = LLVMLabel $  "lab" ++ show c1
                           sl1 = LLVMStmLabel l1
                           l2  = LLVMLabel $  "lab" ++ show c2
                           sl2 = LLVMStmLabel l2
                           l3  = LLVMLabel $  "lab" ++ show c3
                           sl3 = LLVMStmLabel l3
                           condStm   = LLVMStmInstr (CondBranch val (show l1) (show l2))
                           uncondStm = LLVMStmInstr (UncondBranch (show l3))
                           res = expStms    ++
                                [condStm]   ++
                                [sl1]       ++
                                 stms       ++
                                [uncondStm] ++
                                [sl2]       ++
                                 stmsEl     ++
                                [uncondStm] ++
                                [sl3]
                       return res

    IfRRE        -> do ((val, t), expStms) <- transExp e
                       c1 <- getCounter
                       c2 <- getCounter
                       ct <- getCounter
                       stms   <- transStms [stm]
                       let l1  = LLVMLabel $ "lab" ++  show c1
                           sl1 = LLVMStmLabel l1
                           l2  = LLVMLabel $ "lab" ++  show c2
                           sl2 = LLVMStmLabel l2
                           condStm   = LLVMStmInstr (CondBranch val (show l1) (show l2))
                           uncondStm = LLVMStmInstr (UncondBranch (show l2))
                           res = expStms    ++
                                [condStm]   ++
                                [sl1]       ++
                                 stms       ++
                                [uncondStm]  ++
                                [sl2]
                       return res

  IfRE -> do ((vid, t), expStms) <- transExp e
             return expStms
