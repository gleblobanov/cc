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
transDecl t (vid:vids) = do stm  <- mkDeclStm t vid
                            stms <- transDecl t vids
                            return (stm:stms)

mkDeclStm :: Type -> Id -> EnvState Env LLVMStm
mkDeclStm t vid = do (ident, t') <- extendVar vid t
                     let allInstr = Allocate t'
                         stm      = LLVMStmAssgn ident allInstr
                     return stm



transSInit :: Type -> [Id] -> Exp -> [Stm] -> EnvState Env [LLVMStm]
transSInit t ids e ss = do ((vid, _), expStms) <- transExp e
                           cnt                 <- getCounter
                           initStms            <- mkInitStms t ids vid
                           restStms            <- transStms ss
                           return $ expStms ++ initStms ++ restStms

mkInitStms :: Type -> [Id] -> Identifier -> EnvState Env [LLVMStm]
mkInitStms _ [] _ = return []
mkInitStms t (vid:vids) tmp = do stms     <- mkInitStm t vid tmp
                                 stmsRest <- mkInitStms t vids tmp
                                 return (stms ++ stmsRest)

mkInitStm :: Type -> Id -> Identifier -> EnvState Env [LLVMStm]
mkInitStm t vid tmp = do (identPtr, t') <- extendVar vid t
                         let allInstr = Allocate t'
                             sAlloc   = LLVMStmAssgn identPtr allInstr
                             tPtr'    = typeToPtr t'
                             sStore   = LLVMStmInstr (Store t' (OI tmp) tPtr' identPtr)
                         return [sAlloc, sStore]




transSReturn :: ReturnRest ->  EnvState Env [LLVMStm]
transSReturn rest = case rest of
  ReturnRest e -> do ((vid, _), expStms) <- transExp e
                     t <- getType
                     cnt <- getCounter
                     let retStm = LLVMStmInstr (Return t (OI vid))
                     return $ expStms ++ [retStm]
  ReturnRestEmpt -> return [LLVMStmInstr ReturnVoid]





transSWhile :: Exp -> Stm -> [Stm] -> EnvState Env [LLVMStm]
transSWhile e stm ss = do whileStms <- transWhile e stm
                          restStms <- transStms ss
                          return $ whileStms ++ restStms

transWhile :: Exp -> Stm -> EnvState Env [LLVMStm]
transWhile e stm = do ((vid, t), expStms) <- transExp e
                      stms <- transStms [stm]
                      c1 <- getCounter
                      c2 <- getCounter
                      c3 <- getCounter
                      ct <- getCounter
                      let l1  = LLVMLabel $ show c1
                          sl1 = LLVMStmLabel l1
                          l2  = LLVMLabel $ show c2
                          sl2 = LLVMStmLabel l2
                          l3  = LLVMLabel $ show c3
                          sl3 = LLVMStmLabel l3
                          condStm   = LLVMStmInstr (CondBranch (OI vid) (show l2) (show l3))
                          uncondStm = LLVMStmInstr (UncondBranch (show l1))
                          res = [sl1]       ++
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

transIf :: Exp -> IfRest -> EnvState Env [LLVMStm]
transIf e r = case r of
  IfR stm rr -> case rr of
    IfRREl stmEl -> do ((vid, t), expStms) <- transExp e
                       c1 <- getCounter
                       c2 <- getCounter
                       c3 <- getCounter
                       ct <- getCounter
                       stms   <- transStms [stm]
                       stmsEl <- transStms [stmEl]
                       let l1  = LLVMLabel $ show c1
                           sl1 = LLVMStmLabel l1
                           l2  = LLVMLabel $ show c2
                           sl2 = LLVMStmLabel l2
                           l3  = LLVMLabel $ show c3
                           sl3 = LLVMStmLabel l3
                           condStm   = LLVMStmInstr (CondBranch (OI vid) (show l1) (show l2))
                           uncondStm = LLVMStmInstr (UncondBranch (show l3))
                           res = expStms    ++
                                [condStm]   ++
                                [sl1]       ++
                                 stms       ++
                                [uncondStm] ++
                                [sl2]       ++
                                 stmsEl     ++
                                [sl3]
                       return res

    IfRRE        -> do ((vid, t), expStms) <- transExp e
                       c1 <- getCounter
                       c2 <- getCounter
                       ct <- getCounter
                       stms   <- transStms [stm]
                       let l1  = LLVMLabel $ show c1
                           sl1 = LLVMStmLabel l1
                           l2  = LLVMLabel $ show c2
                           sl2 = LLVMStmLabel l2
                           condStm   = LLVMStmInstr (CondBranch (OI vid) (show l1) (show l2))
                           res = expStms    ++
                                [condStm]   ++
                                [sl1]       ++
                                 stms       ++
                                [sl2]
                       return res

  IfRE -> do ((vid, t), expStms) <- transExp e
             return expStms
