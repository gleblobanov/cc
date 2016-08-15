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
    SForeach t ind arr stm -> transSForeach t ind arr stm ss
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
mkDeclStm t@(TypeArr t' brs) vid = do extendVarDecl vid t
                                      return []
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
transSInit t ids e ss = do ((ptr, t'), expStms) <- transExp e
                           initStms             <- mkInitStms t ids ptr t'
                           restStms             <- transStms ss
                           return $ expStms ++ initStms ++ restStms



mkInitStms :: Type -> [Id] -> Operand -> LLVMType -> EnvState Env [LLVMStm]
mkInitStms _ [] _ _ = return []
mkInitStms t (vid:vids) val t' = do stms     <- mkInitStm t vid val t'
                                    stmsRest <- mkInitStms t vids val t'
                                    return (stms ++ stmsRest)


mkInitStm :: Type -> Id -> Operand -> LLVMType -> EnvState Env [LLVMStm]
mkInitStm t@(TypeArr _ _) vid ptr t'@(TypeArray len _) = do extendVar vid ptr t'
                                                            return []
mkInitStm t vid val _ = do (OI ptr, t') <- extendVarDecl vid t
                           let allInstr = Allocate t'
                               sAlloc   = LLVMStmAssgn ptr allInstr
                               tPtr'    = t'
                               sStore   = LLVMStmInstr (Store t' val tPtr' ptr)
                           return [sAlloc, sStore]




transAssArr :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transAssArr (EIdArr vid [InBr e1]) e2 =
  do
    ((ind, _), e1Stms) <- transExp e1
    ((val, _), e2Stms) <- transExp e2
    (arrStrPtr, arrStrType, arrElemType) <- lookupArr vid
    xPtr <- genLocal
    let xPtrGet = LLVMStmAssgn xPtr $ GetElementPtr
          arrStrType arrStrPtr [OC $ ConstInteger 1,
                                ind]

        xStore = LLVMStmInstr $ Store arrElemType val arrElemType xPtr
    return ((val, arrElemType), e1Stms ++ e2Stms ++ [xPtrGet, xStore])




transSReturn :: ReturnRest ->  EnvState Env [LLVMStm]
transSReturn rest = case rest of
  ReturnRest e -> do ((val, t'), expStms) <- transExp e
                     t <- getType
                     case t' of
                       TypeArray _ _ -> do res <- genLocal
                                           let resLoad = LLVMStmAssgn res $ Load t' val
                                               retStm  = LLVMStmInstr (Return t $ OI res)
                                           return $ expStms ++ [resLoad, retStm]
                       _ -> let retStm = LLVMStmInstr (Return t val)
                            in return $ expStms ++ [retStm]
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


-- TODO In brackets could be expression
transSForeach :: Type -> Id -> Id -> Stm -> [Stm] -> EnvState Env [LLVMStm]
transSForeach t ind arr stm stms = do foreachStms <- transForeach t ind arr stm
                                      restStms  <- transStms stms
                                      return $ foreachStms ++ restStms


transForeach :: Type -> Id -> Id -> Stm -> EnvState Env [LLVMStm]
transForeach t ind' arr stm = do
  newBlock
  (arrStrPtr, arrStrType, arrElemType) <- lookupArr arr
  indPtr <- genLocal
  let indPtrAlloc = LLVMStmAssgn indPtr $ Allocate TypeInteger
      indPtrStore = LLVMStmInstr $ Store TypeInteger (OC $ ConstInteger 0) TypeInteger indPtr

  lenPtr <- genLocal
  len    <- genLocal
  let lenPtrGet   = LLVMStmAssgn lenPtr $ GetElementPtr
        arrStrType arrStrPtr [OT TypeInteger $ OC $ ConstInteger 0,
                            OT arrElemType $ OC $ ConstInteger 0]
      lenLoad     = LLVMStmAssgn len $ Load TypeInteger (OI lenPtr)

  c1 <- getCounter
  c2  <- getCounter
  c3  <- getCounter
  let l1 = LLVMLabel $ "lab" ++ show c1
      sl1 = LLVMStmLabel l1
      l2 = LLVMLabel $ "lab" ++ show c2
      sl2 = LLVMStmLabel l2
      l3 = LLVMLabel $ "lab" ++ show c3
      sl3 = LLVMStmLabel l3
      uncond1 = LLVMStmInstr (UncondBranch (show l1))

  ind   <- genLocal
  check <- genLocal
  x     <- genLocal
  extendVar ind' (OI x) arrElemType
  let indLoad = LLVMStmAssgn ind $ Load TypeInteger (OI indPtr)
      checkCmp = LLVMStmAssgn check $ ICmp Slt TypeInteger (OI ind) (OI len)
      br = LLVMStmInstr $ CondBranch (OI check) (show l2) (show l3)

      -- xPtrGetElPtr = LLVMStmAssgn xPtr $
      --   GetElementPtr arrType
      --   arrPtr (OT TypeInteger (OC (ConstInteger 0)))
      --   (OT TypeInteger $ OI ind)

      -- xLoad     = LLVMStmAssgn x $ Load arrElemType (OI xPtr)
      xPtrGetElPtr = LLVMStmAssgn x $
        GetElementPtr arrStrType arrStrPtr [OT TypeInteger $ OC $ ConstInteger 0,
                                            OT TypeInteger $ OC $ ConstInteger 1,
                                            OT arrElemType $ OI ind]


  bodyStms <- transStms [stm]

  indIncr <- genLocal
  exitBlock
  let indIncrAdd = LLVMStmAssgn indIncr $ Add TypeInteger (OI ind) (OC $ ConstInteger 1)
      indStore   = LLVMStmInstr $ Store TypeInteger (OI indIncr) TypeInteger indPtr

  return $ [indPtrAlloc,
            indPtrStore,
            lenPtrGet,
            lenLoad,
            uncond1,
            sl1,
            indLoad,
            checkCmp,
            br,
            sl2,
            xPtrGetElPtr] ++
            bodyStms ++
           [indIncrAdd,
            indStore,
            uncond1,
            sl3]



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
