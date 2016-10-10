module LLVMStms where

import Control.Monad (liftM)

import AbsJL
import LLVMSyntax
import LLVMExps
import Environment


-- | Translates JL statements to LLVM statements
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



-- | Translates expression statements
transSExp :: Exp -> [Stm] -> EnvState Env [LLVMStm]
transSExp e ss = do (_, expStms) <- transExp e
                    restStms <- transStms ss
                    return $ expStms ++ restStms


-- | Translates declarations
transSDecls :: Type -> [Id] -> [Stm] -> EnvState Env [LLVMStm]
transSDecls t ids ss = do declStms <- transDecl t ids
                          restStms <- transStms ss
                          return $ declStms ++ restStms

-- | Walks through a list of declaration and generates LLVM statements
transDecl :: Type -> [Id] -> EnvState Env [LLVMStm]
transDecl _ []  = return []
transDecl t (vid:vids) = do stms  <- mkDeclStm t vid
                            stms' <- transDecl t vids
                            return $ stms ++ stms'

-- | Helper for transDecl. Also it initializes variables to the standard values
mkDeclStm :: Type -> Id -> EnvState Env [LLVMStm]
mkDeclStm t@(TypeArr _ brs) vid = extendVarDeclArr vid t brs >>
                                   return []
mkDeclStm t vid = do (OI ptr, TypePtr t') <- extendVarDecl vid t
                     let allInstr = Allocate t'
                         allstm   = LLVMStmAssgn ptr allInstr
                         val = OC $ case t' of
                           TypeInteger -> ConstInteger 0
                           TypeDouble  -> ConstDouble 0.0
                           TypeBoolean -> ConstFalse
                         sStore   = LLVMStmInstr (Store t' val (TypePtr t') ptr)
                     return [allstm, sStore]


-- | Translates initialization statement both for arrays and other types
transSInit :: Type -> [Id] -> Exp -> [Stm] -> EnvState Env [LLVMStm]
transSInit (TypeArr typ _) [vid] (ENew _ inbrs) stms =
  do (inbrs', inbrsStms) <- transInbrs inbrs
     (OI ptr, t) <- extendVarInitArr vid typ inbrs'
     let sArrPtr = LLVMStmAssgn ptr $ AllocateAlign t 1024
     arrStms <- makeArr ptr t
     restStms <- transStms stms
     return $ inbrsStms ++ [sArrPtr] ++ arrStms ++ restStms
transSInit t ids e ss = do ((ptr, t'), expStms) <- transExp e
                           initStms             <- mkInitStms t ids ptr t'
                           restStms             <- transStms ss
                           return $ expStms ++ initStms ++ restStms


-- | Helper for transSInit
mkInitStms :: Type -> [Id] -> Operand -> LLVMType -> EnvState Env [LLVMStm]
mkInitStms _ [] _ _ = return []
mkInitStms t (vid:vids) val t' = do stms     <- mkInitStm t vid val t'
                                    stmsRest <- mkInitStms t vids val t'
                                    return (stms ++ stmsRest)


mkInitStm :: Type -> Id -> Operand -> LLVMType -> EnvState Env [LLVMStm]
mkInitStm (TypeArr t brs) vid val _ = do (OI ptr, t') <- extendVarDeclArr vid t brs
                                         changeArr (val, t') vid
                                         return []
mkInitStm t vid val _ = do (OI ptr, TypePtr t') <- extendVarDecl vid t
                           let allInstr = Allocate t'
                               sAlloc   = LLVMStmAssgn ptr allInstr
                               sStore   = LLVMStmInstr (Store t' val (TypePtr t') ptr)
                           return [sAlloc, sStore]









-- | Translates return statements
transSReturn :: ReturnRest ->  EnvState Env [LLVMStm]
transSReturn rest = case rest of
  ReturnRest e -> do ((val, t'), expStms) <- transExp e
                     t <- getType
                     case t' of
                       (TypeArray _ _) -> do res <- genLocal
                                             let resLoad = LLVMStmAssgn res $ Load (TypePtr t') val
                                                 retStm  = LLVMStmInstr (Return t $ OI res)
                                             return $ expStms ++ [resLoad, retStm]
                       _ -> let retStm = LLVMStmInstr (Return t val)
                            in return $ expStms ++ [retStm]
  ReturnRestEmpt -> return [LLVMStmInstr ReturnVoid]




-- | Translates while statements
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
                                [uncondStm] ++
                                [sl1]       ++
                                 expStms    ++
                                [condStm]   ++
                                [sl2]       ++
                                 stms       ++
                                [uncondStm] ++
                                [sl3]
                      return res



-- | Translates blocks of statements
transSBlock :: [Stm] -> [Stm] -> EnvState Env [LLVMStm]
transSBlock stms ss = do blockStms <- transBlock stms
                         restStms  <- transStms ss
                         return $ blockStms ++ restStms

transBlock :: [Stm] -> EnvState Env [LLVMStm]
transBlock stms = do newBlock
                     stms' <- transStms stms
                     exitBlock
                     return stms'


-- | Translates Foreach loop
transSForeach :: Type -> Id -> Id -> Stm -> [Stm] -> EnvState Env [LLVMStm]
transSForeach t ind arr stm stms = do foreachStms <- transForeach t ind arr stm
                                      restStms  <- transStms stms
                                      return $ foreachStms ++ restStms


transForeach :: Type -> Id -> Id -> Stm -> EnvState Env [LLVMStm]
transForeach t ind' arr stm = do
  newBlock
  (arrStrPtr, arrStrType) <- lookupVar arr
  indPtr <- genLocal
  let indPtrAlloc = LLVMStmAssgn indPtr $ Allocate TypeInteger
      indPtrStore = LLVMStmInstr $
        Store TypeInteger (OC $ ConstInteger 0) (TypePtr TypeInteger) indPtr

  lenPtr <- genLocal
  len    <- genLocal
  let lenPtrGet   = LLVMStmAssgn lenPtr $ GetElementPtr
        (TypePtr arrStrType) arrStrPtr [OT TypeInteger $ OC $ ConstInteger 0,
                            OT TypeInteger $ OC $ ConstInteger 0]
      lenLoad     = LLVMStmAssgn len $ Load (TypePtr TypeInteger) (OI lenPtr)

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
  xPtr  <- genLocal
  x     <- genLocal
  let arrElemType = getElemType arrStrType 1
  case arrElemType of
        TypeArray _ _ -> extendVar ind' (OI xPtr) arrElemType
        _             -> extendVar ind' (OI x) arrElemType

  let indLoad = LLVMStmAssgn ind $ Load (TypePtr TypeInteger) (OI indPtr)
      checkCmp = LLVMStmAssgn check $ ICmp Slt TypeInteger (OI ind) (OI len)
      br = LLVMStmInstr $ CondBranch (OI check) (show l2) (show l3)

      xPtrGetElPtr = LLVMStmAssgn xPtr $
        GetElementPtr (TypePtr arrStrType) arrStrPtr [OT TypeInteger $ OC $ ConstInteger 0,
                                            OT TypeInteger $ OC $ ConstInteger 1,
                                            OT TypeInteger $ OI ind]
      xLoad = case arrElemType of
        TypeArray _ _ -> LLVMStmEmpty
        _             -> LLVMStmAssgn x $ Load (TypePtr arrElemType) (OI xPtr)

  bodyStms <- transStms [stm]

  indIncr <- genLocal
  exitBlock
  let indIncrAdd = LLVMStmAssgn indIncr $ Add TypeInteger (OI ind) (OC $ ConstInteger 1)
      indStore   = LLVMStmInstr $ Store TypeInteger (OI indIncr) (TypePtr TypeInteger) indPtr

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
            xPtrGetElPtr,
            xLoad] ++
            bodyStms ++
           [indIncrAdd,
            indStore,
            uncond1,
            sl3]



-- | Translates if construction
transSIf :: Exp -> IfRest -> [Stm] -> EnvState Env [LLVMStm]
transSIf e r ss = do ifStms <- transIf e r
                     restStms  <- transStms ss
                     return $ ifStms ++ restStms


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
