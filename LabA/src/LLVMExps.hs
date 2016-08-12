module LLVMExps where
 
import AbsJL
import LLVMSyntax
import Environment
import LLVMTypes

transExp :: Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transExp exp =   case exp of
 ETrue        -> transTrue
 EFalse       -> transFalse
 EString s    -> transString s
 EInt i       -> transInteger i
 EDouble d    -> transDouble d
 EId id       -> transId id
 EApp fid es  -> transApp fid es
 ENeg e       -> transNeg e
 ENot e       -> transNot e
 EPostIncr e  -> transIncr e Incr Post
 EPostDecr e  -> transIncr e Decr Post
 EPreIncr e   -> transIncr e Incr Pre
 EPreDecr e   -> transIncr e Decr Pre
 ETimes e1 e2  -> transAr e1 e2 ArTimes
 EDiv e1 e2    -> transAr e1 e2 ArDiv
 EMod e1 e2    -> transAr e1 e2 ArMod
 EPlus e1 e2   -> transAr e1 e2 ArPlus
 EMinus e1 e2  -> transAr e1 e2 ArMinus
 ELt e1 e2     -> transLt e1 e2
 EGt e1 e2     -> transGt e1 e2
 ELtEq e1 e2   -> transLtEq e1 e2
 EGtEq e1 e2   -> transGtEq e1 e2
 EEq e1 e2     -> transEq e1 e2
 ENEq e1 e2    -> transNEq e1 e2
 EAnd e1 e2    -> transAnd e1 e2
 EOr e1 e2     -> transOr e1 e2
 EAss (EId vid) e -> transAss vid e




--

transConst :: Constant -> LLVMType -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transConst c t = return ((OC c, t), [])

transTrue :: EnvState Env ((Operand, LLVMType), [LLVMStm])
transTrue = transConst ConstTrue TypeBoolean

transFalse :: EnvState Env ((Operand, LLVMType), [LLVMStm])
transFalse = transConst ConstFalse TypeBoolean

transInteger :: Integer -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transInteger i = transConst (ConstInteger i) TypeInteger

transDouble :: Double -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transDouble d = transConst (ConstDouble d) TypeDouble

--




transString :: String -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transString s = do cnt <- getCounter
                   env <- get
                   let gs = getGlobalStrings env
                   case lookup s gs of
                     Just (gid, len) -> do let ta = TypePtr $ TypeArray len TypeChar
                                               o  = OI gid
                                               o' = OT TypeInteger (OC (ConstInteger 0))
                                               inst = GetElementPtr ta o o' o'
                                               t =  Local $ "t" ++ show cnt
                                               sInstr = LLVMStmAssgn t inst
                                           return ((OI t, typeFromPtr ta), [sInstr])
                     Nothing -> fail $ "LLVM global variable for a string isn't found (" ++ s ++ ")."


{-
   %t1 = getelementptr [13 x i8]* @hw, i32 0, i32 0
   call void @printString(i8* %t1)
   ret i32 0
-}


transIdPtr :: Id -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transIdPtr vid = do (ptr, typ) <- lookupVar vid
                    return ((ptr, typ), [])


transId :: Id -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transId vid = do (ptr, typ) <- lookupVar vid
                 val <- genLocal
                 let s = LLVMStmAssgn val $ Load typ ptr
                 return ((OI val, typ), [s])


-- transExpPtr :: Exp -> EnvState Env ((Identifier, LLVMType), [LLVMStm])
-- transExpPtr (EId vid) = do (OI ptr, typ) <- lookupVar vid
--                            return ((ptr, typ), [])



transApp :: Id -> [Exp] -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transApp (Id "printString") [EString s] =
  do ((val, t), stms) <- transString s
     let call = LLVMStmInstr $ Call TypeVoid (Global "printString")
                    (LLVMArgs [LLVMArg (TypePtr TypeChar) val])
     return ((val, t), stms ++ [call])

transApp (Id "printInt") [e] =
  do ((val, typ), stms) <- transExp e
     let call = LLVMStmInstr $ Call TypeVoid (Global "printInt")
                    (LLVMArgs [LLVMArg typ val])
     return ((val, typ), stms ++ [call])

transApp (Id "printDouble") [e] =
  do ((val, typ), stms) <- transExp e
     let call = LLVMStmInstr $ Call TypeVoid (Global "printDouble")
                    (LLVMArgs [LLVMArg typ val])
     return ((val, typ), stms ++ [call])

transApp (Id "readInt") _ =
  do val <- genLocal
     let call = LLVMStmAssgn val $ Call TypeInteger (Global "readInt")
                    (LLVMArgs [])
     return ((OI val, TypeInteger), [call])


transApp (Id "readDouble") _ =
  do val <- genLocal
     let call = LLVMStmAssgn val $ Call TypeDouble (Global "readDouble")
                    (LLVMArgs [])
     return ((OI val, TypeDouble), [call])



transApp fid args = do executedArgsStms <- execArgs args
                       res <- genLocal
                       funs <- getFuns
                       let executedArgs = map fst executedArgsStms
                           executedSS   = concatMap snd executedArgsStms
                           llvmArgs = LLVMArgs $ map (\(val, typ) -> LLVMArg typ val)
                                                     executedArgs
                       case lookup fid funs of
                         Just (ftyp, fid', _) ->
                          let stm = case ftyp of
                                TypeVoid -> LLVMStmInstr $ Call ftyp fid' llvmArgs
                                _ -> LLVMStmAssgn res $ Call ftyp fid' llvmArgs
                          in return ((OI res, ftyp), executedSS ++ [stm])
                         Nothing -> fail $ "Function isn't found. " ++ show fid

execArgs :: [Exp] -> EnvState Env [((Operand, LLVMType), [LLVMStm])]
execArgs = mapM transExp



emitOperation :: Instruction -> EnvState Env (Operand, [LLVMStm])
emitOperation instr =
  do
    cnt <- getCounter
    let res = Local $ "t" ++ show cnt
        stm   = LLVMStmAssgn res instr
    return (OI res, [stm])



transNeg :: Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transNeg e =
  do ((lid, t), expStms) <- transExp e
     case t of
       TypeInteger -> do (lid', stms) <- emitOperation $ Sub TypeInteger
                                                            (OC $ ConstInteger 0)
                                                            lid
                         return ((lid', t), expStms ++ stms)
       TypeDouble  -> do (lid', stms) <- emitOperation $ FSub TypeDouble
                                                             (OC $ ConstDouble 0.0)
                                                             lid
                         return ((lid', t), expStms ++ stms)
       _           -> fail "transNeg: wrong type"



transNot :: Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transNot e =
  do l1c <- getCounter
     l2c <- getCounter
     l3c <- getCounter
     cc <- getCounter
     resTmp <- genLocal
     res    <- genLocal
     ((eop, t), expStms) <- transExp e
     let l1 = LLVMLabel $ "lab" ++ show l1c
         l1Stm = LLVMStmLabel l1
         l2 = LLVMLabel $ "lab" ++ show l2c
         l2Stm = LLVMStmLabel l2
         l3 = LLVMLabel $ "lab" ++ show l3c
         l3Stm = LLVMStmLabel l3
         resTmpStm = LLVMStmAssgn resTmp $ Allocate TypeBoolean
         condId = Local $ "t" ++ show cc
         condInstr = ICmp Eq
                          TypeBoolean
                          eop
                          (OC ConstTrue)
         condStm = LLVMStmAssgn condId condInstr
         brStm = LLVMStmInstr $ CondBranch (OI condId) (show l1) (show l2)
         fStm =  LLVMStmInstr $ Store TypeBoolean
                                      (OC ConstFalse)
                                      TypeBoolean
                                      resTmp
         tStm =  LLVMStmInstr $ Store TypeBoolean
                                      (OC ConstTrue)
                                      TypeBoolean
                                      resTmp
         brStm' = LLVMStmInstr $ UncondBranch $ show l3
         resStm = LLVMStmAssgn res $ Load TypeBoolean (OI resTmp)
     return ((OI res, t), expStms ++ [resTmpStm,
                                   condStm,
                                   brStm,
                                   l1Stm,
                                   fStm,
                                   brStm',
                                   l2Stm,
                                   tStm,
                                   brStm',
                                   l3Stm,
                                   resStm])




data IncrDecr = Incr | Decr
data PostPre = Post | Pre



transIncr :: Exp -> IncrDecr -> PostPre -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transIncr (EId vid) incrDecr postPre =
  do ((OI ptr, typ), expStms) <- transIdPtr vid
     val1 <- genLocal
     val2 <- genLocal
     let s1 = LLVMStmAssgn val1 $ Load typ (OI ptr)
         i2 = case incrDecr of
           Incr -> Add typ (OI val1) (OC $ ConstInteger 1)
           Decr -> Sub typ (OI val1) (OC $ ConstInteger 1)
         s2 = LLVMStmAssgn val2 i2
         s3 = LLVMStmInstr $ Store typ (OI val2) typ ptr
         res = case postPre of
           Post -> val1
           Pre  -> val2
     return ((OI res, typ), expStms ++ [s1, s2, s3])



data Ar = ArTimes | ArDiv | ArMod | ArPlus | ArMinus

transAr :: Exp -> Exp -> Ar -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transAr e1 e2 ar =
  do ((val1, typ), ss1) <- transExp e1
     ((val2, _),   ss2) <- transExp e2
     (res, ss)          <- emitOperation $
       case typ of
         TypeInteger -> case ar of
           ArTimes -> Mul  typ val1 val2
           ArDiv   -> SDiv typ val1 val2
           ArMod   -> SRem typ val1 val2
           ArPlus  -> Add  typ val1 val2
           ArMinus -> Sub  typ val1 val2
         TypeDouble -> case ar of
           ArTimes -> FMul typ val1 val2
           ArDiv   -> FDiv typ val1 val2
           ArPlus  -> FAdd typ val1 val2
           ArMinus -> FSub typ val1 val2
     return ((res, typ), ss1 ++ ss2 ++ ss)



emitCmp :: Exp -> Exp -> Cond -> EnvState Env ((Operand, LLVMType), [LLVMStm])
emitCmp e1 e2 cond  =
  do l1c <- getCounter
     l2c <- getCounter
     l3c <- getCounter
     cc  <- getCounter
     crt <- getCounter
     res <- genLocal
     ((eid1, t), expStms1) <- transExp e1
     ((eid2, _), expStms2) <- transExp e2
     let l1 = LLVMLabel $ "lab" ++ show l1c
         l1Stm = LLVMStmLabel l1
         l2 = LLVMLabel $ "lab" ++ show l2c
         l2Stm = LLVMStmLabel l2
         l3 = LLVMLabel $ "lab" ++ show l3c
         l3Stm = LLVMStmLabel l3
         resTmp = Local $ "t" ++ show crt
         i1 = Allocate TypeBoolean
         resTmpStm = LLVMStmAssgn resTmp i1
         condId = Local $ "t" ++ show cc
         condInstr = case t of
           TypeInteger -> ICmp cond t eid1 eid2
           TypeBoolean -> ICmp cond t eid1 eid2
           TypeDouble  -> FCmp (toOrderedCond cond) t eid1 eid2
         condStm = LLVMStmAssgn condId condInstr
         brStm = LLVMStmInstr $ CondBranch (OI condId) (show l1) (show l2)

         tStm =  LLVMStmInstr $ Store TypeBoolean
                                      (OC ConstTrue)
                                      TypeBoolean
                                      resTmp
         fStm =  LLVMStmInstr $ Store TypeBoolean
                                      (OC ConstFalse)
                                      TypeBoolean
                                      resTmp
         brStm' = LLVMStmInstr $ UncondBranch $ show l3
         resStm = LLVMStmAssgn res $ Load TypeBoolean $ OI resTmp
     return ((OI res, TypeBoolean), expStms1 ++ expStms2 ++ [resTmpStm, condStm, brStm, l1Stm, tStm, brStm', l2Stm, fStm, brStm', l3Stm, resStm])





-- Check if signed comparison goes

transLt :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transLt e1 e2 = emitCmp e1 e2 Slt


transGt :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transGt e1 e2 = emitCmp e1 e2 Sgt


transLtEq :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transLtEq e1 e2 = emitCmp e1 e2 Sle


transGtEq :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transGtEq e1 e2 = emitCmp e1 e2 Sge


transEq :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transEq e1 e2 = emitCmp e1 e2 Eq


transNEq :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transNEq e1 e2 = emitCmp e1 e2 Ne


data AO = AOOr | AOAnd

transAndOr :: Exp -> Exp -> AO -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transAndOr e1 e2 ao =
  do ((val1, t), expStms1) <- transExp e1
     ((val2, _), expStms2) <- transExp e2
     c1 <- getCounter
     c2 <- getCounter
     c3 <- getCounter
     ptr <- genLocal
     tmp <- genLocal
     res <- genLocal
     let l1  = LLVMLabel $ "lab" ++ show c1
         sl1 = LLVMStmLabel l1
         l2  = LLVMLabel $ "lab" ++ show c2
         sl2 = LLVMStmLabel l2
         l3  = LLVMLabel $ "lab" ++ show c3
         sl3 = LLVMStmLabel l3
         allStm    = LLVMStmAssgn ptr $ Allocate TypeBoolean
         condStm   = LLVMStmInstr (CondBranch val1 (show l1) (show l2))
         uncondStm = LLVMStmInstr (UncondBranch (show l3))
         oper      = LLVMStmAssgn tmp $ case ao of
           AOAnd -> And TypeBoolean val1 val2
           AOOr  -> Or TypeBoolean val1 val2
         storeTmpStm    = LLVMStmInstr $ Store TypeBoolean (OI tmp) TypeBoolean ptr
         storeInpStm    = LLVMStmInstr $ Store TypeBoolean val1 TypeBoolean ptr
         loadResStm   = LLVMStmAssgn res $ Load TypeBoolean $ OI ptr
         resStms = case ao of
           AOAnd ->
             expStms1      ++
             [allStm]      ++
             [condStm]     ++
             [sl1]         ++
             expStms2      ++
             [oper]        ++
             [storeTmpStm] ++
             [uncondStm]   ++
             [sl2]         ++
             [storeInpStm] ++
             [uncondStm]   ++
             [sl3]         ++
             [loadResStm]
           AOOr  ->
             expStms1      ++
             [allStm]      ++
             [condStm]     ++
             [sl1]         ++
             [storeInpStm] ++
             [uncondStm]   ++
             [sl2]         ++
             expStms2      ++
             [oper]        ++
             [storeTmpStm] ++
             [uncondStm]   ++
             [sl3]         ++
             [loadResStm]

     return ((OI res, t), resStms)



transOr :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transOr e1 e2 = transAndOr e1 e2 AOOr


transAnd :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transAnd e1 e2 = transAndOr e1 e2 AOAnd


transAss :: Id -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transAss vid e =
  do
    (OI ref, typ) <- lookupVar vid
    ((val, _), expStms) <- transExp e
    let s = LLVMStmInstr $ Store typ
            val
            typ
            ref
    return ((val, typ), expStms ++ [s])



