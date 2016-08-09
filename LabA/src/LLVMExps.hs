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
 _                -> return ((EmptyId, TypeVoid), [])




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
                     Just (gid, len) -> do let ta = TypeArray len TypeChar
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




transId :: Id -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transId vid = do res <- lookupVar vid
                 return (res, [])




transApp :: Id -> [Exp] -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transApp (Id "printString") [EString s] =
  do ((OI lid, t), stms) <- transString s
     res <- genLocal
     let call = LLVMStmAssgn res $ Call TypeVoid (Global "printString")
                    (LLVMArgs [LLVMArg (TypePtr TypeChar) lid])
     return ((OI res, t), stms ++ [call])

transApp (Id "printInt") [e] =
  do ((OI lid, t), stms) <- transExp e
     res <- genLocal
     let call = LLVMStmAssgn res $ Call TypeVoid (Global "printInt")
                    (LLVMArgs [LLVMArg (TypePtr TypeInteger) lid])
     return ((OI res, t), stms ++ [call])

transApp (Id "printDouble") [e] =
  do ((OI lid, t), stms) <- transExp e
     res <- genLocal
     let call = LLVMStmAssgn res $ Call TypeVoid (Global "printDouble")
                    (LLVMArgs [LLVMArg (TypePtr TypeDouble) lid])
     return ((OI res, t), stms ++ [call])

transApp fid args = do executedArgsStms <- execArgs args
                       res <- genLocal
                       funs <- getFuns
                       let executedArgs =map fst executedArgsStms
                           executedSS   = concatMap snd executedArgsStms
                           llvmArgs = LLVMArgs $ map ((\(ident, typ) -> LLVMArg typ ident) .
                                                      (\(OI i, t) -> (i, t))) executedArgs
                       case lookup fid funs of
                         Just (ftyp, fid', _) ->
                          let stm = LLVMStmAssgn res $ Call ftyp fid' llvmArgs
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
     let l1 = LLVMLabel $ show $ "label" ++ show l1c
         l1Stm = LLVMStmLabel l1
         l2 = LLVMLabel $ show $ "label" ++ show l2c
         l2Stm = LLVMStmLabel l2
         l3 = LLVMLabel $ show $ "label" ++ show l3c
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
                                   l3Stm,
                                   resStm])






transPostIncr :: Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transPostIncr e =
  do ((lid, t), expStms) <- transExp e  -- TODO Check passing a pointer but a value everywhere
     c1 <- getCounter
     c2 <- getCounter
     let lid1 = Local $ "t" ++ show c1
         i1 = Allocate t
         s1 = LLVMStmAssgn lid1 i1
         s2 = LLVMStmInstr $ Store t lid) t lid1

         lid2 = Local $ "t" ++ show c2
         i3 = Add t (OI lid) (OC $ ConstInteger 1)
         s3 = LLVMStmAssgn lid2 i3
         s4 = LLVMStmInstr $ Store t (OI lid2) t lid
     return ((lid1, t), expStms ++ [s1, s2, s3, s4])





transPostDecr :: Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transPostDecr e =
  do ((lid, t), expStms) <- transExp e
     c1 <- getCounter
     c2 <- getCounter
     let lid1 = Local $ "t" ++ show c1
         i1 = Allocate t
         s1 = LLVMStmAssgn lid1 i1
         s2 = LLVMStmInstr $ Store t (OI lid) t lid1

         lid2 = Local $ "t" ++ show c2
         i3 = Sub t (OI lid) (OC $ ConstInteger 1)
         s3 = LLVMStmAssgn lid2 i3
         s4 = LLVMStmInstr $ Store t (OI lid2) t lid
     return ((lid1, t), expStms ++ [s1, s2, s3, s4])








transPreIncr  :: Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transPreIncr e =
  do ((lid, t), expStms) <- transExp e
     c1 <- getCounter
     let lid1 = Local $ "t" ++ show c1
         i1 = Add t (OI lid) (OC $ ConstInteger 1)
         s1 = LLVMStmAssgn lid1 i1
         s2 = LLVMStmInstr $ Store t (OI lid1) t lid
     return ((lid, t), expStms ++ [s1, s2])









transPreDecr  :: Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transPreDecr e =
  do ((lid, t), expStms) <- transExp e
     c1 <- getCounter
     let lid1 = Local $ "t" ++ show c1
         i1 = Sub t (OI lid) (OC $ ConstInteger 1)
         s1 = LLVMStmAssgn lid1 i1
         s2 = LLVMStmInstr $ Store t (OI lid1) t lid
     return ((lid, t), expStms ++ [s1, s2])





transTimes :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transTimes e1 e2 =
  do ((lid1, t), expStms1) <- transExp e1
     ((lid2, _), expStms2) <- transExp e2
     case t of
       TypeInteger -> do (lid', stms) <- emitOperation $ Mul TypeInteger
                                                             (OI lid1)
                                                             (OI lid2)
                         return ((lid', t), expStms1 ++ expStms2 ++ stms)
       TypeDouble  -> do (lid', stms) <- emitOperation $ FMul TypeDouble
                                                              (OI lid1)
                                                              (OI lid2)
                         return ((lid', t), expStms1 ++ expStms2 ++ stms)
       _           -> fail "transTimes: wrong type"





transDiv :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transDiv e1 e2 =
  do ((lid1, t), expStms1) <- transExp e1
     ((lid2, _), expStms2) <- transExp e2
     case t of
       TypeInteger -> do (lid', stms) <- emitOperation $ SDiv TypeInteger
                                                             (OI lid1)
                                                             (OI lid2)
                         return ((lid', t), expStms1 ++ expStms2 ++ stms)
       TypeDouble  -> do (lid', stms) <- emitOperation $ FDiv TypeDouble
                                                              (OI lid1)
                                                              (OI lid2)
                         return ((lid', t), expStms1 ++ expStms2 ++ stms)
       _           -> fail "transDiv: wrong type"






transMod :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transMod e1 e2 =
  do ((lid1, t), expStms1) <- transExp e1
     ((lid2, _), expStms2) <- transExp e2
     (lid', stms) <- emitOperation $ SRem TypeInteger
                                         (OI lid1)
                                         (OI lid2)
     return ((lid', t), expStms1 ++ expStms2 ++ stms)






transPlus :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transPlus e1 e2 =
  do ((lid1, t), expStms1) <- transExp e1
     ((lid2, _), expStms2) <- transExp e2
     case t of
       TypeInteger -> do (lid', stms) <- emitOperation $ Add TypeInteger
                                                             (OI lid1)
                                                             (OI lid2)
                         return ((lid', t), expStms1 ++ expStms2 ++ stms)
       TypeDouble  -> do (lid', stms) <- emitOperation $ FAdd TypeDouble
                                                              (OI lid1)
                                                              (OI lid2)
                         return ((lid', t), expStms1 ++ expStms2 ++ stms)
       _           -> fail "transAdd: wrong type"







transMinus :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transMinus e1 e2 =
  do ((lid1, t), expStms1) <- transExp e1
     ((lid2, _), expStms2) <- transExp e2
     case t of
       TypeInteger -> do (lid', stms) <- emitOperation $ Sub TypeInteger
                                                             (OI lid1)
                                                             (OI lid2)
                         return ((lid', t), expStms1 ++ expStms2 ++ stms)
       TypeDouble  -> do (lid', stms) <- emitOperation $ FSub TypeDouble
                                                              (OI lid1)
                                                              (OI lid2)
                         return ((lid', t), expStms1 ++ expStms2 ++ stms)
       _           -> fail "transSub: wrong type"






emitCmp :: Exp -> Exp -> Cond -> EnvState Env ((Operand, LLVMType), [LLVMStm])
emitCmp e1 e2 cond  =
  do l1c <- getCounter
     l2c <- getCounter
     l3c <- getCounter
     cc  <- getCounter
     cr  <- getCounter
     ((eid1, t), expStms1) <- transExp e1
     ((eid2, _), expStms2) <- transExp e2
     let l1 = LLVMLabel $ show $ "label" ++ show l1c
         l1Stm = LLVMStmLabel l1
         l2 = LLVMLabel $ show $ "label" ++ show l2c
         l2Stm = LLVMStmLabel l2
         l3 = LLVMLabel $ show $ "label" ++ show l3c
         l3Stm = LLVMStmLabel l3
         res = Local $ "t" ++ show cr
         i1 = Allocate TypeBoolean
         resStm = LLVMStmAssgn res i1
         condId = Local $ "t" ++ show cc
         condInstr = ICmp cond
                          TypeBoolean
                          (OI eid1)
                          (OI eid2)
         condStm = LLVMStmAssgn condId condInstr
         brStm = LLVMStmInstr $ CondBranch (OI condId) (show l1) (show l2)
         fStm =  LLVMStmInstr $ Store TypeBoolean
                                      (OC ConstTrue)
                                      TypeBoolean
                                      res
         tStm =  LLVMStmInstr $ Store TypeBoolean
                                      (OC ConstFalse)
                                      TypeBoolean
                                      res
         brStm' = LLVMStmInstr $ UncondBranch $ show l3
     return ((res, TypeBoolean), expStms1 ++ expStms2 ++ [resStm, condStm, brStm, l1Stm, fStm, brStm', l2Stm, tStm, l3Stm])





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



transAnd :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transAnd e1 e2 =
  do ((lid1, t), expStms1) <- transExp e1
     ((lid2, _), expStms2) <- transExp e2
     (lid', stms) <- emitOperation $ And TypeBoolean
                                         (OI lid1)
                                         (OI lid2)
     return ((lid', t), expStms1 ++ expStms2 ++ stms)



transOr :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transOr e1 e2 =
  do ((lid1, t), expStms1) <- transExp e1
     ((lid2, _), expStms2) <- transExp e2
     (lid', stms) <- emitOperation $ Or TypeBoolean
                                         (OI lid1)
                                         (OI lid2)
     return ((lid', t), expStms1 ++ expStms2 ++ stms)




transAss :: Id -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transAss vid e =
  do
    (n, t) <- lookupVar vid
    ((lid, _), expStms) <- transExp e
    let s = LLVMStmInstr $ Store t
            (OI lid)
            t
            n
    return ((n, t), expStms ++ [s])



