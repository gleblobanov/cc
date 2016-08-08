module LLVMExps where

import AbsJL
import LLVMSyntax
import Environment
import LLVMTypes

transExp :: Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
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
 _                -> return ((EmptyLocalId, TypeVoid), [])




--

transConst :: Constant -> LLVMType -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transConst c t = do cnt <- getCounter
                    let lid =  LocalId $ "t" ++ show cnt
                        id = IdentLocal lid
                        i1 = Allocate t
                        s1 = LLVMStmAssgn id i1
                        i2 = Store t (OC c) (typeToPtr t) id
                        s2 = LLVMStmInstr i2
                    return ((lid, t), [s1, s2])



transTrue :: EnvState Env ((LocalId, LLVMType), [LLVMStm])
transTrue = transConst ConstTrue TypeBoolean



transFalse :: EnvState Env ((LocalId, LLVMType), [LLVMStm])
transFalse = transConst ConstFalse TypeBoolean



transInteger :: Integer -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transInteger i = transConst (ConstInteger i) TypeInteger



transDouble :: Double -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transDouble d = transConst (ConstDouble d) TypeDouble

--




transString :: String -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transString s = do cnt <- getCounter
                   env <- get
                   let gs = getGlobalStrings env
                   case lookup s gs of
                     Just (gid, len) -> do let ta = TypeArrayPtr len TypeChar
                                               o  = OI $ IdentGlobal gid
                                               o' = OT TypeInteger (OC (ConstInteger 0))
                                               inst = GetElementPtr ta o o' o'
                                               t =  LocalId $ "t" ++ show cnt
                                               sInstr = LLVMStmAssgn (IdentLocal t) inst
                                           return ((t, typeFromPtr ta), [sInstr])
                     Nothing -> fail $ "LLVM global variable for a string isn't found (" ++ s ++ ")."


{-
   %t1 = getelementptr [13 x i8]* @hw, i32 0, i32 0
   call void @printString(i8* %t1)
   ret i32 0
-}




transId :: Id -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transId id = do cnt <- getCounter
                (Scope st cnt) <- getScope
                case lookup id (head st) of -- TODO check if it's right to use the head
                  Just (IdentLocal lid, t) -> return ((lid, t), [])
                  Nothing -> fail "Variable not found"



transApp :: Id -> [Exp] -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transApp (Id "printString") [EString s] =
  do ((lid, t), stms) <- transString s
     let call = Call TypeVoid (IdentGlobal $ GlobalId "printString")
                    (LLVMArgs [LLVMArg (TypePtr TypeCharPtr) lid])
     return ((lid, t), stms ++ [LLVMStmInstr call])

transApp _ _ = undefined



emitOperation :: Instruction -> EnvState Env (LocalId, [LLVMStm])
emitOperation instr =
  do
    cnt <- getCounter
    let lid' = LocalId $ "t" ++ show cnt
        stm   = LLVMStmAssgn (IdentLocal lid') instr
    return (lid', [stm])



transNeg :: Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transNeg e =
  do ((lid, t), expStms) <- transExp e
     case t of
       TypeInteger -> do (lid', stms) <- emitOperation $ Sub TypeInteger
                                                            (OC $ ConstInteger 0)
                                                            (OI (IdentLocal lid))
                         return ((lid', t), expStms ++ stms)
       TypeDouble  -> do (lid', stms) <- emitOperation $ FSub TypeDouble
                                                             (OC $ ConstDouble 0.0)
                                                             (OI (IdentLocal lid))
                         return ((lid', t), expStms ++ stms)
       _           -> fail "transNeg: wrong type"



transNot :: Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transNot e =
  do l1c <- getCounter
     l2c <- getCounter
     l3c <- getCounter
     cc <- getCounter
     ((eid, t), expStms) <- transExp e
     let l1 = LLVMLabel $ show $ "label" ++ show l1c
         l1Stm = LLVMStmLabel l1
         l2 = LLVMLabel $ show $ "label" ++ show l2c
         l2Stm = LLVMStmLabel l2
         l3 = LLVMLabel $ show $ "label" ++ show l3c
         l3Stm = LLVMStmLabel l3
         condId = IdentLocal $ LocalId $ "t" ++ show cc
         condInstr = ICmp Eq
                          TypeBoolean
                          (OI $ IdentLocal eid)
                          (OC ConstTrue)
         condStm = LLVMStmAssgn condId condInstr
         brStm = LLVMStmInstr $ CondBranch (OI condId) (show l1) (show l2)
         fStm =  LLVMStmInstr $ Store TypeBoolean
                                      (OC ConstFalse)
                                      TypeBooleanPtr
                                      (IdentLocal eid)
         tStm =  LLVMStmInstr $ Store TypeBoolean
                                      (OC ConstTrue)
                                      TypeBooleanPtr
                                      (IdentLocal eid)
         brStm' = LLVMStmInstr $ UncondBranch $ show l3
     return ((eid, t), expStms ++ [condStm, brStm, l1Stm, fStm, brStm', l2Stm, tStm, l3Stm])






transPostIncr :: Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transPostIncr e =
  do ((lid, t), expStms) <- transExp e  -- TODO Check passing a pointer but a value everywhere
     c1 <- getCounter
     c2 <- getCounter
     let lid1 = LocalId $ "t" ++ show c1
         i1 = Allocate t
         s1 = LLVMStmAssgn (IdentLocal lid1) i1
         s2 = LLVMStmInstr $ Store t (OI $ IdentLocal lid) (typeToPtr t) (IdentLocal lid1)

         lid2 = LocalId $ "t" ++ show c2
         i3 = Add t (OI $ IdentLocal lid) (OC $ ConstInteger 1)
         s3 = LLVMStmAssgn (IdentLocal lid2) i3
         s4 = LLVMStmInstr $ Store t (OI $ IdentLocal lid2) (typeToPtr t) (IdentLocal lid)
     return ((lid1, t), expStms ++ [s1, s2, s3, s4])





transPostDecr :: Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transPostDecr e =
  do ((lid, t), expStms) <- transExp e
     c1 <- getCounter
     c2 <- getCounter
     let lid1 = LocalId $ "t" ++ show c1
         i1 = Allocate t
         s1 = LLVMStmAssgn (IdentLocal lid1) i1
         s2 = LLVMStmInstr $ Store t (OI $ IdentLocal lid) (typeToPtr t) (IdentLocal lid1)

         lid2 = LocalId $ "t" ++ show c2
         i3 = Sub t (OI $ IdentLocal lid) (OC $ ConstInteger 1)
         s3 = LLVMStmAssgn (IdentLocal lid2) i3
         s4 = LLVMStmInstr $ Store t (OI $ IdentLocal lid2) (typeToPtr t) (IdentLocal lid)
     return ((lid1, t), expStms ++ [s1, s2, s3, s4])








transPreIncr  :: Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transPreIncr e =
  do ((lid, t), expStms) <- transExp e
     c1 <- getCounter
     let lid1 = LocalId $ "t" ++ show c1
         i1 = Add t (OI $ IdentLocal lid) (OC $ ConstInteger 1)
         s1 = LLVMStmAssgn (IdentLocal lid1) i1
         s2 = LLVMStmInstr $ Store t (OI $ IdentLocal lid1) (typeToPtr t) (IdentLocal lid)
     return ((lid, t), expStms ++ [s1, s2])









transPreDecr  :: Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transPreDecr e =
  do ((lid, t), expStms) <- transExp e
     c1 <- getCounter
     let lid1 = LocalId $ "t" ++ show c1
         i1 = Sub t (OI $ IdentLocal lid) (OC $ ConstInteger 1)
         s1 = LLVMStmAssgn (IdentLocal lid1) i1
         s2 = LLVMStmInstr $ Store t (OI $ IdentLocal lid1) (typeToPtr t) (IdentLocal lid)
     return ((lid, t), expStms ++ [s1, s2])





transTimes :: Exp -> Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transTimes e1 e2 =
  do ((lid1, t), expStms1) <- transExp e1
     ((lid2, _), expStms2) <- transExp e2
     case t of
       TypeInteger -> do (lid', stms) <- emitOperation $ Mul TypeInteger
                                                             (OI (IdentLocal lid1))
                                                             (OI (IdentLocal lid2))
                         return ((lid', t), expStms1 ++ expStms2 ++ stms)
       TypeDouble  -> do (lid', stms) <- emitOperation $ FMul TypeDouble
                                                              (OI (IdentLocal lid1))
                                                              (OI (IdentLocal lid2))
                         return ((lid', t), expStms1 ++ expStms2 ++ stms)
       _           -> fail "transTimes: wrong type"





transDiv :: Exp -> Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transDiv e1 e2 =
  do ((lid1, t), expStms1) <- transExp e1
     ((lid2, _), expStms2) <- transExp e2
     case t of
       TypeInteger -> do (lid', stms) <- emitOperation $ SDiv TypeInteger
                                                             (OI (IdentLocal lid1))
                                                             (OI (IdentLocal lid2))
                         return ((lid', t), expStms1 ++ expStms2 ++ stms)
       TypeDouble  -> do (lid', stms) <- emitOperation $ FDiv TypeDouble
                                                              (OI (IdentLocal lid1))
                                                              (OI (IdentLocal lid2))
                         return ((lid', t), expStms1 ++ expStms2 ++ stms)
       _           -> fail "transDiv: wrong type"






transMod :: Exp -> Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transMod e1 e2 =
  do ((lid1, t), expStms1) <- transExp e1
     ((lid2, _), expStms2) <- transExp e2
     (lid', stms) <- emitOperation $ SRem TypeInteger
                                         (OI (IdentLocal lid1))
                                         (OI (IdentLocal lid2))
     return ((lid', t), expStms1 ++ expStms2 ++ stms)






transPlus :: Exp -> Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transPlus e1 e2 =
  do ((lid1, t), expStms1) <- transExp e1
     ((lid2, _), expStms2) <- transExp e2
     case t of
       TypeInteger -> do (lid', stms) <- emitOperation $ Add TypeInteger
                                                             (OI (IdentLocal lid1))
                                                             (OI (IdentLocal lid2))
                         return ((lid', t), expStms1 ++ expStms2 ++ stms)
       TypeDouble  -> do (lid', stms) <- emitOperation $ FAdd TypeDouble
                                                              (OI (IdentLocal lid1))
                                                              (OI (IdentLocal lid2))
                         return ((lid', t), expStms1 ++ expStms2 ++ stms)
       _           -> fail "transAdd: wrong type"







transMinus :: Exp -> Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transMinus e1 e2 =
  do ((lid1, t), expStms1) <- transExp e1
     ((lid2, _), expStms2) <- transExp e2
     case t of
       TypeInteger -> do (lid', stms) <- emitOperation $ Sub TypeInteger
                                                             (OI (IdentLocal lid1))
                                                             (OI (IdentLocal lid2))
                         return ((lid', t), expStms1 ++ expStms2 ++ stms)
       TypeDouble  -> do (lid', stms) <- emitOperation $ FSub TypeDouble
                                                              (OI (IdentLocal lid1))
                                                              (OI (IdentLocal lid2))
                         return ((lid', t), expStms1 ++ expStms2 ++ stms)
       _           -> fail "transSub: wrong type"






emitCmp :: Exp -> Exp -> Cond -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
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
         res = LocalId $ "t" ++ show cr
         i1 = Allocate TypeBoolean
         resStm = LLVMStmAssgn (IdentLocal res) i1
         condId = IdentLocal $ LocalId $ "t" ++ show cc
         condInstr = ICmp cond
                          TypeBoolean
                          (OI $ IdentLocal eid1)
                          (OI $ IdentLocal eid2)
         condStm = LLVMStmAssgn condId condInstr
         brStm = LLVMStmInstr $ CondBranch (OI condId) (show l1) (show l2)
         fStm =  LLVMStmInstr $ Store TypeBoolean
                                      (OC ConstTrue)
                                      TypeBooleanPtr
                                      (IdentLocal res)
         tStm =  LLVMStmInstr $ Store TypeBoolean
                                      (OC ConstFalse)
                                      TypeBooleanPtr
                                      (IdentLocal res)
         brStm' = LLVMStmInstr $ UncondBranch $ show l3
     return ((res, TypeBoolean), expStms1 ++ expStms2 ++ [resStm, condStm, brStm, l1Stm, fStm, brStm', l2Stm, tStm, l3Stm])





-- Check if signed comparison goes

transLt :: Exp -> Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transLt e1 e2 = emitCmp e1 e2 Slt


transGt :: Exp -> Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transGt e1 e2 = emitCmp e1 e2 Sgt


transLtEq :: Exp -> Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transLtEq e1 e2 = emitCmp e1 e2 Sle


transGtEq :: Exp -> Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transGtEq e1 e2 = emitCmp e1 e2 Sge


transEq :: Exp -> Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transEq e1 e2 = emitCmp e1 e2 Eq


transNEq :: Exp -> Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transNEq e1 e2 = emitCmp e1 e2 Ne



transAnd :: Exp -> Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transAnd e1 e2 =
  do ((lid1, t), expStms1) <- transExp e1
     ((lid2, _), expStms2) <- transExp e2
     (lid', stms) <- emitOperation $ And TypeBoolean
                                         (OI (IdentLocal lid1))
                                         (OI (IdentLocal lid2))
     return ((lid', t), expStms1 ++ expStms2 ++ stms)



transOr :: Exp -> Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transOr e1 e2 =
  do ((lid1, t), expStms1) <- transExp e1
     ((lid2, _), expStms2) <- transExp e2
     (lid', stms) <- emitOperation $ Or TypeBoolean
                                         (OI (IdentLocal lid1))
                                         (OI (IdentLocal lid2))
     return ((lid', t), expStms1 ++ expStms2 ++ stms)




transAss :: Id -> Exp -> EnvState Env ((LocalId, LLVMType), [LLVMStm])
transAss vid e =
  do
    res <- lookupVar vid
    case res of
      Nothing     -> fail $ "Variable is not defined: " ++ show vid
      Just (n, t) -> do ((lid, _), expStms) <- transExp e
                        let s = LLVMStmInstr $ Store t
                                      (OI (IdentLocal lid))
                                      (typeToPtr t)
                                      n
                        return ((LocalId $ getIdentStr n, t), expStms ++ [s])



