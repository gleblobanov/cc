module LLVMExps where

import Data.List (intersperse)

import AbsJL
import LLVMSyntax
import Environment

-- | Translates expressions
transExp :: Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transExp exp =   case exp of
 ETrue        -> transTrue
 EFalse       -> transFalse
 EString s    -> transString s
 EInt i       -> transInteger i
 EIdArr id inbrs -> transIdArr id inbrs
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
 EAss arr@(EIdArr vid inbrs) e -> transAssArr arr e
 ELength arr    -> transLen arr




-- | Translates constants
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




-- | Translates strings
transString :: String -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transString s = do cnt <- getCounter
                   env <- get
                   let gs = getGlobalStrings env
                   case lookup s gs of
                     Just (gid, len) -> do let ta = TypeArrayInnerLen len TypeChar
                                               o  = OI gid
                                               o' = OT TypeInteger (OC (ConstInteger 0))
                                               inst = GetElementPtr (TypePtr ta) o [o', o']
                                               t =  Local $ "t" ++ show cnt
                                               sInstr = LLVMStmAssgn t inst
                                           return ((OI t, typeFromPtr ta), [sInstr])
                     Nothing -> fail $ "LLVM global variable for a string isn't found (" ++ s ++ ")."


-- | Translates Id expression for the case when it refers to a pointer
transIdPtr :: Id -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transIdPtr vid = do (ptr, typ) <- lookupVar vid
                    return ((ptr, typ), [])


-- | Translates Id expression for all other cases
transId :: Id -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transId vid = do (ptr, typ) <- lookupVar vid
                 case typ of
                   TypePtr t -> do val <- genLocal
                                   let s = LLVMStmAssgn val $ Load (TypePtr t) ptr
                                   return ((OI val, t), [s])
                   _        -> return ((ptr, typ), [])



-- | Translates access to elements of an array
transIdArr :: Id -> [InBr] -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transIdArr vid inbrs =  do
    (arrOp, arrType)   <- lookupVar vid
    (ots, inbrStms)    <- inbrToPtrs inbrs

    xPtr <- genLocal
    load <- genLocal
    let ots' = [OT TypeInteger $ OC $ ConstInteger 0,
                OT TypeInteger $ OC $ ConstInteger 1] ++
               intersperse (OT TypeInteger $ OC $ ConstInteger 1) ots
        xPtrGet = LLVMStmAssgn xPtr $ GetElementPtr
          (TypePtr arrType) arrOp ots'
        arrElemType = getElemType arrType $ toInteger $ length inbrs
        xLoad = case arrElemType of
          TypeArray _ _ ->  LLVMStmEmpty
          _             ->  LLVMStmAssgn load $ Load (TypePtr arrElemType) (OI xPtr)
        loadRes = case arrElemType of
          TypeArray _ _ -> OI xPtr
          _             -> OI load
    return ((loadRes, arrElemType), inbrStms ++ [xPtrGet, xLoad])


-- | Translates application of third-party functions
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


-- | Translates application of user-defined functions
transApp fid args = do executedArgsStms <- execArgs args
                       funs <- getFuns
                       out <- genLocal
                       res <- genLocal

                       call <- genLocal
                       load <- genLocal
                       alloca <- genLocal
                       let executedArgs = map fst executedArgsStms
                           executedSS   = concatMap snd executedArgsStms
                       (llvmArgs, stms) <- processArgs executedArgs
                       case lookup fid funs of
                         Just (TypeVoid, fid', _) ->
                          let stms' = [LLVMStmInstr $ Call TypeVoid fid' llvmArgs]
                          in return ((OI res, TypeVoid), executedSS ++ stms ++ stms')
                         Just (ftyp@(TypeArray len t), fid', _) ->
                          let stms' = let callStm = LLVMStmAssgn call $ Call ftyp fid' llvmArgs
                                          allocaStm  = LLVMStmAssgn alloca $ AllocateAlign ftyp 32
                                          storeStm  = LLVMStmInstr $ Store ftyp (OI call) (TypePtr ftyp) alloca
                                      in [callStm, allocaStm, storeStm]
                          in return ((OI alloca, (TypeArray len t)), executedSS ++ stms ++ stms')
                         Just (ftyp, fid', _) ->
                          let stms' = [LLVMStmAssgn res $ Call ftyp fid' llvmArgs]
                          in return ((OI res, ftyp), executedSS ++ stms ++ stms')
                         Nothing -> fail $ "Function isn't found. " ++ show fid

-- | Translates arguments
execArgs :: [Exp] -> EnvState Env [((Operand, LLVMType), [LLVMStm])]
execArgs = mapM transExp

-- | Creates LLVM arguments
processArgs :: [(Operand, LLVMType)] -> EnvState Env (LLVMArgs, [LLVMStm])
processArgs []   = return (LLVMArgs [], [])
processArgs (ot: ots) =
  let o = fst ot
      t = snd ot
  in do (LLVMArgs largs, stms) <- processArgs ots
        return (LLVMArgs (LLVMArg t o : largs), stms)

-- | Simplifies generation of LLVM instruction
emitOperation :: Instruction -> EnvState Env (Operand, [LLVMStm])
emitOperation instr =
  do
    cnt <- getCounter
    let res = Local $ "t" ++ show cnt
        stm   = LLVMStmAssgn res instr
    return (OI res, [stm])


-- | Translates negation
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


-- | Translates Boolean Not
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
                                      (TypePtr TypeBoolean)
                                      resTmp
         tStm =  LLVMStmInstr $ Store TypeBoolean
                                      (OC ConstTrue)
                                      (TypePtr TypeBoolean)
                                      resTmp
         brStm' = LLVMStmInstr $ UncondBranch $ show l3
         resStm = LLVMStmAssgn res $ Load (TypePtr TypeBoolean) (OI resTmp)
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


-- | Translates a family of increment an decrement functions
transIncr :: Exp -> IncrDecr -> PostPre -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transIncr (EId vid) incrDecr postPre =
  do ((OI ptr, TypePtr typ), expStms) <- transIdPtr vid
     val1 <- genLocal
     val2 <- genLocal
     let s1 = LLVMStmAssgn val1 $ Load (TypePtr typ) (OI ptr)
         i2 = case incrDecr of
           Incr -> Add typ (OI val1) (OC $ ConstInteger 1)
           Decr -> Sub typ (OI val1) (OC $ ConstInteger 1)
         s2 = LLVMStmAssgn val2 i2
         s3 = LLVMStmInstr $ Store typ (OI val2) (TypePtr typ) ptr
         res = case postPre of
           Post -> val1
           Pre  -> val2
     return ((OI res, typ), expStms ++ [s1, s2, s3])



data Ar = ArTimes | ArDiv | ArMod | ArPlus | ArMinus

-- | Translates a family of arithmetics operations
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


-- | Produces a set of LLVM statements facilitating different comparison operations
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
           TypePtr TypeInteger -> ICmp cond t eid1 eid2
           TypePtr TypeBoolean -> ICmp cond t eid1 eid2
           TypePtr TypeDouble  -> FCmp (toOrderedCond cond) t eid1 eid2
           TypeInteger -> ICmp cond t eid1 eid2
           TypeBoolean -> ICmp cond t eid1 eid2
           TypeDouble  -> FCmp (toOrderedCond cond) t eid1 eid2

         condStm = LLVMStmAssgn condId condInstr
         brStm = LLVMStmInstr $ CondBranch (OI condId) (show l1) (show l2)

         tStm =  LLVMStmInstr $ Store TypeBoolean
                                      (OC ConstTrue)
                                      (TypePtr TypeBoolean)
                                      resTmp
         fStm =  LLVMStmInstr $ Store TypeBoolean
                                      (OC ConstFalse)
                                      (TypePtr TypeBoolean)
                                      resTmp
         brStm' = LLVMStmInstr $ UncondBranch $ show l3
         resStm = LLVMStmAssgn res $ Load (TypePtr TypeBoolean) $ OI resTmp
     return ((OI res, TypeBoolean), expStms1 ++ expStms2 ++ [resTmpStm, condStm, brStm, l1Stm, tStm, brStm', l2Stm, fStm, brStm', l3Stm, resStm])



-- | Translates less-than check
transLt :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transLt e1 e2 = emitCmp e1 e2 Slt

-- | Translates greater-than check
transGt :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transGt e1 e2 = emitCmp e1 e2 Sgt

-- | Translates less-than-or-equal check
transLtEq :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transLtEq e1 e2 = emitCmp e1 e2 Sle

-- | Translates greater-than-or-equal check
transGtEq :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transGtEq e1 e2 = emitCmp e1 e2 Sge

-- | Translates equality check
transEq :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transEq e1 e2 = emitCmp e1 e2 Eq

-- | Translates not-equal check
transNEq :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transNEq e1 e2 = emitCmp e1 e2 Ne


data AO = AOOr | AOAnd

-- | Translates and-operation and or-operation
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
         storeTmpStm    = LLVMStmInstr $ Store TypeBoolean (OI tmp) (TypePtr TypeBoolean) ptr
         storeInpStm    = LLVMStmInstr $ Store TypeBoolean val1 (TypePtr TypeBoolean) ptr
         loadResStm   = LLVMStmAssgn res $ Load (TypePtr TypeBoolean) $ OI ptr
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


type IfArr = Bool


-- | Translates assignment of a newly generated array to an array variable
transAss vid e@(ENew typ inbrs) = do
     (inbrs', inbrsStms) <- transInbrs inbrs
     (OI ptr, t) <- extendVarInitArr vid typ inbrs'
     let sArrPtr = LLVMStmAssgn ptr $ AllocateAlign t 1024
     arrStms <- makeArr ptr t
     return $ ((OI ptr, t), inbrsStms ++ [sArrPtr] ++ arrStms)



-- | Translates assignment of any value to the other variable type
transAss vid e =
  do
    (OI ref, TypePtr typ) <- lookupVar vid
    ((val, t), expStms) <- transExp e
    let s = LLVMStmInstr $ Store typ
          val
          (TypePtr typ)
          ref
    return ((val, typ), expStms ++ [s])



-- | Translates assignment of any value to an element of an array
transAssArr :: Exp -> Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transAssArr e1@(EIdArr vid inbrs) e2 =
  do
    (arrOp, arrType)   <- lookupVar vid
    (ots, inbrStms)    <- inbrToPtrs inbrs
    ((val, t), e2Stms) <- transExp e2

    xPtr <- genLocal
    ref  <- genLocal
    let ots' = [OT TypeInteger $ OC $ ConstInteger 0,
                OT TypeInteger $ OC $ ConstInteger 1] ++
               (intersperse (OT TypeInteger $ OC $ ConstInteger 1) ots)
        xPtrGet = LLVMStmAssgn xPtr $ GetElementPtr
          (TypePtr arrType) arrOp ots'
        arrElemType = getElemType arrType $ toInteger $ length inbrs
        valStm = case t of
          TypeArray _ _ -> LLVMStmAssgn ref $ Load (TypePtr t) val
          _             -> LLVMStmEmpty
        retVal = case t of
          TypeArray _ _ -> (OI ref)
          _ -> val
        xStore = LLVMStmInstr $ Store arrElemType retVal (TypePtr arrElemType) xPtr
    return ((retVal, arrElemType), inbrStms ++ e2Stms ++ [valStm, xPtrGet, xStore])



-- | Translates the length method
transLen :: Exp -> EnvState Env ((Operand, LLVMType), [LLVMStm])
transLen e = do ((arrStrPtr, arrStrType), stms') <- transExp e
                lenPtr <- genLocal
                arrLen <- genLocal
                let lenPtrGet = LLVMStmAssgn lenPtr $ GetElementPtr
                      (TypePtr arrStrType) arrStrPtr [OT TypeInteger $ OC $ ConstInteger 0,
                                                      OT TypeInteger $ OC $ ConstInteger 0]
                    arrLenLoad = LLVMStmAssgn arrLen $ Load (TypePtr TypeInteger) $ OI lenPtr
                return ((OI arrLen, TypeInteger), stms' ++ [lenPtrGet, arrLenLoad])


-- | Translates a list of array indexes to a list of pointers
inbrToPtrs :: [InBr] -> EnvState Env ([Operand], [LLVMStm])
inbrToPtrs [] = return ([], [])
inbrToPtrs (InBr e:inbrs) = do
    ((val, _), stms) <- transExp e
    (ots, stms')     <- inbrToPtrs inbrs
    return (OT TypeInteger val : ots, stms ++ stms')




-- | Creates both one-dimensional and multi-dimensional arrays
makeArr :: Identifier -> LLVMType -> EnvState Env [LLVMStm]
makeArr arrPtr arrType@(TypeArray len innerArrType) =
  do
     lenPtr       <- genLocal
     call         <- genLocal
     bitcasted    <- genLocal
     loaded       <- genLocal
     innerArrPtr  <- genLocal

     let sLenPtr = LLVMStmAssgn lenPtr $
           GetElementPtr (TypePtr arrType) (OI arrPtr)
           [OT TypeInteger (OC $ ConstInteger 0),
            OT TypeInteger (OC $ ConstInteger 0)]

         sLenStore = LLVMStmInstr (Store
                                  TypeInteger len
                                  (TypePtr TypeInteger) lenPtr)

         sCall = LLVMStmAssgn call $
           Call (TypePtr TypeInteger) (Global "calloc")
           (LLVMArgs [LLVMArg TypeInteger len, LLVMArg TypeInteger (OC $ ConstInteger 1)])

         sBitcasted = LLVMStmAssgn bitcasted $
           Bitcast (TypePtr TypeInteger) (OI call) (TypePtr innerArrType)

         sLoaded = LLVMStmAssgn loaded $ Load
           (TypePtr innerArrType)
           (OI bitcasted)

         sInnerArrPtr = LLVMStmAssgn innerArrPtr $
           GetElementPtr (TypePtr arrType) (OI arrPtr)
           [OT TypeInteger (OC $ ConstInteger 0),
            OT TypeInteger (OC $ ConstInteger 1)]

         sInnerArrStore = LLVMStmInstr $
           Store innerArrType (OI loaded)
           (TypePtr innerArrType) innerArrPtr

         result = [sLenPtr,
                   sLenStore,
                   sCall,
                   sBitcasted,
                   sLoaded,
                   sInnerArrPtr,
                   sInnerArrStore]
     case innerArrType of
       TypeArrayInner TypeInteger -> return result
       TypeArrayInner t ->
         do stms <- makeInnerArrForeach (OI arrPtr) arrType t len
            return $ result ++ stms
makeArr _ _ = return  []


-- | Creates multiple inner arrays using foreach loop
makeInnerArrForeach :: Operand -> LLVMType -> LLVMType -> Operand -> EnvState Env [LLVMStm]
makeInnerArrForeach outerArrPtrOp arrType innerType len = do
  indPtr <- genLocal
  let indPtrAlloc = LLVMStmAssgn indPtr $ Allocate TypeInteger
      indPtrStore = LLVMStmInstr $
        Store TypeInteger (OC $ ConstInteger 0) (TypePtr TypeInteger) indPtr
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
  let indLoad = LLVMStmAssgn ind $ Load (TypePtr TypeInteger) (OI indPtr)
      checkCmp = LLVMStmAssgn check $ ICmp Slt TypeInteger (OI ind) len
      br = LLVMStmInstr $ CondBranch (OI check) (show l2) (show l3)

  arrPtr <- genLocal
  let sArrPtr = LLVMStmAssgn arrPtr $ AllocateAlign innerType 1024
  innerArrStms <- makeArr arrPtr innerType
  x1 <- genLocal
  x2 <- genLocal
  let sBind = LLVMStmAssgn x1 $
        GetElementPtr (TypePtr arrType) outerArrPtrOp
        [OT TypeInteger (OC $ ConstInteger 0),
         OT TypeInteger (OC $ ConstInteger 1),
         OT TypeInteger (OI ind)]
      sLoad = LLVMStmAssgn x2 $ Load
           (TypePtr innerType)
           (OI arrPtr)
      sStore = LLVMStmInstr $
           Store innerType (OI x2)
           (TypePtr innerType) x1
      bodyStms = [sArrPtr] ++ innerArrStms ++ [sBind, sLoad, sStore]

  indIncr <- genLocal
  let indIncrAdd = LLVMStmAssgn indIncr $ Add TypeInteger (OI ind) (OC $ ConstInteger 1)
      indStore   = LLVMStmInstr $ Store TypeInteger (OI indIncr) (TypePtr TypeInteger) indPtr

  return $ [indPtrAlloc,
            indPtrStore,
            uncond1,
            sl1,
            indLoad,
            checkCmp,
            br,
            sl2] ++
            bodyStms ++
           [indIncrAdd,
            indStore,
            uncond1,
            sl3]



-- | Simplifies calloc-call generation
callocInstr :: Integer -> Instruction
callocInstr x = Call (TypeArrayMult 1) (Global "calloc")
  (LLVMArgs [LLVMArg TypeInteger (OC $ ConstInteger x), LLVMArg TypeInteger (OC $ ConstInteger 1)])



-- | Translates expression from index brackets of an array
transInbrs :: [InBr] -> EnvState Env ([Operand], [LLVMStm])
transInbrs [] = return ([], [])
transInbrs ((InBr e):inbrs) = do ((ind, _), stms) <- transExp e
                                 (inds, stms')    <- transInbrs inbrs
                                 return (ind : inds, stms ++ stms')

