module Environment where

import qualified Data.Map as Map
import Data.List
--import LLVMTree

import LLVMTypes
import LLVMSyntax
import AbsJL

type Env = ([LLVMLabel], Scope, Funs, GlobalStrings, LLVMType)

put :: Env -> EnvState Env ()
put env = EnvState (const ((), env))

get :: EnvState Env Env
get = EnvState (\env -> (env, env))

runEnvState :: EnvState Env a -> (a, Env)
runEnvState (EnvState f) = f emptyEnv


instance Functor (EnvState q) where
   fmap f m = m >>= \a -> return (f a)

instance Applicative (EnvState q) where
   pure  = return
   (<*>) a_f a_x = a_f >>= \f -> a_x >>= \x -> pure $ f x



type GlobalStrings = [(String, (Identifier, Integer))]

emptyEnv :: Env
emptyEnv = ([], Scope [[]] 0, emptyFuns, [], TypeVoid)

getGlobalStrings :: Env -> GlobalStrings
getGlobalStrings (_, _, _, gs, _) = gs

-- getFuns :: Env -> Funs
-- getFuns (_, _, funs, _, _) = funs

-- TODO replace everywhere
genLocal :: EnvState Env Identifier
genLocal = do cnt <- getCounter
              return $ Local $ "t" ++ show cnt


lookupGS :: String -> EnvState Env (Maybe (Identifier, Integer))
lookupGS str = EnvState (\env -> let gs = getGlobalStrings env
                                 in (lookup str gs, env))

getCounter :: EnvState Env Integer
getCounter = EnvState (\(l, Scope st cnt, f, g, t) -> (cnt+1, (l, Scope st (cnt+1), f, g, t)))

type Storage = [[(Id, (Operand, LLVMType))]]

data Scope = Scope Storage Integer
     deriving Show
emptyScope :: Scope
emptyScope = Scope [] 0
scopeStr (Scope str _) = str
scopeCnt (Scope _ cnt) = cnt

putType :: LLVMType -> EnvState Env ()
putType t = EnvState (\(l, s, f, g, _) -> ((), (l, s, f, g, t)))

getType :: EnvState Env LLVMType
getType = EnvState (\(l, s, f, g, t) -> (t, (l, s, f, g, t)))

getScope :: EnvState Env Scope
getScope = EnvState (\(l, s, f, g, t) -> (s, (l, s, f, g, t)))

putStorage :: Storage -> EnvState Env ()
putStorage st = EnvState (\(l, Scope _ i, f, g, t) -> ((), (l, Scope st i, f, g, t)))

getFuns :: EnvState Env Funs
getFuns = EnvState (\(l, s, f, g, t) -> (f, (l, s, f, g, t)))

putFuns :: Funs -> EnvState Env ()
putFuns f = EnvState (\(l, s, _, g, t) -> ((), (l, s, f, g, t)))


type Funs = [(Id, FunType)]
type FunType = (LLVMType, Identifier,  LLVMArgs)
emptyFuns :: Funs
emptyFuns = []


data EnvState e a = EnvState (e -> (a, e))
instance Monad (EnvState e) where
  return a = EnvState (\e -> (a, e))
  (EnvState m) >>= k = EnvState $ \s_1 -> let (a, s_2) = m s_1
                                              EnvState k_m = k a
                                          in k_m s_2



collectFuns :: [Def] -> EnvState Env [Def]
collectFuns defs = foldr ((>>) . extendFun) (return defs) defs

newBlock :: EnvState Env ()
newBlock = EnvState (\(ls, Scope st n, fs, gs, t) -> ((), (ls, Scope ([]:st) n, fs, gs, t)))


exitBlock :: EnvState Env ()
exitBlock = EnvState (\(ls, Scope st n, fs, gs, t) -> case st of
                         _:scs -> ((), (ls, Scope scs n, fs, gs, t))
                         []    -> ((), (ls, Scope [] n, fs, gs, t)))


newLabel :: LLVMLabel -> EnvState Env LLVMLabel
newLabel l = EnvState (\(ls, sc, fs, gs, t) -> (l, (l:ls, sc, fs, gs, t)))


lookupVar :: Id -> EnvState Env (Operand, LLVMType)
lookupVar vid = do sc <- getScope
                   env <- get
                   let str = scopeStr sc
                       res = lookup' vid str
                   case res of
                     Just (lid, t) -> return (lid, t)
                     Nothing -> fail $ "Variable isn't found " ++ show vid ++ "\n" ++ show env



lookup' :: Eq a => a -> [[(a,b)]] -> Maybe b
lookup' _ []     = Nothing
lookup' a (m:ms) = case lookup a m of
  Just b  -> Just b
  Nothing -> lookup' a ms


lookupArr :: Id -> EnvState Env (Operand, LLVMType, LLVMType)
lookupArr vid = do (op, t) <- lookupVar vid
                   case t of
                     TypeArray len t' -> return (op, t, t')
                     _ -> fail $ "Can't find an array: " ++ show vid

changeArr :: (Operand, LLVMType) -> Id -> EnvState Env ()
changeArr len vid = do sc <- getScope
                       env <- get
                       let str = scopeStr sc
                           res = change' len vid str
                       putStorage res


change' :: (Operand, LLVMType) -> Id -> [[(Id ,(Operand, LLVMType))]] -> [[(Id, (Operand, LLVMType))]]
change' _ _ []     = []
change' oT vid (m:ms) = case lookup vid m of
  Just _  -> ((vid, oT) : del vid m)  : ms
  _ -> m : change' oT vid ms


del :: Id -> [(Id, a)] -> [(Id, a)]
del _ [] = []
del i ((x, y):ss) | i == x    = ss
                  | otherwise = (x,y) : del i ss

lookupFun :: Id -> EnvState Env (Maybe FunType)
lookupFun fid = EnvState (\(ls, sc, fs, gs, t) -> (lookup fid fs, (ls, sc, fs, gs, t)))


extendVar :: Id -> Operand -> LLVMType -> EnvState Env (Operand, LLVMType)
extendVar vid val t = EnvState (\(ls, s, fs, gs, tt) ->
                             let cnt  = scopeCnt s + 1
                                 str  = scopeStr s
                                 el   = (vid, (val, t))
                                 opr  = (val, t)
                                 s'   = if null str
                                        then [el] : str
                                        else (el : head str) : tail str
                             in (opr, (ls, Scope s' cnt, fs, gs, tt)))


extendVarDecl :: Id -> Type -> EnvState Env (Operand, LLVMType)
extendVarDecl vid t = EnvState (\(ls, s, fs, gs, tt) ->
                             let cnt  = scopeCnt s + 1
                                 vid' = OI $ Local ("var" ++ show cnt)
                                 t'   = TypePtr $ transType t
                                 str  = scopeStr s
                                 el   = (vid, (vid', t'))
                                 s'   = if null str
                                        then [el] : str
                                        else (el : head str) : tail str
                             in (snd el, (ls, Scope s' cnt, fs, gs, tt)))


extendVarDeclArr :: Id -> Type -> [EmptBr] -> EnvState Env (Operand, LLVMType)
extendVarDeclArr vid t brs = EnvState (\(ls, s, fs, gs, tt) ->
                             let cnt  = scopeCnt s + 1
                                 vid' = OI $ Local ("var" ++ show cnt)
                                 t'   = makeArrTypeDecl t brs
                                 str  = scopeStr s
                                 el   = (vid, (vid', t'))
                                 s'   = if null str
                                        then [el] : str
                                        else (el : head str) : tail str
                             in (snd el, (ls, Scope s' cnt, fs, gs, tt)))


extendVarInitArr :: Id -> Type -> [Operand] -> EnvState Env (Operand, LLVMType)
extendVarInitArr vid t inbrs = EnvState (\(ls, s, fs, gs, tt) ->
                             let cnt  = scopeCnt s + 1
                                 vid' = OI $ Local ("var" ++ show cnt)
                                 t'   = makeArrTypeInit t inbrs
                                 str  = scopeStr s
                                 el   = (vid, (vid', t'))
                                 s'   = if null str
                                        then [el] : str
                                        else (el : head str) : tail str
                             in (snd el, (ls, Scope s' cnt, fs, gs, tt)))


makeArrTypeDecl :: Type -> [EmptBr] -> LLVMType
makeArrTypeDecl typ [] = transType typ
makeArrTypeDecl typ (_:brs) =
  TypeArray (OC $ ConstInteger 0) $ TypeArrayInner $ makeArrTypeDecl typ brs




makeArrTypeInit :: Type -> [Operand] -> LLVMType
makeArrTypeInit typ [] = transType typ
makeArrTypeInit typ (inbr:inbrs) =
  TypeArray inbr $ TypeArrayInner $ makeArrTypeInit typ inbrs


extendFun :: Def -> EnvState Env ()
extendFun (DFun t (Id fid) args _) =
  do fs  <- getFuns
     cnt <- getCounter
     let fs'   = (Id fid, (t', fid', args')) : fs
         fid'  = Global fid
         t'    = transTypeFun t
         args' = transArgs args cnt
     putFuns fs'

transArgs :: [Arg] -> Integer -> LLVMArgs
transArgs args cnt = LLVMArgs $ map (transArg cnt) args

transArg :: Integer -> Arg -> LLVMArg
transArg cnt (ADecl t (Id aid )) = LLVMArg (transType t) (OI $ Local (aid ++ show cnt))



-- TODO possible bug
transType :: Type -> LLVMType
transType t = case t of
  Type_true       -> TypeBoolean
  Type_false      -> TypeBoolean
  Type_bool_undef -> TypeBoolean
  Type_boolean    -> TypeBoolean
  Type_int        -> TypeInteger
  Type_double     -> TypeDouble
  Type_void       -> TypeVoid
  Type_string     -> TypeString
  TypeArr t' ebr  ->  makeArrTypeDecl t' ebr


transTypeFun :: Type -> LLVMType
-- transTypeFun t@(TypeArr _ _) = TypePtr $ transType t
transTypeFun t@(TypeArr _ _) = transType t
transTypeFun t = transType t


getElemType :: LLVMType -> Integer -> LLVMType
getElemType arrType level
  | level == 1 = innerElemType
  | otherwise  = getElemType innerElemType (level - 1)
      where innerElemType = getInnerElemType arrType
            getInnerElemType (TypeArray _ (TypeArrayInner t)) = t
