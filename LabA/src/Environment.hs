module Environment where

import qualified Data.Map as Map
import Data.Map (Map)
--import LLVMTree

import LLVMTypes
import LLVMSyntax
import AbsJL

type Env = ([LLVMLabel], Scope, Funs, GlobalStrings, LLVMType)

type GlobalStrings = [(String, (GlobalId, Int))]

emptyEnv :: Env
emptyEnv = ([], Scope [[]] 0, emptyFuns, [], TypeVoid)

getGlobalStrings :: Env -> GlobalStrings
getGlobalStrings (_, _, _, gs, _) = gs

lookupGS :: String -> EnvState Env (Maybe (GlobalId, Int))
lookupGS str = EnvState (\env -> let gs = getGlobalStrings env
                                 in (lookup str gs, env))

getCounter :: EnvState Env Integer
getCounter = EnvState (\(l, Scope st cnt, f, g, t) -> (cnt+1, (l, Scope st (cnt+1), f, g, t)))

type Storage = [[(Id, (Identifier, LLVMType))]]

data Scope = Scope Storage Integer
emptyScope :: Scope
emptyScope = Scope [] 0
scopeStr (Scope str _) = str
scopeCnt (Scope _ cnt) = cnt

putType :: LLVMType -> EnvState Env ()
putType t = EnvState (\(l, s, f, g, _) -> ((), (l, s, f, g, t)))

getType :: EnvState Env LLVMType
getType = EnvState (\(l, s, f, g, t) -> (t, (l, s, f, g, t)))


type Funs = Map Id FunType
type FunType = (LLVMType, GlobalId, LLVMArgs)
emptyFuns :: Funs
emptyFuns = Map.empty


data EnvState e a = EnvState (e -> (a, e))
instance Monad (EnvState e) where
  return a = EnvState (\e -> (a, e))
  (EnvState m) >>= k = EnvState $ \s_1 -> let (a, s_2) = m s_1
                                              EnvState k_m = k a
                                          in k_m s_2

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



collectFuns :: [Def] -> EnvState Env [Def]
collectFuns = foldr ((>>) . extendFun) (return [])

newBlock :: EnvState Env ()
newBlock = EnvState (\(ls, Scope st n, fs, gs, t) -> ((), (ls, Scope ([]:st) n, fs, gs, t)))


exitBlock :: EnvState Env ()
exitBlock = EnvState (\(ls, Scope st n, fs, gs, t) -> case st of
                         _:scs -> ((), (ls, Scope scs n, fs, gs, t))
                         []    -> ((), (ls, Scope [] n, fs, gs, t)))


newLabel :: LLVMLabel -> EnvState Env LLVMLabel
newLabel l = EnvState (\(ls, sc, fs, gs, t) -> (l, (l:ls, sc, fs, gs, t)))


lookupVar :: Id -> EnvState Env (Maybe (Identifier, LLVMType))
lookupVar vid = EnvState (\(ls, sc, fs, gs, t) -> let str  = scopeStr sc
                                                      res = lookup' vid str
                                                  in (res, (ls, sc, fs, gs, t)))

lookup' :: Eq a => a -> [[(a,b)]] -> Maybe b
lookup' _ []     = Nothing
lookup' a (m:ms) = case lookup a m of
  Just b  -> Just b
  Nothing -> lookup' a ms


lookupFun :: Id -> EnvState Env (Maybe FunType)
lookupFun fid = EnvState (\(ls, sc, fs, gs, t) -> (Map.lookup fid fs, (ls, sc, fs, gs, t)))


extendVar :: Id -> Type -> EnvState Env (Identifier, LLVMType)
extendVar vid t = EnvState (\(ls, s, fs, gs, tt) ->
                             let cnt  = scopeCnt s + 1
                                 vid' = IdentLocal (LocalId ("var" ++ show cnt))
                                 t'   = transType t
                                 str  = scopeStr s
                                 el   = (vid, (vid', t'))
                                 s'   = if null str
                                        then [el] : str
                                        else (el : head str) : tail str
                             in (snd el, (ls, Scope s' cnt, fs, gs, tt)))


extendFun :: Def -> EnvState Env ()
extendFun (DFun t (Id fid) args _) =
    EnvState (\(ls, sc, fs, gs, tt) ->
               let fs'   = Map.insert (Id fid) (t' , fid', args') fs
                   fid'  = GlobalId fid
                   t'    = transType t
                   args' = transArgs args
               in ((), (ls, sc, fs', gs, tt)))

extendArgs :: [Arg] -> EnvState Env ()
extendArgs = foldr ((>>). (\(ADecl t aid ) -> extendVar aid t)) (return ())


transArgs :: [Arg] -> LLVMArgs
transArgs args = LLVMArgs $ map transArg args

transArg :: Arg -> LLVMArg
transArg (ADecl t (Id aid )) = LLVMArg (transType t) (LocalId aid)

transType :: Type -> LLVMType
transType t = case t of
  Type_true       -> TypeBoolean
  Type_false      -> TypeBoolean
  Type_bool_undef -> TypeBoolean
  Type_boolean    -> TypeString
  Type_int        -> TypeInteger
  Type_double     -> TypeDouble
  Type_void       -> TypeVoid
  Type_string     -> TypeString
