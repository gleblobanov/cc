module TypeChecker where

import AbsJL
import PrintJL
import ErrM
import Control.Monad
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map

-- | The environment. Contains function signatures and context stack
type Env = (Sig, [Context])

-- | Function type signature
type Sig = Map Id ([Type], Type)

-- | Mapping of variables with their types
type Context = Map Id Type

-- | If the main function has a return statement as a direct child.
type HasReturn   = Bool
type CheckReturn = Bool

-- | Looks up a variable in the environment
lookupVar :: Env -> Id -> Err Type
lookupVar env vid = let srch = filter (/= Nothing)
                               (map (Map.lookup vid) (snd env)) in
                       case srch  of
                             [] -> fail $ "variable name not declared in current block: " ++ show vid
                             _  -> return $ fromJust $ head srch


-- | Looks up a function and checks if it has a proper amount of arguments which
-- should also be of a correct type
lookupFun :: Env -> Id -> [Exp] -> Err ([Type], Type)
lookupFun env fid exps = 
    case Map.lookup fid (fst env) of
        Just funtyp -> if (length (fst funtyp) /= length exps)
                       then fail "Number of arguments"
                       else do let a = zip (fst funtyp) (map (inferExp env) exps)
                                   b = map (\(x,y) -> do
                                               t <- y
                                               return $ if isBool x
                                                        then if isBool t
                                                             then True
                                                             else False
                                                        else t == x) a
                               c <- foldM (\x y -> do y'<-y
                                                      return $ x && y') True b
                               if c
                               then return funtyp
                               else fail $ "Wrong type of some arguments." ++ (show fid) ++ show exps ++ show (fst funtyp)
        Nothing -> fail $ "function name not declared: " ++ show fid ++ "-------------"

-- | Updates a variable record in the environment.
updateVar :: Env -> Id -> Type -> Err Env
updateVar (sig, topctx : restctxs) v typ =
    if Map.member v topctx then
        fail "variable already declared in current block"
        else
            return (sig, Map.insert v typ topctx : restctxs)

-- | Updates some records of some variable in the environment.
updateVars :: Env -> [Id] -> Type -> Err Env
updateVars env [] _ = return env
updateVars env (id:ids) t = do env' <- updateVar env id t
                               updateVars env' ids t

-- | Updates a function record in the environment.
updateFun :: Env -> Id -> ([Type],Type) -> Err Env
updateFun (sig, ctxs) f typ = 
    if Map.member f sig then
        fail "function already declared"
        else
                return (Map.insert f typ sig, ctxs) 

-- | Creates a new block in the environment.
newBlock :: Env -> Env
newBlock (sig, ctxs) = (sig, emptyContext:ctxs)

-- | Returns an empty environment.
emptyEnv :: Env
emptyEnv = (emptySig, [emptyContext])

-- | Returns an empty function signature.
emptySig :: Sig
emptySig = Map.empty

-- | Returns an empty context.
emptyContext :: Context
emptyContext = Map.empty

-- | Checks if a function is the main function. 
isMain :: Def -> Bool
isMain (DFun Type_int (Id "main") [] _) = True
isMain _ = False

-- | Checks if all statements of the program have correct types.
typecheck :: Program -> Err ()
typecheck (PDefs defs) =
  let ms = filter isMain defs
  in if length ms /= 1
     then fail "No main"
     else do
          builtenv <- foldM
             (\env (id, typs) -> updateFun env id typs)
             emptyEnv
             [
              (Id "printInt", ([Type_int], Type_void)),
              (Id "printDouble", ([Type_double], Type_void)),
              (Id "printString", ([Type_string], Type_void)),
              (Id "readInt", ([], Type_int))
             ]

          globalenv <- foldM
             (\env (DFun typ id args stms) -> updateFun env id (map (\(ADecl t _) -> t) args, typ))
             builtenv
             defs



          mapM
                (\(DFun outtyp id args stms)  ->
                   let
                        retType = case outtyp of
                                Type_void -> Nothing
                                _ -> Just outtyp
                        in do

                           fbdyenv <-
                                   foldM
                                        (\env (ADecl atyp aid) ->
                                              updateVar env aid atyp
                                         )
                                         (newBlock globalenv)
                                         args

                           hr <- typecheckStms fbdyenv retType stms $ if (isJust retType)
                                                                then  checkRet stms
                                                                else  False
                           if outtyp /= Type_void && not hr
                           then fail "No return."
                           else return ()
                )
                defs
          return ()

-- | Checks if a list of statements contains a return statement.
checkRet :: [Stm] -> Bool
checkRet stms = not $ any isReturn stms

-- | Checks if a statements is a return statement.
isReturn :: Stm -> Bool
isReturn (SReturn _) = True
isReturn _           = False

-- | Checks if some statements have correct types.
typecheckStms :: Env -> Maybe Type -> [Stm] -> CheckReturn -> Err HasReturn
typecheckStms _   _          []         _ = return False
-- Verify if while is not the last block in the main
-- maybe I should pass info about cR that I get after every typecheck
-- and not only after the first one.
typecheckStms env typ [SWhile exp stm] cR@True =
  if exp /= ETrue || exp /= EFalse
  then fail "There is no explicit return statement in the main function."
  else if exp == ETrue
       then do r <- typecheckExp env Type_boolean exp
               typecheckStms (newBlock env) typ [stm] cR
       else fail "There is no explicit return statement in the main function."
typecheckStms env typ (stm:stms) cR = case stm of
              SExp exp -> do
                   inferExp env exp
                   typecheckStms env typ stms cR
              SDecls type' ids -> do
                     res_env <- foldM
                            (\env' id -> updateVar env' id type')
                            env
                            ids
                     typecheckStms res_env typ stms cR

              SWhile exp stm' -> do
                     r <- typecheckExp env Type_boolean exp
                     r1 <- typecheckStms (newBlock env) typ [stm'] cR
                     r2 <- typecheckStms env typ stms cR
                     return $ r1 || r2

              SReturn rest -> do r1 <- typecheckReturn env typ rest
                                 r2 <- typecheckStms env typ stms cR
                                 return $ r1 || r2
              SInit type' ids exp -> do
                    t <- typecheckExp env type' exp
                    if isBool type'
                    then if isBool t
                         then do
                                resEnv <- updateVars env ids type'
                                typecheckStms resEnv typ stms cR
                         else fail $ "SInit error " ++ show ids
                    else if (t == type') then
                       do
                                resEnv <- updateVars env ids type'
                                typecheckStms resEnv typ stms cR
                    else
                       fail $ "SInit error " ++ show ids


              SBlock s -> do
                     r1 <- typecheckStms (newBlock env) typ s cR
                     r2 <- typecheckStms env typ stms cR
                     return $ r1 || r2
              SIf exp rest -> do
                      r1 <- typecheckIf env typ stms exp rest cR
                      r2 <- typecheckStms env typ stms cR
                      return $ r1 || r2

-- | Checks if if-else statement is of a correct type.
typecheckIf :: Env -> Maybe Type -> [Stm] ->
               Exp -> IfRest -> CheckReturn -> Err HasReturn
typecheckIf env mt stms exp rest cR = do
  res <- typecheckExp env Type_boolean exp;
  case rest of
    IfR stm rest' ->
      do r <- typecheckStms (newBlock env) mt [stm] cR
         let hR = r || case stm of
               SBlock stms -> (not $ checkRet stms)
               SReturn _   -> True
               _           -> False
         case rest' of
           IfRREl stm' -> do r <- typecheckStms (newBlock env) mt [stm'] cR
                             let hR' = r || case stm' of
                                  SBlock stms -> (not $ checkRet stms)
                                  SReturn _   -> True
                                  _           -> False
                             if cR
                             then if res == Type_true 
                                  then if hR
                                       then return hR
                                       else fail "No return."
                                  else if res == Type_false
                                       then return hR'
                                       else return $ hR && hR'
                             else return $ hR && hR'
           IfRRE -> if cR
                    then if res `elem` [Type_true]-- || res == Type_boolean
                         then if hR
                              then return hR
                              else fail "No return."
                         else fail "No return."
                    else return hR
    IfRE       -> return False

-- | Checks if a return statements is of a correct type.
typecheckReturn :: Env -> Maybe Type ->
                   ReturnRest -> Err HasReturn
typecheckReturn env Nothing rest = case rest of
  ReturnRest exp -> fail "Returns different type"
  ReturnRestEmpt -> return True

typecheckReturn env (Just typ) rest = case rest of
  ReturnRest exp -> do t <- typecheckExp env typ exp
                       if isBool typ
                       then if isBool t
                            then return True
                            else fail "Returns different type"
                       else if (t /= typ)
                            then fail "Returns different type"
                            else return True
  ReturnRestEmpt -> if typ == Type_void then return True
                    else fail "Returns different type"


-- | Checks if an expression is of a given type.
typecheckExp :: Env -> Type -> Exp -> Err Type
typecheckExp env typ exp = do
             typ2 <- inferExp env exp
             if isBool typ
             then if isBool typ2
                  then return typ2
                  else fail $ "type of " ++ printTree exp
             else if typ2 == typ
                  then return typ2
                  else fail $ "type of " ++ printTree exp

isBool :: Type -> Bool
isBool = flip elem [Type_true, Type_false, Type_boolean]

inferExp :: Env -> Exp -> Err Type
inferExp env x = case x of

         ETrue   -> return Type_true
         EFalse  -> return Type_false
         EInt n  -> return Type_int
         EDouble d -> return Type_double
         EString _ -> return Type_string

         EId id  -> lookupVar env id
         EApp id exps -> inferApp env id exps
         ENeg exp -> do t <- inferExp env exp
                        if t == Type_int || t == Type_double
                        then return t
                        else fail "An argument of negation\
                                  \has a different type"
         ENot exp -> do t <- inferExp env exp
                        if isBool t
                        then return t
                        else fail "An argument of negation\
                                  \has a different type"
         EPostIncr a -> inferIncr env a
         EPostDecr a -> inferIncr env a
         EPreIncr a -> inferIncr env a
         EPreDecr a -> inferIncr env a
         ETimes exp0 exp -> inferArithmBin env exp0 exp
         EDiv exp0 exp -> inferArithmBin env exp0 exp
         EMod e1 e2 -> do t1 <- inferExp env e1
                          t2 <- inferExp env e2
                          if t1 == Type_int && t2 == Type_int
                          then return Type_int
                          else fail "An argument of mod\
                                  \has a different type, not int."
         EPlus exp0 exp -> inferArithmBin env exp0 exp
         EMinus exp0 exp -> inferArithmBin env exp0 exp
         ELt exp0 exp -> inferBoolBin env exp0 exp
         EGt exp0 exp -> inferBoolBin env exp0 exp
         ELtEq exp0 exp -> inferBoolBin env exp0 exp
         EGtEq exp0 exp -> inferBoolBin env exp0 exp
         EEq exp0 exp -> inferBoolBin env exp0 exp
         ENEq exp0 exp -> inferBoolBin env exp0 exp
         EAnd exp0 exp -> inferBoolBin env exp0 exp
         EOr exp0 exp -> inferBoolBin env exp0 exp
         EAss a b -> do
              typa <- inferExp env a
              typb <- inferExp env b
              if (typa == typb) then
                 return typa
              else
                 fail $ "type of " ++ printTree a
         _ -> fail $ "Unknown expression" ++ show x

inferArithmBin :: Env -> Exp -> Exp -> Err Type
inferArithmBin env a b = do
    typ <- inferExp env a
    if elem typ [Type_int, Type_double] then do
        typecheckExp env typ b
      else
        fail $ "type of expression " ++ printTree a

inferBoolBin :: Env -> Exp -> Exp -> Err Type
inferBoolBin env a b = do
    typa <- inferExp env a
    typb <- inferExp env b
    if isBool typa
    then if isBool typb
         then return Type_boolean
         else fail $ "type of expression " ++ printTree a
    else if isBool typb
         then if isBool typa
              then return Type_boolean
              else fail $ "type of expression " ++ printTree a
         else if (typa == typb) 
              then return Type_boolean
              else fail $ "type of expression " ++ printTree a

inferApp :: Env -> Id -> [Exp] -> Err Type
inferApp env id exps = do
         tapp <- lookupFun env id exps
         return $ snd tapp

inferIncr :: Env -> Exp -> Err Type
inferIncr env a = do
          type' <- inferExp env a
          if (isBool type') then
             fail $ "type of expression " ++ printTree a
          else
             return type'
