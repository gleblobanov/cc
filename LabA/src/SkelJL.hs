module SkelJL where

-- Haskell module generated by the BNF converter

import AbsJL
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transId :: Id -> Result
transId x = case x of
  Id string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  PDefs defs -> failure x
transDef :: Def -> Result
transDef x = case x of
  DFun type_ id args stms -> failure x
transArg :: Arg -> Result
transArg x = case x of
  ADecl type_ id -> failure x
transStm :: Stm -> Result
transStm x = case x of
  SExp exp -> failure x
  SDecls type_ ids -> failure x
  SInit type_ ids exp -> failure x
  SReturn returnrest -> failure x
  SWhile exp stm -> failure x
  SBlock stms -> failure x
  SForeach type_ id1 id2 stm -> failure x
  SIf exp ifrest -> failure x
transReturnRest :: ReturnRest -> Result
transReturnRest x = case x of
  ReturnRest exp -> failure x
  ReturnRestEmpt -> failure x
transIfRest :: IfRest -> Result
transIfRest x = case x of
  IfR stm ifrestrest -> failure x
  IfRE -> failure x
transIfRestRest :: IfRestRest -> Result
transIfRestRest x = case x of
  IfRREl stm -> failure x
  IfRRE -> failure x
transExp :: Exp -> Result
transExp x = case x of
  ETrue -> failure x
  EFalse -> failure x
  EString string -> failure x
  EInt integer -> failure x
  EDouble double -> failure x
  EId id -> failure x
  EIdArr id inbrs -> failure x
  EApp id exps -> failure x
  ENeg exp -> failure x
  ENot exp -> failure x
  EPostIncr exp -> failure x
  EPostDecr exp -> failure x
  ELength exp -> failure x
  EPreIncr exp -> failure x
  EPreDecr exp -> failure x
  ETimes exp1 exp2 -> failure x
  EDiv exp1 exp2 -> failure x
  EMod exp1 exp2 -> failure x
  EPlus exp1 exp2 -> failure x
  EMinus exp1 exp2 -> failure x
  ELt exp1 exp2 -> failure x
  EGt exp1 exp2 -> failure x
  ELtEq exp1 exp2 -> failure x
  EGtEq exp1 exp2 -> failure x
  EEq exp1 exp2 -> failure x
  ENEq exp1 exp2 -> failure x
  EAnd exp1 exp2 -> failure x
  EOr exp1 exp2 -> failure x
  ENew type_ inbrs -> failure x
  EAss exp1 exp2 -> failure x
transType :: Type -> Result
transType x = case x of
  Type_true -> failure x
  Type_false -> failure x
  Type_int -> failure x
  Type_double -> failure x
  Type_void -> failure x
  Type_string -> failure x
  Type_bool_undef -> failure x
  Type_boolean -> failure x
  TypeArr type_ emptbrs -> failure x
transEmptBr :: EmptBr -> Result
transEmptBr x = case x of
  EmptBr -> failure x
transInBr :: InBr -> Result
transInBr x = case x of
  InBr exp -> failure x

