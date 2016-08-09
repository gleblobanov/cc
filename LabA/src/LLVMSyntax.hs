module LLVMSyntax where

import Data.List
import LLVMTypes

data Operand = OI Identifier | OC Constant | OT LLVMType Operand
instance Show Operand where
  show (OI i)   = show i
  show (OC c)   = show c
  show (OT t o) = show t ++ " " ++ show o


data Identifier = Local String
                | Global String
                | EmptyId
instance Show Identifier where
  show (Local  lid) = "%" ++ lid
  show (Global gid) = "@" ++ gid


data Constant = ConstTrue
              | ConstFalse
              | ConstDouble Double
              | ConstInteger Integer
instance Show Constant where
  show ConstTrue  = "true"
  show ConstFalse = "false"
  show (ConstDouble d)   = show d
  show (ConstInteger i)  = show i


data Cond = Eq  -- equal
          | Ne  -- not equal
          | Ugt -- unsigned greater than
          | Uge -- unsigned greater or equal
          | Ult -- unsigned less than
          | Ule -- unsigned less or equal
          | Sgt -- signed greater than
          | Sge -- signed greater or equal
          | Slt -- signed less than
          | Sle -- signed less or equal
instance Show Cond where
  show c = case c of
    Eq  -> "eq"
    Ne  -> "ne"
    Ugt -> "ugt"
    Uge -> "uge"
    Ult -> "ult"
    Ule -> "ule"
    Sgt -> "sgt"
    Sge -> "sge"
    Slt -> "slt"
    Sle -> "sle"



data Instruction = Mul  LLVMType Operand Operand   -- Multiply two integers
                 | FMul LLVMType Operand Operand   -- Multiply two floats
                 | Add  LLVMType Operand Operand
                 | FAdd LLVMType Operand Operand
                 | SDiv LLVMType Operand Operand
                 | FDiv LLVMType Operand Operand
                 | SRem LLVMType Operand Operand
                 | Sub  LLVMType Operand Operand
                 | FSub LLVMType Operand Operand

                 | And LLVMType Operand Operand
                 | Or  LLVMType Operand Operand

                 | Allocate LLVMType
                 | GetElementPtr LLVMTypePtr Operand Operand Operand
                 | Store LLVMType Operand LLVMTypePtr Identifier
                 | Load  LLVMTypePtr Operand

                 | ICmp Cond LLVMType Operand Operand
                 | FCmp Cond LLVMType Operand Operand
                 | Call LLVMType Identifier LLVMArgs

                 | Return LLVMType Operand
                 | ReturnVoid
                 | CondBranch Operand String String
                 | UncondBranch String

                 | EmptyInstr
                 | ConstInstr Constant
                 | IdentInstr Identifier

data LLVMLabel = LLVMLabel String
instance Show LLVMLabel where
  show (LLVMLabel l) = l

lenghLabel :: LLVMLabel -> Int
lenghLabel (LLVMLabel l) = length l

instance Show Instruction where
    show instr = case instr of
      Mul  t o1 o2 -> "mul "  ++ show t ++ " " ++ show o1 ++ ", " ++ show o2
      FMul t o1 o2 -> "fmul " ++ show t ++ " " ++ show o1 ++ ", " ++ show o2
      Add  t o1 o2 -> "add "  ++ show t ++ " " ++ show o1 ++ ", " ++ show o2
      FAdd t o1 o2 -> "fadd " ++ show t ++ " " ++ show o1 ++ ", " ++ show o2
      SDiv t o1 o2 -> "div "  ++ show t ++ " " ++ show o1 ++ ", " ++ show o2
      FDiv t o1 o2 -> "fdiv " ++ show t ++ " " ++ show o1 ++ ", " ++ show o2
      SRem t o1 o2 -> "srem " ++ show t ++ " " ++ show o1 ++ ", " ++ show o2
      Sub  t o1 o2 -> "sub "  ++ show t ++ " " ++ show o1 ++ ", " ++ show o2
      FSub t o1 o2 -> "fsub " ++ show t ++ " " ++ show o1 ++ ", " ++ show o2

      And  t o1 o2 -> "and " ++ show t ++ " " ++ show o1 ++ ", " ++ show o2
      Or   t o1 o2 -> "or "  ++ show t ++ " " ++ show o1 ++ ", " ++ show o2

      Allocate t           -> "alloca " ++ show t
      GetElementPtr t o1 o2 o3 ->
        "getelementptr " ++ show t ++ " " ++ show o1 ++ ", " ++ show o2
                                   ++  ", " ++ show o3
      Store t o1 tp o2     ->
        "store " ++ show t ++ " " ++ show o1 ++ ", " ++
        show tp ++ " " ++ show o2
      Load t o ->
        "load " ++ show t ++ " " ++ show o
      ICmp c t o1 o2 ->
        "icmp " ++ show c ++ " " ++ show t ++ " " ++ show o1 ++ ", " ++ show o2
      FCmp c t o1 o2 ->
        "fcmp " ++ show c ++ " " ++ show t ++ " " ++ show o1 ++ ", " ++ show o2

      Call t fid args ->
        "call " ++
        show t  ++ " " ++
        show fid ++ "(" ++ show args ++ ")"

      Return t o  ->
        "ret " ++
        show t ++ " " ++
        show o

      ReturnVoid -> "ret void\n"

      CondBranch o l1 l2 ->
        "br i1" ++
        show o ++ ", " ++
        "label " ++ l1 ++ ", " ++
        "label " ++ l2

      UncondBranch l ->
        "br "    ++
        "label " ++
        show l

      ConstInstr const -> show const

      EmptyInstr -> ""
      IdentInstr i -> show i

data LLVMArg = LLVMArg LLVMType Identifier
instance Show LLVMArg where
  show (LLVMArg t aid) = show t ++ " " ++ show aid

data LLVMArgs = LLVMArgs [LLVMArg]
instance Show LLVMArgs where
  show (LLVMArgs args) = intercalate ", "  $ map show args

data LLVMFunction = LLVMFunction LLVMType Identifier LLVMArgs [LLVMStm]
instance Show LLVMFunction where
  show (LLVMFunction t fid args stms) =
    "define " ++ show t ++ " " ++ show fid ++ " (" ++ show args ++ ") {\n" ++
    instrsCode ++ "\n}\n\n"
    where instrsCode = concatMap show stms


data LLVMStm = LLVMStmInstr Instruction
             | LLVMStmAssgn Identifier Instruction
             | LLVMStmLabel LLVMLabel
instance Show LLVMStm where
  show (LLVMStmInstr i) =
    shift ++
    show i ++
    "\n"
  show (LLVMStmAssgn lid i) =
    shift ++
    show lid ++
    " = " ++
    show i ++
    "\n"
  show (LLVMStmLabel l) =
    show l ++ ":\n"

shift :: String
shift = "       "
shiftN :: Int -> String
shiftN n = replicate n ' '




