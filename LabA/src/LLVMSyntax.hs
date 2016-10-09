module LLVMSyntax where






import Data.List


type Label = String
type Addr = Integer

-- | LLVM types.
data LLVMType = TypeInteger
              | TypeChar
              | TypeBoolean -- Pointer to one bit integer or boolean.
              | TypeFloat
              | TypeDouble
              | TypeVoid
              | TypeString
              | TypeFunction LLVMType [LLVMType]
              | TypeStructure [LLVMType]
              | TypeArray Operand LLVMType
              | TypeArrayInner LLVMType -- Array with 0 length
              | TypeArrayMult Integer
              | TypeArrayInnerLen Integer LLVMType -- Array with a specified length
              | TypePtr LLVMType
              | TypeArrayOfPtr [LLVMType]
              | None


instance Show LLVMType where
  show instr = case instr of
    TypeInteger -> "i32"
    TypeChar    -> "i8"
    TypeBoolean -> "i1"
    TypeFloat   -> "float"
    TypeDouble  -> "double"
    TypeVoid    -> "void"
    TypeString  -> "i8*"
    TypeFunction t ts -> ""
    TypeStructure ts  -> ""
    TypeArray len t  -> "{i32, " ++ show t ++ "}"
    TypeArrayInner t -> "[" ++ (show 0) ++ " x " ++ show t ++ "]"
    TypeArrayMult dem -> makeMultArr dem
    TypeArrayInnerLen len t -> "[" ++ (show len) ++ " x " ++ show t ++ "]"
    TypePtr t -> show t ++ "*"
    TypeArrayOfPtr t     -> ""
    _ -> ""

setArrLen :: LLVMType -> Operand -> LLVMType
setArrLen (TypeArray _ t) len = TypeArray len t

typeFromPtr :: LLVMType -> LLVMType
typeFromPtr (TypePtr t) = t
typeFromPtr t = t

makeMultArr :: Integer -> String
makeMultArr dem | dem == 1  = "[0 x i32]"
                | otherwise = "[0 x ]" ++ (makeMultArr $ dem - 1) ++ "]"



data Operand = OI Identifier | OC Constant | OT LLVMType Operand | ON
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
          | Oeq  -- equal
          | One  -- not equal
          | Ogt -- unsigned greater than
          | Oge -- unsigned greater or equal
          | Olt -- unsigned less than
          | Ole -- unsigned less or equal


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
    Oeq -> "oeq"
    One -> "one"
    Ogt -> "ogt"
    Oge -> "oge"
    Olt -> "olt"
    Ole -> "ole"




toOrderedCond :: Cond -> Cond
toOrderedCond c = case c of
    Eq  -> Oeq
    Ne  -> One
    Ugt -> Ogt
    Uge -> Oge
    Ult -> Olt
    Ule -> Ole
    Sgt -> Ogt
    Sge -> Oge
    Slt -> Olt
    Sle -> Ole



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
                 | AllocateAlign LLVMType Integer
                 | AllocateArr LLVMType Operand
                 | Bitcast LLVMType Operand LLVMType
                 | GetElementPtr LLVMType Operand [Operand]
                 | Store LLVMType Operand LLVMType Identifier
                 | Load  LLVMType Operand

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
      SDiv t o1 o2 -> "sdiv "  ++ show t ++ " " ++ show o1 ++ ", " ++ show o2
      FDiv t o1 o2 -> "fdiv " ++ show t ++ " " ++ show o1 ++ ", " ++ show o2
      SRem t o1 o2 -> "srem " ++ show t ++ " " ++ show o1 ++ ", " ++ show o2
      Sub  t o1 o2 -> "sub "  ++ show t ++ " " ++ show o1 ++ ", " ++ show o2
      FSub t o1 o2 -> "fsub " ++ show t ++ " " ++ show o1 ++ ", " ++ show o2

      And  t o1 o2 -> "and " ++ show t ++ " " ++ show o1 ++ ", " ++ show o2
      Or   t o1 o2 -> "or "  ++ show t ++ " " ++ show o1 ++ ", " ++ show o2

      Allocate t           -> "alloca " ++ show t
      AllocateAlign t a           -> "alloca " ++ show t ++ ", align " ++ show a
      AllocateArr t i      -> "alloca " ++ show t ++ ", i32 " ++ show i
      Bitcast t1 o t2 -> "bitcast " ++ show t1 ++ " " ++ show o ++ " to " ++ show t2
      GetElementPtr t ptr inds ->
        "getelementptr " ++ show t ++ " " ++ show ptr ++ ", " ++
        (intercalate ", " $ map show inds)
      Store t o1 tp o2     ->
        "store " ++ show t ++ " " ++ show o1 ++ ", " ++
        show tp ++ " " ++ show o2
      Load tp o ->
        "load " ++ show tp ++ " " ++ show o
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
        "br i1 " ++
        show o ++ ", " ++
        "label %" ++ l1 ++ ", " ++
        "label %" ++ l2

      UncondBranch l ->
        "br "    ++
        "label %" ++
        l

      ConstInstr const -> show const

      EmptyInstr -> ""
      IdentInstr i -> show i

data LLVMArg = LLVMArg LLVMType Operand
instance Show LLVMArg where
  show (LLVMArg t aid) = typ ++ " " ++ show aid
    where typ = case t of
            (TypeArray _ _) -> show t ++ "*"
            _             -> show t

data LLVMArgs = LLVMArgs [LLVMArg]
instance Show LLVMArgs where
  show (LLVMArgs args) = intercalate ", "  $ map show args


type LLVMTree = [LLVMFunction]

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




