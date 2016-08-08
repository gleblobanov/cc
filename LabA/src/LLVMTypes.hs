module LLVMTypes where

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
              | TypeArray Int LLVMType
              | TypePtr LLVMTypePtr

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
    TypeArray size t  -> "[" ++ show size ++ " x " ++ show t ++ "]"
    TypePtr t -> show t

data LLVMTypePtr = TypeIntegerPtr
                 | TypeCharPtr
                 | TypeBooleanPtr
                 | TypeFloatPtr
                 | TypeDoublePtr
                 | TypeFunctionPtr LLVMType [LLVMType]
                 | TypeStructurePtr [LLVMType]
                 | TypeArrayPtr Int LLVMType
                 | TypeArrayOfPtr [LLVMTypePtr]
                 | NonePtr

instance Show LLVMTypePtr where
  show instr = case instr of
    TypeIntegerPtr -> "i32*"
    TypeCharPtr    -> "i8*"
    TypeBooleanPtr -> "i1*"
    TypeFloatPtr   -> "float*"
    TypeDoublePtr  -> "double*"
    TypeFunctionPtr t ts -> ""
    TypeStructurePtr ts  -> ""
    TypeArrayPtr t l     -> show (TypeArray t l) ++ "*"
    TypeArrayOfPtr t     -> ""
    _ -> ""


typeToPtr :: LLVMType -> LLVMTypePtr
typeToPtr TypeInteger = TypeIntegerPtr
typeToPtr TypeChar    = TypeCharPtr
typeToPtr TypeBoolean = TypeBooleanPtr
typeToPtr TypeFloat = TypeFloatPtr
typeToPtr TypeDouble = TypeDoublePtr
typeToPtr (TypeFunction t ts) = TypeFunctionPtr t ts
typeToPtr (TypeStructure ts) = TypeStructurePtr ts
typeToPtr (TypeArray t l) = TypeArrayPtr t l
typeToPtr _ = NonePtr

typeFromPtr :: LLVMTypePtr -> LLVMType
typeFromPtr TypeIntegerPtr = TypeInteger
typeFromPtr TypeCharPtr    = TypeChar
typeFromPtr TypeBooleanPtr = TypeBoolean
typeFromPtr TypeFloatPtr = TypeFloat
typeFromPtr TypeDoublePtr = TypeDouble
typeFromPtr (TypeFunctionPtr t ts) = TypeFunction t ts
typeFromPtr (TypeStructurePtr ts) = TypeStructure ts
typeFromPtr (TypeArrayPtr t l) = TypeArray t l
typeFromPtr _ = TypeVoid





