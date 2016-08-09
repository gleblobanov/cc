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
    TypeArray size t  -> "[" ++ show size ++ " x " ++ show t ++ "]"
    TypePtr t -> show t ++ "*"
    TypeArrayOfPtr t     -> ""
    _ -> ""


typeFromPtr :: LLVMType -> LLVMType
typeFromPtr (TypePtr t) = t
typeFromPtr t = t

