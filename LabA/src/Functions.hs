module Functions where


-- compileFun :: Env -> Def -> Code
-- compileFun env (DFun _ (Id "main" ) _ stms) =
--     ".method public static main()I\n\n"
--     ++ "   .limit locals " ++ show $ getAddr env' ++ "\n"
--     ++ "   .limit stack " ++ show $ getStack env' ++ "\n"
--     ++ addReturn code Type_int
--     ++ ".end method\n\n"
--         where (code, env') = compileStms env stms


-- compileFun env (DFun t (Id id) args stms) =
--     ".method public static " ++ id ++ "(" ++ targs ++ ")" ++ t' ++"\n"
--     ++ "   .limit locals " ++ show $ getAddr env'' ++ "\n"
--     ++ "   .limit stack " ++ show $ getStack env'' ++ "\n"
--     ++ addReturn code t
--     ++ ".end method\n\n"
--         where (code, env'') = compileStms env' stms
--               t'     = typeToLetter t
--               targs  = argsToString args
--               env'   = extendArgs env args


declPrintRead :: String
declPrintRead = "declare void @printInt(i32)\n\
                \declare void @printDouble(double)\n\
                \declare void @printString(i8*)\n\
                \declare i32 @readInt()\n\
                \declare double @readDouble()\n"

