@str1 = internal constant [9 x i8] c"/* world "
@str0 = internal constant [9 x i8] c"hello */ "
declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()
define i32 @main () {
       %t1 = getelementptr [9 x i8]* @str0, i32 0, i32 0
       call void @printString(i8* %t1)
       %t2 = getelementptr [9 x i8]* @str1, i32 0, i32 0
       call void @printString(i8* %t2)
       %t3 = alloca i32
       store i32 0, i32* %t3
       %tmp4 = load i32* %t3
       ret i32 %tmp4

}

