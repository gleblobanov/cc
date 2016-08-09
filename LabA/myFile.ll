@str1 = internal constant [9 x i8] c"/* world "
@str0 = internal constant [9 x i8] c"hello */ "
declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()
define i32 @main () {
       %t10 = alloca i32
       store i32 10, i32* %t10
       %t12 = call i32 @fac(i32 %t10)
       call void @printInt(i32* %t12)
       %t13 = alloca i32
       store i32 10, i32* %t13
       %t15 = call i32 @rfac(i32 %t13)
       call void @printInt(i32* %t15)
       %t16 = alloca i32
       store i32 10, i32* %t16
       %t18 = call i32 @mfac(i32 %t16)
       call void @printInt(i32* %t18)
       %t19 = alloca i32
       store i32 10, i32* %t19
       %t21 = call i32 @ifac(i32 %t19)
       call void @printInt(i32* %t21)
       %var22 = alloca double
       %t23 = alloca i32
       store i32 10, i32* %t23
       %var25 = alloca i32
       store i32 %t23, i32* %var25
       %t26 = alloca i32
       store i32 1, i32* %t26
       %var28 = alloca i32
       store i32 %t26, i32* %var28
38:
       %t34 = alloca i32
       store i32 0, i32* %t34
       %t33 = alloca i1
       %t32 = icmp sgt i1 %var25, %t34
       br i1%t32, label "label29", label "label30"
"label29":
       store i1 true, i1* %t33
       br label "label31"
"label30":
       store i1 false, i1* %t33
"label31":
       br i1%t33, label 39, label 40
39:
       %t35 = mul i32 %var28, %var25
       store i32 %t35, i32* %var28
       %t36 = alloca i32
       store i32 %var25, i32* %t36
       %t37 = sub i32 %var25, 1
       store i32 %t37, i32* %var25
       br label 38
40:
       call void @printInt(i32* %var28)
       %t42 = alloca double
       store double 10.0, double* %t42
       %t44 = call double @dfac(double %t42)
       call void @printDouble(double* %t44)
       %t45 = getelementptr [9 x i8] @str0, i32 0, i32 0
       call void @printString(i8* %t45)
       %t46 = getelementptr [9 x i8] @str1, i32 0, i32 0
       call void @printString(i8* %t46)
       %t47 = alloca i32
       store i32 0, i32* %t47
       ret i32 %t47

}

define i32 @fac (i32 %a2) {
       %var50 = alloca i32
       %var51 = alloca i32
       %t52 = alloca i32
       store i32 1, i32* %t52
       store i32 %t52, i32* %var50
       store i32 %a2, i32* %var51
62:
       %t58 = alloca i32
       store i32 0, i32* %t58
       %t57 = alloca i1
       %t56 = icmp sgt i1 %var51, %t58
       br i1%t56, label "label53", label "label54"
"label53":
       store i1 true, i1* %t57
       br label "label55"
"label54":
       store i1 false, i1* %t57
"label55":
       br i1%t57, label 63, label 64
63:
       %t59 = mul i32 %var50, %var51
       store i32 %t59, i32* %var50
       %t60 = alloca i32
       store i32 1, i32* %t60
       %t61 = sub i32 %var51, %t60
       store i32 %t61, i32* %var51
       br label 62
64:
       ret i32 %var50

}

define i32 @rfac (i32 %n3) {
       %t73 = alloca i32
       store i32 0, i32* %t73
       %t72 = alloca i1
       %t71 = icmp eq i1 %n3, %t73
       br i1%t71, label "label68", label "label69"
"label68":
       store i1 true, i1* %t72
       br label "label70"
"label69":
       store i1 false, i1* %t72
"label70":
       br i1%t72, label 74, label 75
74:
       %t78 = alloca i32
       store i32 1, i32* %t78
       ret i32 %t78
       br label 76
75:
       %t79 = alloca i32
       store i32 1, i32* %t79
       %t80 = sub i32 %n3, %t79
       %t82 = call i32 @rfac(i32 %t80)
       %t83 = mul i32 %n3, %t82
       ret i32 %t83
76:

}

define i32 @mfac (i32 %n4) {
       %t91 = alloca i32
       store i32 0, i32* %t91
       %t90 = alloca i1
       %t89 = icmp eq i1 %n4, %t91
       br i1%t89, label "label86", label "label87"
"label86":
       store i1 true, i1* %t90
       br label "label88"
"label87":
       store i1 false, i1* %t90
"label88":
       br i1%t90, label 92, label 93
92:
       %t96 = alloca i32
       store i32 1, i32* %t96
       ret i32 %t96
       br label 94
93:
       %t97 = alloca i32
       store i32 1, i32* %t97
       %t98 = sub i32 %n4, %t97
       %t100 = call i32 @nfac(i32 %t98)
       %t101 = mul i32 %n4, %t100
       ret i32 %t101
94:

}

define i32 @nfac (i32 %n5) {
       %t109 = alloca i32
       store i32 0, i32* %t109
       %t108 = alloca i1
       %t107 = icmp ne i1 %n5, %t109
       br i1%t107, label "label104", label "label105"
"label104":
       store i1 true, i1* %t108
       br label "label106"
"label105":
       store i1 false, i1* %t108
"label106":
       br i1%t108, label 110, label 111
110:
       %t114 = alloca i32
       store i32 1, i32* %t114
       %t115 = sub i32 %n5, %t114
       %t117 = call i32 @mfac(i32 %t115)
       %t118 = mul i32 %t117, %n5
       ret i32 %t118
       br label 112
111:
       %t119 = alloca i32
       store i32 1, i32* %t119
       ret i32 %t119
112:

}

define double @dfac (double %n6) {
       %t127 = alloca double
       store double 0.0, double* %t127
       %t126 = alloca i1
       %t125 = icmp eq i1 %n6, %t127
       br i1%t125, label "label122", label "label123"
"label122":
       store i1 true, i1* %t126
       br label "label124"
"label123":
       store i1 false, i1* %t126
"label124":
       br i1%t126, label 128, label 129
128:
       %t132 = alloca double
       store double 1.0, double* %t132
       ret double %t132
       br label 130
129:
       %t133 = alloca double
       store double 1.0, double* %t133
       %t134 = fsub double %n6, %t133
       %t136 = call double @dfac(double %t134)
       %t137 = fmul double %n6, %t136
       ret double %t137
130:

}

define i32 @ifac (i32 %n7) {
       %t140 = alloca i32
       store i32 1, i32* %t140
       %t142 = call i32 @ifac2f(i32 %t140, i32 %n7)
       ret i32 %t142

}

define i32 @ifac2f (i32 %l8, i32 %h8) {
       %t150 = alloca i1
       %t149 = icmp eq i1 %l8, %h8
       br i1%t149, label "label146", label "label147"
"label146":
       store i1 true, i1* %t150
       br label "label148"
"label147":
       store i1 false, i1* %t150
"label148":
       br i1%t150, label 151, label 152
151:
       ret i32 %l8
152:
       %t158 = alloca i1
       %t157 = icmp sgt i1 %l8, %h8
       br i1%t157, label "label154", label "label155"
"label154":
       store i1 true, i1* %t158
       br label "label156"
"label155":
       store i1 false, i1* %t158
"label156":
       br i1%t158, label 159, label 160
159:
       %t162 = alloca i32
       store i32 1, i32* %t162
       ret i32 %t162
160:
       %var163 = alloca i32
       %t164 = add i32 %l8, %h8
       %t165 = alloca i32
       store i32 2, i32* %t165
       %t166 = div i32 %t164, %t165
       store i32 %t166, i32* %var163
       %t168 = call i32 @ifac2f(i32 %l8, i32 %var163)
       %t169 = alloca i32
       store i32 1, i32* %t169
       %t170 = add i32 %var163, %t169
       %t172 = call i32 @ifac2f(i32 %t170, i32 %h8)
       %t173 = mul i32 %t168, %t172
       ret i32 %t173

}

