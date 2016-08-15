declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()
declare i8* @calloc(i32, i32)
define { i32, [100 x i32]} @doubleArray ({ i32, [100 x i32]} %a11) {
entry:
       %t6 = alloca { i32, [100 x i32]}
       store { i32, [100 x i32]} %a11, { i32, [100 x i32]}* %t6
       %t10 = getelementptr { i32, [100 x i32]}* %t6, i32 0, i32 0
       %t11 = load i32* %t10
       %t8 = alloca { i32, [100 x i32]}
       %t9 = getelementptr { i32, [100 x i32]}* %t8, i32 0, i32 0
       store i32 %t11, i32* %t9
       %t12 = alloca i32
       store i32 0, i32* %t12
       br label %lab13
lab13:
       %t16 = load i32* %t12
       %t17 = icmp slt i32 %t16, %t11
       br i1 %t17, label %lab14, label %lab15
lab14:
       %t18 = getelementptr { i32, [100 x i32]}* %t8, i32 0, i32 1, i32 %t16
       store i32 0, i32* %t18
       %t19 = add i32 %t16, 1
       store i32 %t19, i32* %t12
       br label %lab13
lab15:
       %var21 = alloca i32
       store i32 0, i32* %var21
       %t22 = alloca i32
       store i32 0, i32* %t22
       %t23 = getelementptr { i32, [100 x i32]}* %t6, i32 0, i32 0
       %t24 = load i32* %t23
       br label %lab25
lab25:
       %t28 = load i32* %t22
       %t29 = icmp slt i32 %t28, %t24
       br i1 %t29, label %lab26, label %lab27
lab26:
       %t30 = getelementptr { i32, [100 x i32]}* %t6, i32 0, i32 1, i32 %t28
       %t32 = load i32* %var21
       %t33 = load i32* %t30
       %t34 = mul i32 2, %t33
       %t35 = getelementptr { i32, [100 x i32]}* %t8, i32 0, i32 1, i32 %t32
       store i32 %t34, i32* %t35
       %t36 = load i32* %var21
       %t37 = add i32 %t36, 1
       store i32 %t37, i32* %var21
       %t38 = add i32 %t28, 1
       store i32 %t38, i32* %t22
       br label %lab25
lab27:
       %t39 = load { i32, [100 x i32]}* %t8
       ret { i32, [100 x i32]} %t39

}

define void @shiftLeft ({ i32, [100 x i32]} %a2) {
entry:
       %t41 = alloca { i32, [100 x i32]}
       store { i32, [100 x i32]} %a2, { i32, [100 x i32]}* %t41
       %t43 = getelementptr { i32, [100 x i32]}* %t41, i32 0, i32 1, i32 0
       %t44 = load i32* %t43
       %var45 = alloca i32
       store i32 %t44, i32* %var45
       %var46 = alloca i32
       store i32 0, i32* %var46
       br label %lab65
lab65:
       %t53 = load i32* %var46
       %t54 = getelementptr { i32, [100 x i32]}* %t41, i32 0, i32 0
       %t55 = load i32* %t54
       %t56 = sub i32 %t55, 1
       %t51 = alloca i1
       %t50 = icmp slt i32 %t53, %t56
       br i1 %t50, label %lab47, label %lab48
lab47:
       store i1 true, i1* %t51
       br label %lab49
lab48:
       store i1 false, i1* %t51
       br label %lab49
lab49:
       %t52 = load i1* %t51
       br i1 %t52, label %lab66, label %lab67
lab66:
       %t57 = load i32* %var46
       %t58 = load i32* %var46
       %t59 = add i32 %t58, 1
       %t60 = getelementptr { i32, [100 x i32]}* %t41, i32 0, i32 1, i32 %t59
       %t61 = load i32* %t60
       %t62 = getelementptr { i32, [100 x i32]}* %t41, i32 0, i32 1, i32 %t57
       store i32 %t61, i32* %t62
       %t63 = load i32* %var46
       %t64 = add i32 %t63, 1
       store i32 %t64, i32* %var46
       br label %lab65
lab67:
       %t69 = getelementptr { i32, [100 x i32]}* %t41, i32 0, i32 0
       %t70 = load i32* %t69
       %t71 = sub i32 %t70, 1
       %t72 = load i32* %var45
       %t73 = getelementptr { i32, [100 x i32]}* %t41, i32 0, i32 1, i32 %t71
       store i32 %t72, i32* %t73
       ret void


}

define i32 @scalProd ({ i32, [100 x i32]} %a3, { i32, [100 x i32]} %b3) {
entry:
       %t75 = alloca { i32, [100 x i32]}
       store { i32, [100 x i32]} %a3, { i32, [100 x i32]}* %t75
       %t77 = alloca { i32, [100 x i32]}
       store { i32, [100 x i32]} %b3, { i32, [100 x i32]}* %t77
       %var79 = alloca i32
       store i32 0, i32* %var79
       %var80 = alloca i32
       store i32 0, i32* %var80
       br label %lab101
lab101:
       %t87 = load i32* %var80
       %t88 = getelementptr { i32, [100 x i32]}* %t75, i32 0, i32 0
       %t89 = load i32* %t88
       %t85 = alloca i1
       %t84 = icmp slt i32 %t87, %t89
       br i1 %t84, label %lab81, label %lab82
lab81:
       store i1 true, i1* %t85
       br label %lab83
lab82:
       store i1 false, i1* %t85
       br label %lab83
lab83:
       %t86 = load i1* %t85
       br i1 %t86, label %lab102, label %lab103
lab102:
       %t90 = load i32* %var79
       %t91 = load i32* %var80
       %t92 = getelementptr { i32, [100 x i32]}* %t75, i32 0, i32 1, i32 %t91
       %t93 = load i32* %t92
       %t94 = load i32* %var80
       %t95 = getelementptr { i32, [100 x i32]}* %t77, i32 0, i32 1, i32 %t94
       %t96 = load i32* %t95
       %t97 = mul i32 %t93, %t96
       %t98 = add i32 %t90, %t97
       store i32 %t98, i32* %var79
       %t99 = load i32* %var80
       %t100 = add i32 %t99, 1
       store i32 %t100, i32* %var80
       br label %lab101
lab103:
       %t105 = load i32* %var79
       ret i32 %t105

}

define i32 @main () {
entry:
       %t107 = alloca { i32, [100 x i32]}
       %t108 = getelementptr { i32, [100 x i32]}* %t107, i32 0, i32 0
       store i32 5, i32* %t108
       %t109 = alloca i32
       store i32 0, i32* %t109
       br label %lab110
lab110:
       %t113 = load i32* %t109
       %t114 = icmp slt i32 %t113, 5
       br i1 %t114, label %lab111, label %lab112
lab111:
       %t115 = getelementptr { i32, [100 x i32]}* %t107, i32 0, i32 1, i32 %t113
       store i32 0, i32* %t115
       %t116 = add i32 %t113, 1
       store i32 %t116, i32* %t109
       br label %lab110
lab112:
       %var118 = alloca i32
       store i32 0, i32* %var118
       br label %lab133
lab133:
       %t125 = load i32* %var118
       %t126 = getelementptr { i32, [100 x i32]}* %t107, i32 0, i32 0
       %t127 = load i32* %t126
       %t123 = alloca i1
       %t122 = icmp slt i32 %t125, %t127
       br i1 %t122, label %lab119, label %lab120
lab119:
       store i1 true, i1* %t123
       br label %lab121
lab120:
       store i1 false, i1* %t123
       br label %lab121
lab121:
       %t124 = load i1* %t123
       br i1 %t124, label %lab134, label %lab135
lab134:
       %t128 = load i32* %var118
       %t129 = load i32* %var118
       %t130 = getelementptr { i32, [100 x i32]}* %t107, i32 0, i32 1, i32 %t128
       store i32 %t129, i32* %t130
       %t131 = load i32* %var118
       %t132 = add i32 %t131, 1
       store i32 %t132, i32* %var118
       br label %lab133
lab135:
       %t139 = load { i32, [100 x i32]}* %t107
       call void @shiftLeft({ i32, [100 x i32]} %t139)
       %t142 = load { i32, [100 x i32]}* %t107
       %t140 = alloca { i32, [100 x i32]}
       %t141 = call { i32, [100 x i32]} @doubleArray({ i32, [100 x i32]} %t142)
       store { i32, [100 x i32]} %t141, { i32, [100 x i32]}* %t140
       %t144 = alloca i32
       store i32 0, i32* %t144
       %t145 = getelementptr { i32, [100 x i32]}* %t107, i32 0, i32 0
       %t146 = load i32* %t145
       br label %lab147
lab147:
       %t150 = load i32* %t144
       %t151 = icmp slt i32 %t150, %t146
       br i1 %t151, label %lab148, label %lab149
lab148:
       %t152 = getelementptr { i32, [100 x i32]}* %t107, i32 0, i32 1, i32 %t150
       %t154 = load i32* %t152
       call void @printInt(i32 %t154)
       %t155 = add i32 %t150, 1
       store i32 %t155, i32* %t144
       br label %lab147
lab149:
       %t156 = alloca i32
       store i32 0, i32* %t156
       %t157 = getelementptr { i32, [100 x i32]}* %t140, i32 0, i32 0
       %t158 = load i32* %t157
       br label %lab159
lab159:
       %t162 = load i32* %t156
       %t163 = icmp slt i32 %t162, %t158
       br i1 %t163, label %lab160, label %lab161
lab160:
       %t164 = getelementptr { i32, [100 x i32]}* %t140, i32 0, i32 1, i32 %t162
       %t166 = load i32* %t164
       call void @printInt(i32 %t166)
       %t167 = add i32 %t162, 1
       store i32 %t167, i32* %t156
       br label %lab159
lab161:
       %t170 = load { i32, [100 x i32]}* %t107
       %t171 = load { i32, [100 x i32]}* %t140
       %t168 = call i32 @scalProd({ i32, [100 x i32]} %t170, { i32, [100 x i32]} %t171)
       call void @printInt(i32 %t168)
       ret i32 0

}

