/*
val x7 : SpiralS2.ComplexVector = helper._1
val x8 : SpiralS2.ComplexVector = helper._2
val x226 = {for(lc <- 0 until 1){
 val helper = (lc,x7,x8)

val x11 : Int = helper._1
val x12 : SpiralS2.ComplexVector = helper._2
val x13 : SpiralS2.ComplexVector = helper._3
val x57 = x12(7)
val x106 = x12(10)
val x28 = x12(2)
val x21 = x12(0)
val x134 = x12(14)
val x22 = x12(1)
val x25 = x21 - x22
val x98 = x12(8)
val x49 = x12(5)
val x18 = new ComplexVector(new Array[Complex](8)) //buffer creation
val x128 = x12(13)
val x55 = x12(6)
val x60 = x55 - x57
val x23 = x21 + x22
val x58 = x55 + x57
val x30 = x12(3)
val x33 = x28 - x30
val x31 = x28 + x30
val x20 = new ComplexVector(new Array[Complex](4)) //buffer creation
val x24 = x20.update(0,x23)
val x26 = x24.update(1,x25)
val x32 = x26.update(2,x31)
val x34 = x32.update(3,x33)
val x42 = x34(3)
val x41 = x34(1)
val x45 = x41 - x42
val x35 = x34(0)
val x43 = x41 + x42
val x36 = x34(2)
val x37 = x35 + x36
val x38 = x18.update(0,x37)
val x39 = x35 - x36
val x40 = x38.update(2,x39)
val x44 = x40.update(1,x43)
val x46 = x44.update(3,x45)
val x108 = x12(11)
val x109 = x106 + x108
val x126 = x12(12)
val x129 = x126 + x128
val x130 = x20.update(0,x129)
val x136 = x12(15)
val x137 = x134 + x136
val x139 = x134 - x136
val x131 = x126 - x128
val x132 = x130.update(1,x131)
val x138 = x132.update(2,x137)
val x140 = x138.update(3,x139)
val x142 = x140(2)
val x147 = x140(1)
val x141 = x140(0)
val x148 = x140(3)
val x149 = x147 + x148
val x145 = x141 - x142
val x143 = x141 + x142
val x151 = x147 - x148
val x47 = x12(4)
val x52 = x47 - x49
val x50 = x47 + x49
val x51 = x20.update(0,x50)
val x53 = x51.update(1,x52)
val x59 = x53.update(2,x58)
val x61 = x59.update(3,x60)
val x69 = x61(3)
val x63 = x61(2)
val x16 = new ComplexVector(new Array[Complex](16)) //buffer creation
val x68 = x61(1)
val x70 = x68 + x69
val x72 = x68 - x69
val x62 = x61(0)
val x64 = x62 + x63
val x65 = x46.update(4,x64)
val x66 = x62 - x63
val x111 = x106 - x108
val x100 = x12(9)
val x67 = x65.update(6,x66)
val x101 = x98 + x100
val x103 = x98 - x100
val x71 = x67.update(5,x70)
val x102 = x20.update(0,x101)
val x73 = x71.update(7,x72)
val x93 = x73(7)
val x74 = x73(0)
val x92 = x73(3)
val x96 = x92 - x93
val x86 = x73(2)
val x81 = x73(5)
val x80 = x73(1)
val x84 = x80 - x81
val x87 = x73(6)
val x88 = x86 + x87
val x104 = x102.update(1,x103)
val x110 = x104.update(2,x109)
val x112 = x110.update(3,x111)
val x120 = x112(3)
val x113 = x112(0)
val x114 = x112(2)
val x115 = x113 + x114
val x116 = x18.update(0,x115)
val x117 = x113 - x114
val x118 = x116.update(2,x117)
val x75 = x73(4)
val x78 = x74 - x75
val x76 = x74 + x75
val x77 = x16.update(0,x76)
val x119 = x112(1)
val x121 = x119 + x120
val x123 = x119 - x120
val x82 = x80 + x81
val x79 = x77.update(4,x78)
val x94 = x92 + x93
val x90 = x86 - x87
val x122 = x118.update(1,x121)
val x83 = x79.update(1,x82)
val x124 = x122.update(3,x123)
val x85 = x83.update(5,x84)
val x144 = x124.update(4,x143)
val x89 = x85.update(2,x88)
val x146 = x144.update(6,x145)
val x91 = x89.update(6,x90)
val x150 = x146.update(5,x149)
val x95 = x91.update(3,x94)
val x152 = x150.update(7,x151)
val x153 = x152(0)
val x160 = x152(5)
val x165 = x152(2)
val x97 = x95.update(7,x96)
val x166 = x152(6)
val x169 = x165 - x166
val x159 = x152(1)
val x161 = x159 + x160
val x172 = x152(7)
val x167 = x165 + x166
val x154 = x152(4)
val x157 = x153 - x154
val x155 = x153 + x154
val x171 = x152(3)
val x156 = x97.update(8,x155)
val x173 = x171 + x172
val x175 = x171 - x172
val x158 = x156.update(12,x157)
val x163 = x159 - x160
val x162 = x158.update(9,x161)
val x164 = x162.update(13,x163)
val x168 = x164.update(10,x167)
val x170 = x168.update(14,x169)
val x174 = x170.update(11,x173)
val x176 = x174.update(15,x175)
val x202 = x176(12)
val x184 = x176(9)
val x196 = x176(11)
val x189 = x176(2)
val x201 = x176(4)
val x220 = x176(15)
val x205 = x201 - x202
val x219 = x176(7)
val x221 = x219 + x220
val x208 = x176(13)
val x213 = x176(6)
val x223 = x219 - x220
val x177 = x176(0)
val x203 = x201 + x202
val x207 = x176(5)
val x211 = x207 - x208
val x209 = x207 + x208
val x214 = x176(14)
val x217 = x213 - x214
val x190 = x176(10)
val x193 = x189 - x190
val x191 = x189 + x190
val x183 = x176(1)
val x185 = x183 + x184
val x187 = x183 - x184
val x195 = x176(3)
val x197 = x195 + x196
val x199 = x195 - x196
val x178 = x176(8)
val x215 = x213 + x214
val x179 = x177 + x178
val x181 = x177 - x178
val x180 = x13.update(0,x179)
val x182 = x180.update(8,x181)
val x186 = x182.update(1,x185)
val x188 = x186.update(9,x187)
val x192 = x188.update(2,x191)
val x194 = x192.update(10,x193)
val x198 = x194.update(3,x197)
val x200 = x198.update(11,x199)
val x204 = x200.update(4,x203)
val x206 = x204.update(12,x205)
val x210 = x206.update(5,x209)
val x212 = x210.update(13,x211)
val x216 = x212.update(6,x215)
val x218 = x216.update(14,x217)
val x222 = x218.update(7,x221)
val x224 = x222.update(15,x223)
(x224)
};
x8 }
val x227 = x226

 (x227)
}


////////////////////////////////////////
 val x226 = {
          for (lc <- 0 until 1) {
            val helper = (lc, x7, x8)

            val x11: Int = helper._1
            val x12: SpiralS2.ComplexVector = helper._2
            val x13: SpiralS2.ComplexVector = helper._3
            val x57 = x12(7)
            val x106 = x12(10)
            val x28 = x12(2)
            val x21 = x12(0)
            val x134 = x12(14)
            val x22 = x12(1)
            val x25 = x21 - x22
            val x98 = x12(8)
            val x49 = x12(5)
            val x18 = new ComplexVector(new Array[Complex](8))
            //buffer creation
            val x128 = x12(13)
            val x55 = x12(6)
            val x60 = x55 - x57
            val x23 = x21 + x22
            val x58 = x55 + x57
            val x30 = x12(3)
            val x33 = x28 - x30
            val x31 = x28 + x30
            val x20 = new ComplexVector(new Array[Complex](4))
            //buffer creation
            val x24 = x20.update(0, x23)
            val x26 = x24.update(1, x25)
            val x32 = x26.update(2, x31)
            val x34 = x32.update(3, x33)
            val x42 = x34(3)
            val x41 = x34(1)
            val x45 = x41 - x42
            val x35 = x34(0)
            val x43 = x41 + x42
            val x36 = x34(2)
            val x37 = x35 + x36
            val x38 = x18.update(0, x37)
            val x39 = x35 - x36
            val x40 = x38.update(2, x39)
            val x44 = x40.update(1, x43)
            val x46 = x44.update(3, x45)
            val x108 = x12(11)
            val x109 = x106 + x108
            val x126 = x12(12)
            val x129 = x126 + x128
            val x130 = x20.update(0, x129)
            val x136 = x12(15)
            val x137 = x134 + x136
            val x139 = x134 - x136
            val x131 = x126 - x128
            val x132 = x130.update(1, x131)
            val x138 = x132.update(2, x137)
            val x140 = x138.update(3, x139)
            val x142 = x140(2)
            val x147 = x140(1)
            val x141 = x140(0)
            val x148 = x140(3)
            val x149 = x147 + x148
            val x145 = x141 - x142
            val x143 = x141 + x142
            val x151 = x147 - x148
            val x47 = x12(4)
            val x52 = x47 - x49
            val x50 = x47 + x49
            val x51 = x20.update(0, x50)
            val x53 = x51.update(1, x52)
            val x59 = x53.update(2, x58)
            val x61 = x59.update(3, x60)
            val x69 = x61(3)
            val x63 = x61(2)
            val x16 = new ComplexVector(new Array[Complex](16))
            //buffer creation
            val x68 = x61(1)
            val x70 = x68 + x69
            val x72 = x68 - x69
            val x62 = x61(0)
            val x64 = x62 + x63
            val x65 = x46.update(4, x64)
            val x66 = x62 - x63
            val x111 = x106 - x108
            val x100 = x12(9)
            val x67 = x65.update(6, x66)
            val x101 = x98 + x100
            val x103 = x98 - x100
            val x71 = x67.update(5, x70)
            val x102 = x20.update(0, x101)
            val x73 = x71.update(7, x72)
            val x93 = x73(7)
            val x74 = x73(0)
            val x92 = x73(3)
            val x96 = x92 - x93
            val x86 = x73(2)
            val x81 = x73(5)
            val x80 = x73(1)
            val x84 = x80 - x81
            val x87 = x73(6)
            val x88 = x86 + x87
            val x104 = x102.update(1, x103)
            val x110 = x104.update(2, x109)
            val x112 = x110.update(3, x111)
            val x120 = x112(3)
            val x113 = x112(0)
            val x114 = x112(2)
            val x115 = x113 + x114
            val x116 = x18.update(0, x115)
            val x117 = x113 - x114
            val x118 = x116.update(2, x117)
            val x75 = x73(4)
            val x78 = x74 - x75
            val x76 = x74 + x75
            val x77 = x16.update(0, x76)
            val x119 = x112(1)
            val x121 = x119 + x120
            val x123 = x119 - x120
            val x82 = x80 + x81
            val x79 = x77.update(4, x78)
            val x94 = x92 + x93
            val x90 = x86 - x87
            val x122 = x118.update(1, x121)
            val x83 = x79.update(1, x82)
            val x124 = x122.update(3, x123)
            val x85 = x83.update(5, x84)
            val x144 = x124.update(4, x143)
            val x89 = x85.update(2, x88)
            val x146 = x144.update(6, x145)
            val x91 = x89.update(6, x90)
            val x150 = x146.update(5, x149)
            val x95 = x91.update(3, x94)
            val x152 = x150.update(7, x151)
            val x153 = x152(0)
            val x160 = x152(5)
            val x165 = x152(2)
            val x97 = x95.update(7, x96)
            val x166 = x152(6)
            val x169 = x165 - x166
            val x159 = x152(1)
            val x161 = x159 + x160
            val x172 = x152(7)
            val x167 = x165 + x166
            val x154 = x152(4)
            val x157 = x153 - x154
            val x155 = x153 + x154
            val x171 = x152(3)
            val x156 = x97.update(8, x155)
            val x173 = x171 + x172
            val x175 = x171 - x172
            val x158 = x156.update(12, x157)
            val x163 = x159 - x160
            val x162 = x158.update(9, x161)
            val x164 = x162.update(13, x163)
            val x168 = x164.update(10, x167)
            val x170 = x168.update(14, x169)
            val x174 = x170.update(11, x173)
            val x176 = x174.update(15, x175)
            val x202 = x176(12)
            val x184 = x176(9)
            val x196 = x176(11)
            val x189 = x176(2)
            val x201 = x176(4)
            val x220 = x176(15)
            val x205 = x201 - x202
            val x219 = x176(7)
            val x221 = x219 + x220
            val x208 = x176(13)
            val x213 = x176(6)
            val x223 = x219 - x220
            val x177 = x176(0)
            val x203 = x201 + x202
            val x207 = x176(5)
            val x211 = x207 - x208
            val x209 = x207 + x208
            val x214 = x176(14)
            val x217 = x213 - x214
            val x190 = x176(10)
            val x193 = x189 - x190
            val x191 = x189 + x190
            val x183 = x176(1)
            val x185 = x183 + x184
            val x187 = x183 - x184
            val x195 = x176(3)
            val x197 = x195 + x196
            val x199 = x195 - x196
            val x178 = x176(8)
            val x215 = x213 + x214
            val x179 = x177 + x178
            val x181 = x177 - x178
            val x180 = x13.update(0, x179)
            val x182 = x180.update(8, x181)
            val x186 = x182.update(1, x185)
            val x188 = x186.update(9, x187)
            val x192 = x188.update(2, x191)
            val x194 = x192.update(10, x193)
            val x198 = x194.update(3, x197)
            val x200 = x198.update(11, x199)
            val x204 = x200.update(4, x203)
            val x206 = x204.update(12, x205)
            val x210 = x206.update(5, x209)
            val x212 = x210.update(13, x211)
            val x216 = x212.update(6, x215)
            val x218 = x216.update(14, x217)
            val x222 = x218.update(7, x221)
            val x224 = x222.update(15, x223)
            (x224)
          };
          x8
        }
        val x227 = x226

        (x227)
      }

//bla!
/*****************************************
  Emitting Generated Code
*******************************************/
class testClass  {
def test( helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector))): ((SpiralS2.ComplexVector)) = {
val x1 : SpiralS2.ComplexVector = helper._1
val x2 : SpiralS2.ComplexVector = helper._2
val x14 = x1(2)
val x4 = new ComplexVector(new Array[Complex](4)) //buffer creation
val x16 = x1(3)
val x17 = x14 + x16
val x19 = x14 - x16
val x18 = x4.update(2,x17)
val x20 = x18.update(3,x19)
val x27 = x20(1)
val x28 = x20(3)
val x29 = x27 + x28
val x31 = x27 - x28
val x30 = x2.update(1,x29)
val x32 = x30.update(3,x31)

 (x32)
}


//bla!
/*****************************************
  Emitting Generated Code                  
*******************************************/
class failClass0 extends (((Long,Long))=> ((Long,Long,Long,scala.Function1[(Long,Long,Long),(Long,Long,Long,scala.Function1[(Long),(Long,Long,Long,Long,Long,Long)],scala.Function1[(Long,Long,Long),(Long,Long,Long,Long,Long,Long,Long,Long)],Long,Long,Long,Long,Long,Long,Long,Long)],Long,Long,Long,Long,scala.Function1[(Long),(Long,Long,Long,Long,Long,Long)],scala.Function1[(Long,Long,Long),(Long,Long,Long,Long,Long,Long,Long,Long)],Long,Long,Long,Long,Long,Long,Long,Long,Long))) {
def apply( helper: ((Long,Long))): ((Long,Long,Long,scala.Function1[(Long,Long,Long),(Long,Long,Long,scala.Function1[(Long),(Long,Long,Long,Long,Long,Long)],scala.Function1[(Long,Long,Long),(Long,Long,Long,Long,Long,Long,Long,Long)],Long,Long,Long,Long,Long,Long,Long,Long)],Long,Long,Long,Long,scala.Function1[(Long),(Long,Long,Long,Long,Long,Long)],scala.Function1[(Long,Long,Long),(Long,Long,Long,Long,Long,Long,Long,Long)],Long,Long,Long,Long,Long,Long,Long,Long,Long)) = {
val x0 : Long = helper._1
val x1 : Long = helper._2
val x37 = x0 + x0
val x2 = x1 - x0
val x36: ((Long,Long,Long)) => ((Long,Long,Long,scala.Function1[(Long),(Long,Long,Long,Long,Long,Long)],scala.Function1[(Long,Long,Long),(Long,Long,Long,Long,Long,Long,Long,Long)],Long,Long,Long,Long,Long,Long,Long,Long)) = (helper: ((Long,Long,Long))) =>{
val x3 : Long = helper._1
val x4 : Long = helper._2
val x5 : Long = helper._3
val x21 = x5 - x3
val x20: ((Long,Long,Long)) => ((Long,Long,Long,Long,Long,Long,Long,Long)) = (helper: ((Long,Long,Long))) =>{
val x12 : Long = helper._1
val x13 : Long = helper._2
val x14 : Long = helper._3
val x19 = x13 + x14
val x17 = x13 * x12
val x15 = x13 - x12
val x16 = x12 + x15
val x18 = x16 + x14

 (x12,x13,x14,x15,x16,x17,x18,x19)
}
val x11: ((Long)) => ((Long,Long,Long,Long,Long,Long)) = (helper: ((Long))) =>{
val x6 : Long = helper

val x7 = x6 + x6
val x8 = x7 + x6
val x10 = x8 * x7
val x9 = x8 * x6

 (x6,x7,x7,x8,x9,x10)
}
val x28 =  x11(x4)

val x29 = x28._1//returnarg  false
val x33 = x28._5//returnarg  false
val x32 = x28._4//returnarg  false
val x34 = x28._6//returnarg  true
val x31 = x28._3//returnarg  false
val x30 = x28._2//returnarg  false
val x35 = x32 + x34

 (x3,x4,x5,x11,x20,x21,x29,x30,x31,x32,x33,x34,x35)
}
val x69 =  x36(x37, x0, x2)

val x78 = x69._9//returnarg  false
val x74 = x69._5//returnarg  false
val x70 = x69._1//returnarg  false
val x77 = x69._8//returnarg  false
val x73 = x69._4//returnarg  false
val x71 = x69._2//returnarg  false
val x81 = x69._12//returnarg  false
val x76 = x69._7//returnarg  false
val x80 = x69._11//returnarg  false
val x72 = x69._3//returnarg  false
val x75 = x69._6//returnarg  false
val x82 = x69._13//returnarg  true
val x79 = x69._10//returnarg  false
val x83 = x37 * x76

 (x0,x1,x2,x36,x37,x70,x71,x72,x73,x74,x75,x76,x77,x78,x79,x80,x81,x82,x83)
}}
/*****************************************
  End of Generated Code                  
*******************************************///bla end!
*/
