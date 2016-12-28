/*
package apps

class ComplexVector(n: Int)

//bla!
/** ***************************************
  * Emitting Generated Code
  * ******************************************/
class testClass extends (((apps.ComplexVector, apps.ComplexVector, Int, Int, scala.collection.immutable.Vector[Int], Int, scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int])) => ((apps.ComplexVector))) {
  def apply(helper: ((apps.ComplexVector, apps.ComplexVector, Int, Int, scala.collection.immutable.Vector[Int], Int, scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]))): ((apps.ComplexVector)) = {
    val x2: apps.ComplexVector = helper._1
    val x3: apps.ComplexVector = helper._2
    val x1: Int = helper._3
    val x4: Int = helper._4
    val x5: scala.collection.immutable.Vector[Int] = helper._5
    val x6: Int = helper._6
    val x7: scala.collection.immutable.Vector[Int] = helper._7
    val x8: scala.collection.immutable.Vector[Int] = helper._8
    val x9 = x1 == 2 //check for base case
    val x206 = if (x9) {
        val x33 = x12(x2, x3, x1, x4, x5, x6, x7, x8)

        val x34 = x33
        (x34)
      } else {
        val x39 = new ComplexVector(x1) //buffer creation
        val x203 = (0 until 1).foldLeft(x39)(
            (acc, ele) => {
              val helper = (acc, ele)

              val x41: apps.ComplexVector = helper._1
              val x42: Int = helper._2
              val x37 = x1 / 2 //stupid radix choice placeholder
              val x38 = x1 / x37
              val x44 = Vector.empty[Int] //creating vector with x37
              val x52 = x44 :+ 1
              val x53 = x52 :+ x37
              val x193 = x44 :+ x37
              val x45 = x44 :+ x38
              val x46 = x45 :+ 1
              val x57 = Vector(x7.head * x53.head) ++ x7.zipAll(x53, 0, 0).map(p => p._1 + x7.head * p._2)
              val x194 = x193 :+ 1
              val x196 = Vector(x7.head * x194.head) ++ x7.zipAll(x194, 0, 0).map(p => p._1 + x7.head * p._2)
              val x43 = x8 :+ x42
              val x51 = Vector(x5.head * x46.head) ++ x5.zipAll(x46, 0, 0).map(p => p._1 + x5.head * p._2)
              val x195 = Vector(x5.head * x194.head) ++ x5.zipAll(x194, 0, 0).map(p => p._1 + x5.head * p._2)
              val x54 = x7(0)
              val x48 = x5(0)
              val x55 = x54 * 0
              val x49 = x48 * 0
              val x56 = x6 + x55
              val x50 = x4 + x49
              val x191 = x58(x2, x41, x37, x38, x50, x51, x56, x57, x43)

              val x192 = x191
              val x198 = x123(x192, x3, x38, x37, x50, x195, x56, x196, x43)

              val x199 = x198
              (x199)
            })
        val x204 = x203
        (x204)
      }
    val x207 = x206

    (x207)
  }

  /** ***************************************
    * End Main
    * ******************************************/
  def x58: ((apps.ComplexVector, apps.ComplexVector, Int, Int, Int, scala.collection.immutable.Vector[Int], Int, scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int])) => ((apps.ComplexVector)) = (helper: ((apps.ComplexVector, apps.ComplexVector, Int, Int, Int, scala.collection.immutable.Vector[Int], Int, scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]))) => {
    val x61: apps.ComplexVector = helper._1
    val x62: apps.ComplexVector = helper._2
    val x59: Int = helper._3
    val x60: Int = helper._4
    val x63: Int = helper._5
    val x64: scala.collection.immutable.Vector[Int] = helper._6
    val x65: Int = helper._7
    val x66: scala.collection.immutable.Vector[Int] = helper._8
    val x67: scala.collection.immutable.Vector[Int] = helper._9
    val x68 = x59 == 2 //check for base case
    val x188 = if (x68) {
        val x92 = x71(x61, x62, x59, x60, x63, x64, x65, x66, x67)

        val x93 = x92
        (x93)
      } else {
        val x98 = new ComplexVector(x59) //buffer creation
        val x185 = (0 until x60).foldLeft(x98)(
            (acc, ele) => {
              val helper = (acc, ele)

              val x100: apps.ComplexVector = helper._1
              val x101: Int = helper._2
              val x102 = x67 :+ x101
              val x96 = x59 / 2 //stupid radix choice placeholder
              val x97 = x59 / x96
              val x103 = Vector.empty[Int] //creating vector with x96
              val x110 = x103 :+ 1
              val x104 = x103 :+ x97
              val x105 = x104 :+ 1
              val x109 = Vector(x64.head * x105.head) ++ x64.zipAll(x105, 0, 0).map(p => p._1 + x64.head * p._2)
              val x119 = x103 :+ x96
              val x120 = x119 :+ 1
              val x121 = Vector(x64.head * x120.head) ++ x64.zipAll(x120, 0, 0).map(p => p._1 + x64.head * p._2)
              val x106 = x64(0)
              val x112 = x66(0)
              val x113 = x112 * 0
              val x114 = x65 + x113
              val x107 = x106 * 0
              val x108 = x63 + x107
              val x111 = x110 :+ x96
              val x122 = Vector(x66.head * x120.head) ++ x66.zipAll(x120, 0, 0).map(p => p._1 + x66.head * p._2)
              val x115 = Vector(x66.head * x111.head) ++ x66.zipAll(x111, 0, 0).map(p => p._1 + x66.head * p._2)
              val x117 = x58(x61, x100, x96, x97, x108, x109, x114, x115, x102)

              val x118 = x117
              val x180 = x123(x118, x62, x97, x96, x108, x121, x114, x122, x102)

              val x181 = x180
              (x181)
            })
        val x186 = x185
        (x186)
      }
    val x189 = x188

    (x189)
  }

  def x123: ((apps.ComplexVector, apps.ComplexVector, Int, Int, Int, scala.collection.immutable.Vector[Int], Int, scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int])) => ((apps.ComplexVector)) = (helper: ((apps.ComplexVector, apps.ComplexVector, Int, Int, Int, scala.collection.immutable.Vector[Int], Int, scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]))) => {
    val x126: apps.ComplexVector = helper._1
    val x127: apps.ComplexVector = helper._2
    val x124: Int = helper._3
    val x125: Int = helper._4
    val x128: Int = helper._5
    val x129: scala.collection.immutable.Vector[Int] = helper._6
    val x130: Int = helper._7
    val x131: scala.collection.immutable.Vector[Int] = helper._8
    val x132: scala.collection.immutable.Vector[Int] = helper._9
    val x133 = x124 == 2 //check for base case
    val x177 = if (x133) {
        (x138)
      } else {
        val x143 = new ComplexVector(x124) //buffer creation
        val x174 = (0 until x125).foldLeft(x143)(
            (acc, ele) => {
              val helper = (acc, ele)

              val x145: apps.ComplexVector = helper._1
              val x146: Int = helper._2
              val x141 = x124 / 2 //stupid radix choice placeholder
              val x142 = x124 / x141
              val x148 = Vector.empty[Int] //creating vector with x141
              val x164 = x148 :+ x141
              val x149 = x148 :+ x142
              val x165 = x164 :+ 1
              val x166 = Vector(x129.head * x165.head) ++ x129.zipAll(x165, 0, 0).map(p => p._1 + x129.head * p._2)
              val x155 = x148 :+ 1
              val x156 = x155 :+ x141
              val x160 = Vector(x131.head * x156.head) ++ x131.zipAll(x156, 0, 0).map(p => p._1 + x131.head * p._2)
              val x167 = Vector(x131.head * x165.head) ++ x131.zipAll(x165, 0, 0).map(p => p._1 + x131.head * p._2)
              val x150 = x149 :+ 1
              val x154 = Vector(x129.head * x150.head) ++ x129.zipAll(x150, 0, 0).map(p => p._1 + x129.head * p._2)
              val x147 = x132 :+ x146
              val x157 = x131(0)
              val x151 = x129(0)
              val x158 = x157 * 0
              val x152 = x151 * 0
              val x159 = x130 + x158
              val x153 = x128 + x152
              val x162 = x58(x126, x145, x141, x142, x153, x154, x159, x160, x147)

              val x163 = x162
              val x169 = x123(x163, x127, x142, x141, x153, x166, x159, x167, x147)

              val x170 = x169
              (x170)
            })
        val x175 = x174
        (x175)
      }
    val x178 = x177

    (x178)
  }

  def x71: ((apps.ComplexVector, apps.ComplexVector, Int, Int, Int, scala.collection.immutable.Vector[Int], Int, scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int])) => ((apps.ComplexVector)) = (helper: ((apps.ComplexVector, apps.ComplexVector, Int, Int, Int, scala.collection.immutable.Vector[Int], Int, scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]))) => {
    val x74: apps.ComplexVector = helper._1
    val x75: apps.ComplexVector = helper._2
    val x72: Int = helper._3
    val x73: Int = helper._4
    val x76: Int = helper._5
    val x77: scala.collection.immutable.Vector[Int] = helper._6
    val x78: Int = helper._7
    val x79: scala.collection.immutable.Vector[Int] = helper._8
    val x80: scala.collection.immutable.Vector[Int] = helper._9
    val x81 = Vectormult(x76 x77 x76)
    val x83 = Vectormult(x78 x79 x78)
    val x85 = x74(x81)
    val x82 = x81 + 1
    val x86 = x74(x82)
    val x87 = x85 + x86
    val x88 = x85 - x86
    val x89 = x75.update(x83, x87)
    val x90 = x89.update(x83, x88)

    (x90)
  }

  def x12: ((apps.ComplexVector, apps.ComplexVector, Int, Int, scala.collection.immutable.Vector[Int], Int, scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int])) => ((apps.ComplexVector)) = (helper: ((apps.ComplexVector, apps.ComplexVector, Int, Int, scala.collection.immutable.Vector[Int], Int, scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]))) => {
    val x14: apps.ComplexVector = helper._1
    val x15: apps.ComplexVector = helper._2
    val x13: Int = helper._3
    val x16: Int = helper._4
    val x17: scala.collection.immutable.Vector[Int] = helper._5
    val x18: Int = helper._6
    val x19: scala.collection.immutable.Vector[Int] = helper._7
    val x20: scala.collection.immutable.Vector[Int] = helper._8
    val x21 = Vectormult(x16 x17 x16)
    val x24 = Vectormult(x18 x19 x18)
    val x26 = x14(x21)
    val x23 = x21 + 1
    val x27 = x14(x23)
    val x28 = x26 + x27
    val x29 = x26 - x27
    val x30 = x15.update(x24, x28)
    val x31 = x30.update(x24, x29)

    (x31)
  }

  //bla end!

}

*/
