package SpiralS2

import scala.annotation.tailrec
import scala.util.Random

object Settings {
  val validate = false//true
  val WHT = true
  var sanitycheck: Int = 0
  val n: Option[Int] = None
}


object Twiddle {
  var TMap = Map.empty[(Int, Int, Int), ComplexVector]

  object MathUtilities {

    def dLin(N: Int, a: Double, b: Double): List[Double] = {
      val t_array = new Array[Double](N)
      for (i <- 0 until N)
        t_array(i) = a * i + b
      t_array.toList
    }

    def diagTensor(a: List[Double], b: List[Double]): List[Double] = {
      val t_array = new Array[Double](a.size * b.size)
      for (i <- 0 until a.size)
        for (j <- 0 until b.size)
          t_array(i * b.size + j) = a(i) * b(j)
      t_array.toList
    }
  }

  def apply(x: ComplexVector, n: Int, d: Int, k: Int): ComplexVector = {
    val diag = MathUtilities.diagTensor(MathUtilities.dLin(n / d, 1, 0), MathUtilities.dLin(d, 1, 0))
    val t = E(n)
    val root_list_re = diag map (ele => t.re(ele.toInt * k))
    val root_list_im = diag map (ele => t.im(ele.toInt * k))

    for (i <- 0 until root_list_re.size) {
      val u = Complex(root_list_re(i), root_list_im(i))
      //val idx = vrep(yi)
      val tx = x.apply(i)
      x.update(i, tx * u)
    }
    x
  }

  def apply(n: Int, d: Int, k: Int, i: Int): Complex = {

    if (!TMap.contains((n, d, k))) {
      val diag = MathUtilities.diagTensor(MathUtilities.dLin(n / d, 1, 0), MathUtilities.dLin(d, 1, 0))
      val t = E(n)
      val root_list_re = diag map (ele => t.re(ele.toInt * k))
      val root_list_im = diag map (ele => t.im(ele.toInt * k))

      val cv = new ComplexVector(new Array[Complex](root_list_re.size))
      for (i <- 0 until root_list_re.size) {
        val u = Complex(root_list_re(i), root_list_im(i))
        cv.update(i, u)
      }
      TMap = TMap + ((n, d, k) -> cv)
    }
    val cv = TMap.get((n, d, k))
    cv.get(i)
  }


  def DFT(n: Int): Vector[ComplexVector] = {
    val m = new Array[ComplexVector](n)
    val k = 1
    val t_e = E(n)
    for (x <- 0 until n)
      m(x) = new ComplexVector(new Array[Complex](n))
    for (x <- 0 until n)
      for (y <- 0 until n) {

        m(x).update(y, new Complex(t_e.re(x * y * k), t_e.im(x * y * k)))
      }
    m.toVector
  }


  def WHT(n: Int, x: Int, y: Int): Complex = {
    //1* 1*
    //1* -1*


    val t = if (n == 1) new Complex(1, 0) /*{
      if (rx == 1 && ry == 1) new Complex(-1, 0) else
    }*/
    else {
      val nx = x % (n / 2)
      val ny = y % (n / 2)
      if (x >= n / 2 && y >= n / 2)
        Complex(-1, 0) * WHT(n / 2, nx, ny)
      else
        WHT(n / 2, nx, ny)
    }
    t

  }

  //this is the version that returns a single complex
  def DFT(n: Int, x: Int, y: Int): Complex = {
    val k = 1
    val t_e = E(n)
    new Complex(t_e.re(x * y * k), t_e.im(x * y * k))
  }


}


object E {
  var EMap = Map.empty[Int, E]

  def apply(n: Int): E = {
    val eo = EMap.get(n)
    eo.getOrElse({
      val ne = new E(n)
      EMap = EMap + (n -> ne)
      ne
    })
  }
}

class E(val n: Int) {
  def Gcd[A](x: A, y: A)(implicit integral: Integral[A]): A = {
    val t = scala.math.BigInt(integral.toLong(x))
    val res = t.gcd(scala.math.BigInt(integral.toLong(y)))
    x match {
      case _: Int => res.toInt.asInstanceOf[A]
      case _: Long => res.toLong.asInstanceOf[A]
      case _: Short => res.toShort.asInstanceOf[A]
    }
  }

  def NormalizeRational[A](x: A, y: A)(implicit integral: Integral[A]): (A, A) = {
    val gcd = Gcd(x, y)
    (integral.quot(x, gcd), integral.quot(y, gcd))
  }

  def normalize_2pi_shift(xin: Double, yin: Double): (Double, Double) = {
    var (x, y) = NormalizeRational(Math.round(xin), Math.round(yin))
    if ((x / y) < 0) {
      val t: Long = Math.ceil(x.toDouble / y.toDouble / (-2.0)).toLong
      x = x + 2 * t * y
    } else {
      val t = (Math.floor((x.toDouble - 2 * y.toDouble) / y.toDouble / 2.0) + 1).toLong;
      x = x - 2 * y * t;
    }
    val (xp, yp) = NormalizeRational(x, y)
    (xp.toDouble, yp.toDouble)
  }

  def normalize_pi_over2_shift(xin: Double, yin: Double): (Double, Double) = {
    val (x, y) = (Math.round(xin), Math.round(yin))
    val (xp, yp) = NormalizeRational(2 * x - y, 2 * y)
    (xp.toDouble, yp.toDouble)
  }

  def normalize_pi_over2_reflection(xin: Double, yin: Double): (Double, Double) = {
    val (x, y) = (Math.round(xin), Math.round(yin))
    val (xp, yp) = NormalizeRational(y - 2 * x, 2 * y)
    (xp.toDouble, yp.toDouble)
  }

  def normalize_trig(sign: Int, trig: String, x: Double, y: Double): (Int, String, Double, Double, Double) = {
    // normalization in 2Pi, achieving: 0 <= xn / yn <= 2
    val (xn, yn) = normalize_2pi_shift(x, y)
    if (xn > yn) {
      trig match {
        case "sin" => normalize_trig(sign * (-1), "sin", xn - yn, yn)
        case "cos" => normalize_trig(sign * (-1), "cos", xn - yn, yn)
      }
    } else if (xn == yn) {
      trig match {
        case "sin" => (sign, "sin", xn, yn, sign * (+0.0))
        case "cos" => (sign, "cos", xn, yn, sign * (-1.0))
      }
    } else {
      if (xn > yn / 2) {
        // normalization in Pi, achieving 0 <= xn / yn <= 1/2
        val (xp, yp) = normalize_pi_over2_shift(xn, yn)
        trig match {
          case "sin" => normalize_trig(sign * (+1), "cos", xp, yp)
          case "cos" => normalize_trig(sign * (-1), "sin", xp, yp)
        }
      } else if (xn == yn / 2) {
        trig match {
          case "sin" => (sign, "sin", xn, yn, sign * (+1.0))
          case "cos" => (sign, "cos", xn, yn, sign * (+0.0))
        }
      } else {
        // now reflect in Pi / 2, and make sure that 0 <= xn / yn <= 1/4
        if (xn > yn / 4) {
          val (xp, yp) = normalize_pi_over2_reflection(xn, yn)
          trig match {
            case "sin" => (sign, "cos", xp, yp, Double.MaxValue)
            case "cos" => (sign, "sin", xp, yp, Double.MaxValue)
          }
        } else if (xn == yn / 4) {
          (sign, "cos", 1.0, 4.0, Double.MaxValue)
        } else {
          if (xn == 0.0) {
            trig match {
              case "sin" => (sign, "sin", xn, yn, sign * (+0.0))
              case "cos" => (sign, "cos", xn, yn, sign * (+1.0))
            }
          } else {
            trig match {
              case "sin" => (sign, "sin", xn, yn, Double.MaxValue)
              case "cos" => (sign, "cos", xn, yn, Double.MaxValue)
            }
          }
        }
      }
    }
  }

  private def valueSinOrCos(f: String, x: Double, y: Double): Double = {
    val (sign, trig, xn, yn, value) = normalize_trig(1, f, x, y)
    if (!value.equals(scala.Double.MaxValue)) {
      value

    } else {
      trig match {
        case "sin" => (xn, yn) match {
          case (1.0, 6.0) => sign * 0.5
          case _ => sign * Math.sin(xn * Math.PI / yn)
        }
        case "cos" => sign * Math.cos(xn * Math.PI / yn)
      }
    }
  }

  def SinPi(x: Double, y: Double): Double = valueSinOrCos("sin", x, y)

  def CosPi(x: Double, y: Double): Double = valueSinOrCos("cos", x, y)

  private def yieldk(n: Int) = {
    //TODO - find short form for return value
    def tmp() = {
      for (k <- 0 until n
           // this if checks if x^t becomes 1 before n==t, this is e.g. the
           // case for 2nd root of unity of 4 where it becomes 1 at x^2
           if (for (t <- 2 until n - 1
                    if (Math.cos(2 * math.Pi * k * t / n) == 1)
           ) yield 1).isEmpty
      )
        yield k
    }
    tmp.last
  }

  lazy val store = yieldk(n)

  def re(p: Int): Double = {
    val x = CosPi(2.0 * p * store, n)
    x
  }

  def im(p: Int): Double = SinPi(2.0 * p * store, n) * -1.0
}


case class Complex(val re: Double, val im: Double) {
  def +(rhs: Complex): Complex = {
    if (Settings.validate) Settings.sanitycheck = Settings.sanitycheck + 1
    Complex(re + rhs.re, im + rhs.im)
  }

  def -(rhs: Complex): Complex = {
    if (Settings.validate) Settings.sanitycheck = Settings.sanitycheck + 1
    Complex(re - rhs.re, im - rhs.im)
  }

  def *(rhs: Complex): Complex = Complex(re * rhs.re - im * rhs.im, re * rhs.im + im * rhs.re)

}

class ComplexVector(val save: Array[Complex]) extends AnyVal {
  //class ComplexVector(n: Int) extends AnyVal{
  //val save = new Array[Complex](n)

  def apply(i: Int): Complex = save(i)

  def update(i: Int, y: Complex): ComplexVector = {
    save(i) = y
    this
  }

  def print() = {
    save.map(p => println(p))
  }

}

object VectorMult {
  def apply(base: Int, strides: Vector[Int], loopvars: Vector[Int]): Int = {
    //val t = loopvars.reverse.zip(strides)
    var x = base
    val length = loopvars.length
    for (i <- 0 until strides.length)
      x = x + (strides(i) * loopvars(length - 1 - i))

    //val r = base + t.foldLeft(0)({ (acc, ele) => acc + (ele._1 * ele._2) })
    //assert(r == x)
    //r
    x
  }
}

object Testit extends App {
  val t = new testClass

  /*for (i <- 0 until 4) {
    for (j <- 0 until 4)
      println(Twiddle.WHT(4, i, j))
    println(" ------------")
  }*/
  val r = new Random()

  val iterateover = if(Settings.n.isDefined) Vector(Settings.n.get) else (2 until 30)
  for (twopower <-  iterateover){
    val size = Math.pow(2, twopower).toInt

    var fail = false
    val fftmatrix = for (i <- Vector(r.nextInt(size))/*0 until size*/) yield {
      //columns
      val one = Complex(1, 0)
      val zero = Complex(0, 0)

      val input = (0 until size).foldLeft(new ComplexVector(new Array[Complex](size))) {
        (acc, ele) => if (ele == i) acc.update(ele, one) else acc.update(ele, zero)
      }
      val out = new ComplexVector(new Array[Complex](size))
      val instride = Vector(1, 1)
      Settings.sanitycheck = 0
      //VARIOUS CALLS HERE
val res = t.apply(input, out, size) //, 0, instride, 0, instride, Vector.empty)//VARIOUS CALLS HERE END
      println(Settings.sanitycheck)
      if (Settings.validate) {

        for (c <- 0 until res.save.size) {
          val c1 = res(c)
          val c2 = if (Settings.WHT) Twiddle.WHT(size, c, i) else Twiddle.DFT(size, c, i)

          val thres = 1E-3
          if (Math.abs(c1.re - c2.re) > thres) {
            println(c1.re)
            println(c2.re)
            fail = true
          }
          if (Math.abs(c1.im - c2.im) > thres) {
            println(c1.im)
            println(c2.im)
            fail = true
          }
          assert(!fail)
        }
      }


      res
    }
    //println(fftmatrix)
    //val validate = Twiddle.DFT(size)
    //println(validate)

    /*var fail = false
    val thres = 1E-3
    for (i <- 0 until size)
      for (j <- 0 until size) {
        val c1 = fftmatrix(i)(j)
        val c2 = validate(i)(j)
        if (Math.abs(c1.re - c2.re) > thres) {
          println(c1.re)
          println(c2.re)
          fail = true
        }
        if (Math.abs(c1.im - c2.im) > thres) {
          println(c1.im)
          println(c2.im)
          fail = true
        }
      }*/

    if (!fail)
      println(size + " WORKS!!!!")

  }



  /*val one = Complex(0, 0)
  val two = Complex(0, 0)
  val three = Complex(1, 0)
  val four = Complex(0, 0)


  val in = new ComplexVector(4)
  val x1 = in.update(0, one)
  val x2 = x1.update(1, two)
  val x3 = x2.update(2, three)
  val x4 = x3.update(3, four)

  val out = new ComplexVector(4)
  val res = t.apply(x4, out, 4, 0, Vector.empty, 0, Vector.empty, Vector.empty)
  res.print()*/

}
//bla!
/*****************************************
  Emitting Generated Code                  
*******************************************/
class testClass extends (((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int))=> ((SpiralS2.ComplexVector))) {
def apply( helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int))): ((SpiralS2.ComplexVector)) = {
val x1 : SpiralS2.ComplexVector = helper._1
val x2 : SpiralS2.ComplexVector = helper._2
val x3 : Int = helper._3
val x5451 =  DFTnlb1gb0s01s10sb0s01s10v0_x4(x1, x2, x3)

val x5452 = x5451

 (x5452)
}
/*****************************************
  End Main                  
*******************************************/
def DFTnlb1gb0s01s10sb0s01s10v0_x4: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int))) =>{
val x5 : SpiralS2.ComplexVector = helper._1
val x6 : SpiralS2.ComplexVector = helper._2
val x7 : Int = helper._3
val x9 = x7 < 10
val x5448 = if (x9) {
val x13 = x7 < 4
val x506 = if (x13) {
val x17 = x7 < 2
val x154 = if (x17) {
val x21 = x7 < 1
val x80 = if (x21) {
val x54 = if (x21) {
val x37 =  Basen0lb1gb0s01s10sb0s01s10v0_x26(x5, x6)

val x38 = x37
(x38)
} else {
val x51 =  Basen1lb1gb0s01s10sb0s01s10v0_x41(x5, x6)

val x52 = x51
(x52)
}
val x55 = x54
(x55)
} else {
val x77 = if (x17) {
val x51 =  Basen1lb1gb0s01s10sb0s01s10v0_x41(x5, x6)

val x61 = x51
(x61)
} else {
val x74 =  Basen2lb1gb0s01s10sb0s01s10v0_x64(x5, x6)

val x75 = x74
(x75)
}
val x78 = x77
(x78)
}
val x81 = x80
(x81)
} else {
val x85 = x7 < 3
val x151 = if (x85) {
val x107 = if (x85) {
val x74 =  Basen2lb1gb0s01s10sb0s01s10v0_x64(x5, x6)

val x91 = x74
(x91)
} else {
val x104 =  Basen3lb1gb0s01s10sb0s01s10v0_x94(x5, x6)

val x105 = x104
(x105)
}
val x108 = x107
(x108)
} else {
val x148 = if (x13) {
val x104 =  Basen3lb1gb0s01s10sb0s01s10v0_x94(x5, x6)

val x114 = x104
(x114)
} else {
val x145 =  Basen4lb1gb0s01s10sb0s01s10v0_x117(x5, x6)

val x146 = x145
(x146)
}
val x149 = x148
(x149)
}
val x152 = x151
(x152)
}
val x155 = x154
(x155)
} else {
val x159 = x7 < 6
val x503 = if (x159) {
val x163 = x7 < 5
val x253 = if (x163) {
val x203 = if (x163) {
val x145 =  Basen4lb1gb0s01s10sb0s01s10v0_x117(x5, x6)

val x169 = x145
(x169)
} else {
val x200 =  Basen5lb1gb0s01s10sb0s01s10v0_x172(x5, x6)

val x201 = x200
(x201)
}
val x204 = x203
(x204)
} else {
val x250 = if (x159) {
val x200 =  Basen5lb1gb0s01s10sb0s01s10v0_x172(x5, x6)

val x210 = x200
(x210)
} else {
val x247 =  Basen6lb1gb0s01s10sb0s01s10v0_x213(x5, x6)

val x248 = x247
(x248)
}
val x251 = x250
(x251)
}
val x254 = x253
(x254)
} else {
val x258 = x7 < 7
val x500 = if (x258) {
val x304 = if (x258) {
val x247 =  Basen6lb1gb0s01s10sb0s01s10v0_x213(x5, x6)

val x264 = x247
(x264)
} else {
val x301 =  Basen7lb1gb0s01s10sb0s01s10v0_x267(x5, x6)

val x302 = x301
(x302)
}
val x305 = x304
(x305)
} else {
val x309 = x7 < 8
val x497 = if (x309) {
val x397 = if (x309) {
val x301 =  Basen7lb1gb0s01s10sb0s01s10v0_x267(x5, x6)

val x315 = x301
(x315)
} else {
val x394 =  Basen8lb1gb0s01s10sb0s01s10v0_x318(x5, x6)

val x395 = x394
(x395)
}
val x398 = x397
(x398)
} else {
val x402 = x7 < 9
val x494 = if (x402) {
val x394 =  Basen8lb1gb0s01s10sb0s01s10v0_x318(x5, x6)

val x406 = x394
(x406)
} else {
val x491 =  DFTn9lb1gb0s01s10sb0s01s10v0_x409(x5, x6)

val x492 = x491
(x492)
}
val x495 = x494
(x495)
}
val x498 = x497
(x498)
}
val x501 = x500
(x501)
}
val x504 = x503
(x504)
}
val x507 = x506
(x507)
} else {
val x13 = x7 < 4
val x5445 = if (x13) {
val x523 =  F2nlb1gb0s01s10sb0s01s10v0_x512(x5, x6, x7)

val x524 = x523
(x524)
} else {
val x5442 =  DFT_CTnlb1gb0s01s10sb0s01s10v0_x527(x5, x6, x7)

val x5443 = x5442
(x5443)
}
val x5446 = x5445
(x5446)
}
val x5449 = x5448

 (x5449)
}
def Basen0lbgbs01s1sbs01s1v_x1322: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x1323 : SpiralS2.ComplexVector = helper._1
val x1324 : SpiralS2.ComplexVector = helper._2
val x1325 : Int = helper._3
val x1326 : Int = helper._4
val x1327 : Int = helper._5
val x1328 : Int = helper._6
val x1329 : Int = helper._7
val x1330 : Int = helper._8
val x1352 = (0 until x1325).foldLeft( x1323 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x1333 : SpiralS2.ComplexVector = helper._1
val x1334 : Int = helper._2
val x1345 = x1329 + 1
val x1338 = x1327 + 1
val x1341 = x1330 * x1334
val x1335 = x1328 * x1334
val x1342 = x1329 + x1341
val x1339 = x1338 + x1335
val x1336 = x1327 + x1335
val x1346 = x1345 + x1341
val x1340 = x1323(x1339)
val x1337 = x1323(x1336)
val x1343 = x1337 + x1340
val x1347 = x1337 - x1340
val x1344 = x1324.update(x1342,x1343)
val x1348 = x1344.update(x1346,x1347)
(x1348)
})
val x1353 = x1352

 (x1353)
}
def Basen5lbimbs0s11twb0s0s11v_x2395: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int))) =>{
val x2396 : SpiralS2.ComplexVector = helper._1
val x2397 : SpiralS2.ComplexVector = helper._2
val x2398 : Int = helper._3
val x2399 : Int = helper._4
val x2400 : Int = helper._5
val x2401 : Int = helper._6
val x2402 : Int = helper._7
val x2440 = (0 until x2398).foldLeft( x2396 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x2405 : SpiralS2.ComplexVector = helper._1
val x2406 : Int = helper._2
val x2407 = x2401 * 2
val x2408 = x2400 + x2406
val x2410 = x2408 + x2401
val x2416 = x2408 + x2407
val x2418 = x2410 + x2407
val x2417 = x2396(x2416)
val x2431 = x2416 + x2401
val x2419 = x2396(x2418)
val x2420 = x2417 + x2419
val x2422 = x2417 - x2419
val x2421 = x2397.update(x2416,x2420)
val x2423 = x2421.update(x2418,x2422)
val x2430 = x2423(x2410)
val x2432 = x2423(x2431)
val x2433 = x2430 + x2432
val x2435 = x2430 - x2432
val x2434 = x2397.update(x2410,x2433)
val x2436 = x2434.update(x2431,x2435)
(x2436)
})
val x2441 = x2440

 (x2441)
}
def Basen8lbgbs01s1sbs01s1v_x1808: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x1809 : SpiralS2.ComplexVector = helper._1
val x1810 : SpiralS2.ComplexVector = helper._2
val x1811 : Int = helper._3
val x1812 : Int = helper._4
val x1813 : Int = helper._5
val x1814 : Int = helper._6
val x1815 : Int = helper._7
val x1816 : Int = helper._8
val x1918 = (0 until x1811).foldLeft( x1809 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x1819 : SpiralS2.ComplexVector = helper._1
val x1820 : Int = helper._2
val x1823 = x1816 * x1820
val x1821 = x1814 * x1820
val x1824 = x1815 + x1823
val x1822 = x1813 + x1821
val x1907 = x1824 + 3
val x1857 = x1824 + 4
val x1863 = x1857 + 1
val x1856 = x1822 + 4
val x1909 = x1857 + 3
val x1873 = x1863 + 2
val x1866 = x1856 + 2
val x1859 = x1856 + 1
val x1870 = x1857 + 2
val x1867 = x1809(x1866)
val x1868 = x1859 + 2
val x1883 = x1870 + 1
val x1869 = x1809(x1868)
val x1871 = x1867 + x1869
val x1874 = x1867 - x1869
val x1872 = x1810.update(x1870,x1871)
val x1875 = x1872.update(x1873,x1874)
val x1882 = x1875(x1863)
val x1884 = x1875(x1883)
val x1885 = x1882 + x1884
val x1887 = x1882 - x1884
val x1886 = x1810.update(x1863,x1885)
val x1888 = x1886.update(x1883,x1887)
val x1908 = x1888(x1907)
val x1910 = x1888(x1909)
val x1911 = x1908 + x1910
val x1913 = x1908 - x1910
val x1912 = x1810.update(x1907,x1911)
val x1914 = x1912.update(x1909,x1913)
(x1914)
})
val x1919 = x1918

 (x1919)
}
def Basen3lbimbs0s11twb0s0s11v_x2282: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int))) =>{
val x2283 : SpiralS2.ComplexVector = helper._1
val x2284 : SpiralS2.ComplexVector = helper._2
val x2285 : Int = helper._3
val x2286 : Int = helper._4
val x2287 : Int = helper._5
val x2288 : Int = helper._6
val x2289 : Int = helper._7
val x2306 = (0 until x2285).foldLeft( x2283 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x2292 : SpiralS2.ComplexVector = helper._1
val x2293 : Int = helper._2
val x2296 = x2287 + x2288
val x2294 = x2287 + x2293
val x2297 = x2296 + x2293
val x2295 = x2283(x2294)
val x2298 = x2283(x2297)
val x2299 = x2295 + x2298
val x2301 = x2295 - x2298
val x2300 = x2284.update(x2294,x2299)
val x2302 = x2300.update(x2297,x2301)
(x2302)
})
val x2307 = x2306

 (x2307)
}
def DFTnlbgb0s01s1sb0s01s1v0_x533: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int))) =>{
val x534 : SpiralS2.ComplexVector = helper._1
val x535 : SpiralS2.ComplexVector = helper._2
val x536 : Int = helper._3
val x537 : Int = helper._4
val x538 : Int = helper._5
val x539 : Int = helper._6
val x540 = x536 < 10
val x4690 = if (x540) {
val x543 = x536 < 4
val x1246 = if (x543) {
val x546 = x536 < 2
val x765 = if (x546) {
val x549 = x536 < 1
val x655 = if (x549) {
val x613 = if (x549) {
val x580 =  Basen0lbgb0s01s1sb0s01s1v0_x554(x534, x535, x537, x538, x539)

val x581 = x580
(x581)
} else {
val x610 =  Basen1lbgb0s01s1sb0s01s1v0_x584(x534, x535, x537, x538, x539)

val x611 = x610
(x611)
}
val x614 = x613
(x614)
} else {
val x652 = if (x546) {
val x610 =  Basen1lbgb0s01s1sb0s01s1v0_x584(x534, x535, x537, x538, x539)

val x620 = x610
(x620)
} else {
val x649 =  Basen2lbgb0s01s1sb0s01s1v0_x623(x534, x535, x537, x538, x539)

val x650 = x649
(x650)
}
val x653 = x652
(x653)
}
val x656 = x655
(x656)
} else {
val x659 = x536 < 3
val x762 = if (x659) {
val x697 = if (x659) {
val x649 =  Basen2lbgb0s01s1sb0s01s1v0_x623(x534, x535, x537, x538, x539)

val x665 = x649
(x665)
} else {
val x694 =  Basen3lbgb0s01s1sb0s01s1v0_x668(x534, x535, x537, x538, x539)

val x695 = x694
(x695)
}
val x698 = x697
(x698)
} else {
val x759 = if (x543) {
val x694 =  Basen3lbgb0s01s1sb0s01s1v0_x668(x534, x535, x537, x538, x539)

val x704 = x694
(x704)
} else {
val x756 =  Basen4lbgb0s01s1sb0s01s1v0_x707(x534, x535, x537, x538, x539)

val x757 = x756
(x757)
}
val x760 = x759
(x760)
}
val x763 = x762
(x763)
}
val x766 = x765
(x766)
} else {
val x769 = x536 < 6
val x1243 = if (x769) {
val x772 = x536 < 5
val x906 = if (x772) {
val x833 = if (x772) {
val x756 =  Basen4lbgb0s01s1sb0s01s1v0_x707(x534, x535, x537, x538, x539)

val x778 = x756
(x778)
} else {
val x830 =  Basen5lbgb0s01s1sb0s01s1v0_x781(x534, x535, x537, x538, x539)

val x831 = x830
(x831)
}
val x834 = x833
(x834)
} else {
val x903 = if (x769) {
val x830 =  Basen5lbgb0s01s1sb0s01s1v0_x781(x534, x535, x537, x538, x539)

val x840 = x830
(x840)
} else {
val x900 =  Basen6lbgb0s01s1sb0s01s1v0_x843(x534, x535, x537, x538, x539)

val x901 = x900
(x901)
}
val x904 = x903
(x904)
}
val x907 = x906
(x907)
} else {
val x910 = x536 < 7
val x1240 = if (x910) {
val x979 = if (x910) {
val x900 =  Basen6lbgb0s01s1sb0s01s1v0_x843(x534, x535, x537, x538, x539)

val x916 = x900
(x916)
} else {
val x976 =  Basen7lbgb0s01s1sb0s01s1v0_x919(x534, x535, x537, x538, x539)

val x977 = x976
(x977)
}
val x980 = x979
(x980)
} else {
val x983 = x536 < 8
val x1237 = if (x983) {
val x1103 = if (x983) {
val x976 =  Basen7lbgb0s01s1sb0s01s1v0_x919(x534, x535, x537, x538, x539)

val x989 = x976
(x989)
} else {
val x1100 =  Basen8lbgb0s01s1sb0s01s1v0_x992(x534, x535, x537, x538, x539)

val x1101 = x1100
(x1101)
}
val x1104 = x1103
(x1104)
} else {
val x1107 = x536 < 9
val x1234 = if (x1107) {
val x1100 =  Basen8lbgb0s01s1sb0s01s1v0_x992(x534, x535, x537, x538, x539)

val x1111 = x1100
(x1111)
} else {
val x1231 =  DFTn9lbgb0s01s1sb0s01s1v0_x1114(x534, x535, x537, x538, x539)

val x1232 = x1231
(x1232)
}
val x1235 = x1234
(x1235)
}
val x1238 = x1237
(x1238)
}
val x1241 = x1240
(x1241)
}
val x1244 = x1243
(x1244)
}
val x1247 = x1246
(x1247)
} else {
val x543 = x536 < 4
val x4687 = if (x543) {
val x1279 =  F2nlbgb0s01s1sb0s01s1v0_x1252(x534, x535, x536, x537, x538, x539)

val x1280 = x1279
(x1280)
} else {
val x4684 =  DFT_CTnlbgb0s01s1sb0s01s1v0_x1283(x534, x535, x536, x537, x538, x539)

val x4685 = x4684
(x4685)
}
val x4688 = x4687
(x4688)
}
val x4691 = x4690

 (x4691)
}
def Basen7lbgbs0s1sbs0s1v_x3365: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int))) =>{
val x3366 : SpiralS2.ComplexVector = helper._1
val x3367 : SpiralS2.ComplexVector = helper._2
val x3368 : Int = helper._3
val x3369 : Int = helper._4
val x3370 : Int = helper._5
val x3371 : Int = helper._6
val x3372 : Int = helper._7
val x3373 : Int = helper._8
val x3374 : Int = helper._9
val x3375 : Int = helper._10
val x3429 = (0 until x3368).foldLeft( x3366 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x3378 : SpiralS2.ComplexVector = helper._1
val x3379 : Int = helper._2
val x3380 = x3371 * 3
val x3383 = x3374 * 3
val x3417 = x3374 * 2
val x3384 = x3375 * x3379
val x3381 = x3372 * x3379
val x3385 = x3373 + x3384
val x3382 = x3370 + x3381
val x3418 = x3385 + x3417
val x3387 = x3382 + x3371
val x3396 = x3387 + x3380
val x3397 = x3366(x3396)
val x3391 = x3385 + x3374
val x3398 = x3385 + x3383
val x3394 = x3382 + x3380
val x3401 = x3391 + x3383
val x3420 = x3398 + x3417
val x3395 = x3366(x3394)
val x3399 = x3395 + x3397
val x3402 = x3395 - x3397
val x3400 = x3367.update(x3398,x3399)
val x3403 = x3400.update(x3401,x3402)
val x3419 = x3403(x3418)
val x3421 = x3403(x3420)
val x3422 = x3419 + x3421
val x3424 = x3419 - x3421
val x3423 = x3367.update(x3418,x3422)
val x3425 = x3423.update(x3420,x3424)
(x3425)
})
val x3430 = x3429

 (x3430)
}
def DFT_CTnlbgbs0s1sbs0s1v_x3777: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int,Int))) =>{
val x3778 : SpiralS2.ComplexVector = helper._1
val x3779 : SpiralS2.ComplexVector = helper._2
val x3780 : Int = helper._3
val x3781 : Int = helper._4
val x3782 : Int = helper._5
val x3783 : Int = helper._6
val x3784 : Int = helper._7
val x3785 : Int = helper._8
val x3786 : Int = helper._9
val x3787 : Int = helper._10
val x3788 : Int = helper._11
val x4624 = (0 until x3781).foldLeft( x3778 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x3793 : SpiralS2.ComplexVector = helper._1
val x3794 : Int = helper._2
val x3799 = x3788 * x3794
val x3796 = x3785 * x3794
val x3800 = x3786 + x3799
val x3797 = x3783 + x3796
val x3789 = x3780 / 2
val x3798 = x3787 * x3789
val x3795 = x3784 * x3789
val x3790 = x3780 / x3789
val x3802 =  DFTnlbgbs0s1sbs0s1v_x2910(x3778, x3779, x3789, x3790, x3794, x3797, x3784, x3795, x3800, x3787, x3798)

val x3803 = x3802
val x4619 =  DFTnlbimbs0s1twb0s0s11v_x3804(x3803, x3779, x3790, x3789, x3794, x3800, x3798, x3787, x3789)

val x4620 = x4619
(x4620)
})
val x4625 = x4624

 (x4625)
}
def DFTn9lbgbs01s1sbs01s1v_x1935: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x1936 : SpiralS2.ComplexVector = helper._1
val x1937 : SpiralS2.ComplexVector = helper._2
val x1938 : Int = helper._3
val x1939 : Int = helper._4
val x1940 : Int = helper._5
val x1941 : Int = helper._6
val x1942 : Int = helper._7
val x1943 : Int = helper._8
val x2057 =  DFT_CTn9lbgbs01s1sbs01s1v_x1944(x1936, x1937, x1938, x1939, x1940, x1941, x1942, x1943)

val x2058 = x2057

 (x2058)
}
def Basen1lbgbs01s1sbs01s1v_x1359: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x1360 : SpiralS2.ComplexVector = helper._1
val x1361 : SpiralS2.ComplexVector = helper._2
val x1362 : Int = helper._3
val x1363 : Int = helper._4
val x1364 : Int = helper._5
val x1365 : Int = helper._6
val x1366 : Int = helper._7
val x1367 : Int = helper._8
val x1389 = (0 until x1362).foldLeft( x1360 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x1370 : SpiralS2.ComplexVector = helper._1
val x1371 : Int = helper._2
val x1375 = x1364 + 1
val x1382 = x1366 + 1
val x1372 = x1365 * x1371
val x1378 = x1367 * x1371
val x1379 = x1366 + x1378
val x1373 = x1364 + x1372
val x1383 = x1382 + x1378
val x1376 = x1375 + x1372
val x1374 = x1360(x1373)
val x1377 = x1360(x1376)
val x1380 = x1374 + x1377
val x1384 = x1374 - x1377
val x1381 = x1361.update(x1379,x1380)
val x1385 = x1381.update(x1383,x1384)
(x1385)
})
val x1390 = x1389

 (x1390)
}
def Basen1lbimb0s0s11twb0s0s11v0_x4743: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x4744 : SpiralS2.ComplexVector = helper._1
val x4745 : SpiralS2.ComplexVector = helper._2
val x4746 : Int = helper._3
val x4747 : Int = helper._4
val x4748 : Int = helper._5
val x4763 = (0 until x4746).foldLeft( x4744 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x4751 : SpiralS2.ComplexVector = helper._1
val x4752 : Int = helper._2
val x4753 = x4744(x4752)
val x4754 = x4747 + x4752
val x4755 = x4744(x4754)
val x4756 = x4753 + x4755
val x4758 = x4753 - x4755
val x4757 = x4745.update(x4752,x4756)
val x4759 = x4757.update(x4754,x4758)
(x4759)
})
val x4764 = x4763

 (x4764)
}
def DFTnlbgbs01s1sbs01s1v_x1298: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int))) =>{
val x1299 : SpiralS2.ComplexVector = helper._1
val x1300 : SpiralS2.ComplexVector = helper._2
val x1301 : Int = helper._3
val x1302 : Int = helper._4
val x1303 : Int = helper._5
val x1304 : Int = helper._6
val x1305 : Int = helper._7
val x1306 : Int = helper._8
val x1307 : Int = helper._9
val x1308 = x1301 < 10
val x4670 = if (x1308) {
val x1311 = x1301 < 4
val x2075 = if (x1311) {
val x1314 = x1301 < 2
val x1566 = if (x1314) {
val x1317 = x1301 < 1
val x1444 = if (x1317) {
val x1395 = if (x1317) {
val x1355 =  Basen0lbgbs01s1sbs01s1v_x1322(x1299, x1300, x1302, x1303, x1304, x1305, x1306, x1307)

val x1356 = x1355
(x1356)
} else {
val x1392 =  Basen1lbgbs01s1sbs01s1v_x1359(x1299, x1300, x1302, x1303, x1304, x1305, x1306, x1307)

val x1393 = x1392
(x1393)
}
val x1396 = x1395
(x1396)
} else {
val x1441 = if (x1314) {
val x1392 =  Basen1lbgbs01s1sbs01s1v_x1359(x1299, x1300, x1302, x1303, x1304, x1305, x1306, x1307)

val x1402 = x1392
(x1402)
} else {
val x1438 =  Basen2lbgbs01s1sbs01s1v_x1405(x1299, x1300, x1302, x1303, x1304, x1305, x1306, x1307)

val x1439 = x1438
(x1439)
}
val x1442 = x1441
(x1442)
}
val x1445 = x1444
(x1445)
} else {
val x1448 = x1301 < 3
val x1563 = if (x1448) {
val x1493 = if (x1448) {
val x1438 =  Basen2lbgbs01s1sbs01s1v_x1405(x1299, x1300, x1302, x1303, x1304, x1305, x1306, x1307)

val x1454 = x1438
(x1454)
} else {
val x1490 =  Basen3lbgbs01s1sbs01s1v_x1457(x1299, x1300, x1302, x1303, x1304, x1305, x1306, x1307)

val x1491 = x1490
(x1491)
}
val x1494 = x1493
(x1494)
} else {
val x1560 = if (x1311) {
val x1490 =  Basen3lbgbs01s1sbs01s1v_x1457(x1299, x1300, x1302, x1303, x1304, x1305, x1306, x1307)

val x1500 = x1490
(x1500)
} else {
val x1557 =  Basen4lbgbs01s1sbs01s1v_x1503(x1299, x1300, x1302, x1303, x1304, x1305, x1306, x1307)

val x1558 = x1557
(x1558)
}
val x1561 = x1560
(x1561)
}
val x1564 = x1563
(x1564)
}
val x1567 = x1566
(x1567)
} else {
val x1570 = x1301 < 6
val x2072 = if (x1570) {
val x1573 = x1301 < 5
val x1717 = if (x1573) {
val x1639 = if (x1573) {
val x1557 =  Basen4lbgbs01s1sbs01s1v_x1503(x1299, x1300, x1302, x1303, x1304, x1305, x1306, x1307)

val x1579 = x1557
(x1579)
} else {
val x1636 =  Basen5lbgbs01s1sbs01s1v_x1582(x1299, x1300, x1302, x1303, x1304, x1305, x1306, x1307)

val x1637 = x1636
(x1637)
}
val x1640 = x1639
(x1640)
} else {
val x1714 = if (x1570) {
val x1636 =  Basen5lbgbs01s1sbs01s1v_x1582(x1299, x1300, x1302, x1303, x1304, x1305, x1306, x1307)

val x1646 = x1636
(x1646)
} else {
val x1711 =  Basen6lbgbs01s1sbs01s1v_x1649(x1299, x1300, x1302, x1303, x1304, x1305, x1306, x1307)

val x1712 = x1711
(x1712)
}
val x1715 = x1714
(x1715)
}
val x1718 = x1717
(x1718)
} else {
val x1721 = x1301 < 7
val x2069 = if (x1721) {
val x1795 = if (x1721) {
val x1711 =  Basen6lbgbs01s1sbs01s1v_x1649(x1299, x1300, x1302, x1303, x1304, x1305, x1306, x1307)

val x1727 = x1711
(x1727)
} else {
val x1792 =  Basen7lbgbs01s1sbs01s1v_x1730(x1299, x1300, x1302, x1303, x1304, x1305, x1306, x1307)

val x1793 = x1792
(x1793)
}
val x1796 = x1795
(x1796)
} else {
val x1799 = x1301 < 8
val x2066 = if (x1799) {
val x1924 = if (x1799) {
val x1792 =  Basen7lbgbs01s1sbs01s1v_x1730(x1299, x1300, x1302, x1303, x1304, x1305, x1306, x1307)

val x1805 = x1792
(x1805)
} else {
val x1921 =  Basen8lbgbs01s1sbs01s1v_x1808(x1299, x1300, x1302, x1303, x1304, x1305, x1306, x1307)

val x1922 = x1921
(x1922)
}
val x1925 = x1924
(x1925)
} else {
val x1928 = x1301 < 9
val x2063 = if (x1928) {
val x1921 =  Basen8lbgbs01s1sbs01s1v_x1808(x1299, x1300, x1302, x1303, x1304, x1305, x1306, x1307)

val x1932 = x1921
(x1932)
} else {
val x2060 =  DFTn9lbgbs01s1sbs01s1v_x1935(x1299, x1300, x1302, x1303, x1304, x1305, x1306, x1307)

val x2061 = x2060
(x2061)
}
val x2064 = x2063
(x2064)
}
val x2067 = x2066
(x2067)
}
val x2070 = x2069
(x2070)
}
val x2073 = x2072
(x2073)
}
val x2076 = x2075
(x2076)
} else {
val x1311 = x1301 < 4
val x4667 = if (x1311) {
val x2115 =  F2nlbgbs01s1sbs01s1v_x2081(x1299, x1300, x1301, x1302, x1303, x1304, x1305, x1306, x1307)

val x2116 = x2115
(x2116)
} else {
val x4664 =  DFT_CTnlbgbs01s1sbs01s1v_x2119(x1299, x1300, x1301, x1302, x1303, x1304, x1305, x1306, x1307)

val x4665 = x4664
(x4665)
}
val x4668 = x4667
(x4668)
}
val x4671 = x4670

 (x4671)
}
def Basen4lbgbs0s1sbs0s1v_x3125: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int))) =>{
val x3126 : SpiralS2.ComplexVector = helper._1
val x3127 : SpiralS2.ComplexVector = helper._2
val x3128 : Int = helper._3
val x3129 : Int = helper._4
val x3130 : Int = helper._5
val x3131 : Int = helper._6
val x3132 : Int = helper._7
val x3133 : Int = helper._8
val x3134 : Int = helper._9
val x3135 : Int = helper._10
val x3180 = (0 until x3128).foldLeft( x3126 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x3138 : SpiralS2.ComplexVector = helper._1
val x3139 : Int = helper._2
val x3140 = x3131 * 2
val x3144 = x3135 * x3139
val x3141 = x3132 * x3139
val x3143 = x3134 * 2
val x3145 = x3133 + x3144
val x3142 = x3130 + x3141
val x3151 = x3145 + x3134
val x3154 = x3142 + x3140
val x3147 = x3142 + x3131
val x3158 = x3145 + x3143
val x3155 = x3126(x3154)
val x3161 = x3151 + x3143
val x3156 = x3147 + x3140
val x3171 = x3158 + x3134
val x3157 = x3126(x3156)
val x3159 = x3155 + x3157
val x3162 = x3155 - x3157
val x3160 = x3127.update(x3158,x3159)
val x3163 = x3160.update(x3161,x3162)
val x3170 = x3163(x3151)
val x3172 = x3163(x3171)
val x3173 = x3170 + x3172
val x3175 = x3170 - x3172
val x3174 = x3127.update(x3151,x3173)
val x3176 = x3174.update(x3171,x3175)
(x3176)
})
val x3181 = x3180

 (x3181)
}
def Basen3lbimbs0s1twb0s0s11v_x3951: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x3952 : SpiralS2.ComplexVector = helper._1
val x3953 : SpiralS2.ComplexVector = helper._2
val x3954 : Int = helper._3
val x3955 : Int = helper._4
val x3956 : Int = helper._5
val x3957 : Int = helper._6
val x3958 : Int = helper._7
val x3959 : Int = helper._8
val x3977 = (0 until x3954).foldLeft( x3952 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x3962 : SpiralS2.ComplexVector = helper._1
val x3963 : Int = helper._2
val x3967 = x3956 + x3957
val x3964 = x3958 * x3963
val x3965 = x3956 + x3964
val x3968 = x3967 + x3964
val x3966 = x3952(x3965)
val x3969 = x3952(x3968)
val x3970 = x3966 + x3969
val x3972 = x3966 - x3969
val x3971 = x3953.update(x3965,x3970)
val x3973 = x3971.update(x3968,x3972)
(x3973)
})
val x3978 = x3977

 (x3978)
}
def Basen6lbimbs0s11twb0s0s11v_x2456: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int))) =>{
val x2457 : SpiralS2.ComplexVector = helper._1
val x2458 : SpiralS2.ComplexVector = helper._2
val x2459 : Int = helper._3
val x2460 : Int = helper._4
val x2461 : Int = helper._5
val x2462 : Int = helper._6
val x2463 : Int = helper._7
val x2510 = (0 until x2459).foldLeft( x2457 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x2466 : SpiralS2.ComplexVector = helper._1
val x2467 : Int = helper._2
val x2468 = x2462 * 3
val x2498 = x2462 * 2
val x2469 = x2461 + x2467
val x2499 = x2469 + x2498
val x2477 = x2469 + x2468
val x2471 = x2469 + x2462
val x2478 = x2457(x2477)
val x2501 = x2477 + x2498
val x2479 = x2471 + x2468
val x2480 = x2457(x2479)
val x2481 = x2478 + x2480
val x2483 = x2478 - x2480
val x2482 = x2458.update(x2477,x2481)
val x2484 = x2482.update(x2479,x2483)
val x2500 = x2484(x2499)
val x2502 = x2484(x2501)
val x2503 = x2500 + x2502
val x2505 = x2500 - x2502
val x2504 = x2458.update(x2499,x2503)
val x2506 = x2504.update(x2501,x2505)
(x2506)
})
val x2511 = x2510

 (x2511)
}
def Basen4lbimb0s0s11twb0s0s11v0_x4857: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x4858 : SpiralS2.ComplexVector = helper._1
val x4859 : SpiralS2.ComplexVector = helper._2
val x4860 : Int = helper._3
val x4861 : Int = helper._4
val x4862 : Int = helper._5
val x4899 = (0 until x4860).foldLeft( x4858 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x4865 : SpiralS2.ComplexVector = helper._1
val x4866 : Int = helper._2
val x4869 = x4866 + x4861
val x4867 = x4861 * 2
val x4875 = x4866 + x4867
val x4877 = x4869 + x4867
val x4876 = x4858(x4875)
val x4890 = x4875 + x4861
val x4878 = x4858(x4877)
val x4879 = x4876 + x4878
val x4881 = x4876 - x4878
val x4880 = x4859.update(x4875,x4879)
val x4882 = x4880.update(x4877,x4881)
val x4889 = x4882(x4869)
val x4891 = x4882(x4890)
val x4892 = x4889 + x4891
val x4894 = x4889 - x4891
val x4893 = x4859.update(x4869,x4892)
val x4895 = x4893.update(x4890,x4894)
(x4895)
})
val x4900 = x4899

 (x4900)
}
def DFT_CTnlbimbs0s11twb0s0s11v_x2893: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x2894 : SpiralS2.ComplexVector = helper._1
val x2895 : SpiralS2.ComplexVector = helper._2
val x2896 : Int = helper._3
val x2897 : Int = helper._4
val x2898 : Int = helper._5
val x2899 : Int = helper._6
val x2900 : Int = helper._7
val x2901 : Int = helper._8
val x4644 = (0 until x2897).foldLeft( x2894 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x2906 : SpiralS2.ComplexVector = helper._1
val x2907 : Int = helper._2
val x2902 = x2896 / 2
val x2909 = x2899 + x2907
val x2908 = x2900 * x2902
val x2903 = x2896 / x2902
val x4636 =  DFTnlbgbs0s1sbs0s1v_x2910(x2894, x2895, x2902, x2903, x2907, x2909, x2900, x2908, x2909, x2900, x2908)

val x4637 = x4636
val x4639 =  DFTnlbimbs0s1twb0s0s11v_x3804(x4637, x2895, x2903, x2902, x2907, x2909, x2908, x2900, x2902)

val x4640 = x4639
(x4640)
})
val x4645 = x4644

 (x4645)
}
def Basen2lbimbs0s11twb0s0s11v_x2236: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int))) =>{
val x2237 : SpiralS2.ComplexVector = helper._1
val x2238 : SpiralS2.ComplexVector = helper._2
val x2239 : Int = helper._3
val x2240 : Int = helper._4
val x2241 : Int = helper._5
val x2242 : Int = helper._6
val x2243 : Int = helper._7
val x2260 = (0 until x2239).foldLeft( x2237 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x2246 : SpiralS2.ComplexVector = helper._1
val x2247 : Int = helper._2
val x2250 = x2241 + x2242
val x2248 = x2241 + x2247
val x2251 = x2250 + x2247
val x2249 = x2237(x2248)
val x2252 = x2237(x2251)
val x2253 = x2249 + x2252
val x2255 = x2249 - x2252
val x2254 = x2238.update(x2248,x2253)
val x2256 = x2254.update(x2251,x2255)
(x2256)
})
val x2261 = x2260

 (x2261)
}
def Basen2lbgb0s01s1sb0s01s1v0_x623: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x624 : SpiralS2.ComplexVector = helper._1
val x625 : SpiralS2.ComplexVector = helper._2
val x626 : Int = helper._3
val x627 : Int = helper._4
val x628 : Int = helper._5
val x646 = (0 until x626).foldLeft( x624 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x631 : SpiralS2.ComplexVector = helper._1
val x632 : Int = helper._2
val x637 = x628 * x632
val x633 = x627 * x632
val x640 = 1 + x637
val x634 = x624(x633)
val x635 = 1 + x633
val x636 = x624(x635)
val x638 = x634 + x636
val x641 = x634 - x636
val x639 = x625.update(x637,x638)
val x642 = x639.update(x640,x641)
(x642)
})
val x647 = x646

 (x647)
}
def DFT_CTnlbgb0s01s1sb0s01s1v0_x1283: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int))) =>{
val x1284 : SpiralS2.ComplexVector = helper._1
val x1285 : SpiralS2.ComplexVector = helper._2
val x1286 : Int = helper._3
val x1287 : Int = helper._4
val x1288 : Int = helper._5
val x1289 : Int = helper._6
val x4681 = (0 until x1287).foldLeft( x1284 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x1294 : SpiralS2.ComplexVector = helper._1
val x1295 : Int = helper._2
val x1296 = x1288 * x1295
val x1297 = x1289 * x1295
val x1290 = x1286 / 2
val x1291 = x1286 / x1290
val x4673 =  DFTnlbgbs01s1sbs01s1v_x1298(x1284, x1285, x1290, x1291, x1295, x1296, x1290, x1297, x1290)

val x4674 = x4673
val x4676 =  DFTnlbimbs0s11twb0s0s11v_x2142(x4674, x1285, x1291, x1290, x1295, x1297, x1290, x1290)

val x4677 = x4676
(x4677)
})
val x4682 = x4681

 (x4682)
}
def F2nlbimbs0s11twb0s0s11v_x2861: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x2862 : SpiralS2.ComplexVector = helper._1
val x2863 : SpiralS2.ComplexVector = helper._2
val x2864 : Int = helper._3
val x2865 : Int = helper._4
val x2866 : Int = helper._5
val x2867 : Int = helper._6
val x2868 : Int = helper._7
val x2869 : Int = helper._8
val x2886 = (0 until x2865).foldLeft( x2862 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x2872 : SpiralS2.ComplexVector = helper._1
val x2873 : Int = helper._2
val x2876 = x2867 + x2868
val x2874 = x2867 + x2873
val x2877 = x2876 + x2873
val x2875 = x2862(x2874)
val x2878 = x2862(x2877)
val x2879 = x2875 + x2878
val x2881 = x2875 - x2878
val x2880 = x2863.update(x2874,x2879)
val x2882 = x2880.update(x2877,x2881)
(x2882)
})
val x2887 = x2886

 (x2887)
}
def Basen2lbgbs01s1sbs01s1v_x1405: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x1406 : SpiralS2.ComplexVector = helper._1
val x1407 : SpiralS2.ComplexVector = helper._2
val x1408 : Int = helper._3
val x1409 : Int = helper._4
val x1410 : Int = helper._5
val x1411 : Int = helper._6
val x1412 : Int = helper._7
val x1413 : Int = helper._8
val x1435 = (0 until x1408).foldLeft( x1406 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x1416 : SpiralS2.ComplexVector = helper._1
val x1417 : Int = helper._2
val x1418 = x1411 * x1417
val x1424 = x1413 * x1417
val x1425 = x1412 + x1424
val x1419 = x1410 + x1418
val x1428 = x1412 + 1
val x1421 = x1410 + 1
val x1420 = x1406(x1419)
val x1429 = x1428 + x1424
val x1422 = x1421 + x1418
val x1423 = x1406(x1422)
val x1426 = x1420 + x1423
val x1430 = x1420 - x1423
val x1427 = x1407.update(x1425,x1426)
val x1431 = x1427.update(x1429,x1430)
(x1431)
})
val x1436 = x1435

 (x1436)
}
def DFT_CTnlb1gb0s01s10sb0s01s10v0_x527: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int))) =>{
val x528 : SpiralS2.ComplexVector = helper._1
val x529 : SpiralS2.ComplexVector = helper._2
val x530 : Int = helper._3
val x531 = x530 / 2
val x532 = x530 / x531
val x4693 =  DFTnlbgb0s01s1sb0s01s1v0_x533(x528, x529, x531, x532, x531, x531)

val x4694 = x4693
val x5439 =  DFTnlbimb0s0s11twb0s0s11v0_x4695(x4694, x529, x532, x531, x531, x531)

val x5440 = x5439

 (x5440)
}
def DFTn9lbgbs0s1sbs0s1v_x3582: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int))) =>{
val x3583 : SpiralS2.ComplexVector = helper._1
val x3584 : SpiralS2.ComplexVector = helper._2
val x3585 : Int = helper._3
val x3586 : Int = helper._4
val x3587 : Int = helper._5
val x3588 : Int = helper._6
val x3589 : Int = helper._7
val x3590 : Int = helper._8
val x3591 : Int = helper._9
val x3592 : Int = helper._10
val x3713 =  DFT_CTn9lbgbs0s1sbs0s1v_x3593(x3583, x3584, x3585, x3586, x3587, x3588, x3589, x3590, x3591, x3592)

val x3714 = x3713

 (x3714)
}
def Basen4lb1gb0s01s10sb0s01s10v0_x117: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector))) =>{
val x118 : SpiralS2.ComplexVector = helper._1
val x119 : SpiralS2.ComplexVector = helper._2
val x127 = x118(3)
val x126 = x118(2)
val x128 = x126 + x127
val x130 = x126 - x127
val x129 = x119.update(2,x128)
val x131 = x129.update(3,x130)
val x138 = x131(1)
val x139 = x131(3)
val x140 = x138 + x139
val x142 = x138 - x139
val x141 = x119.update(1,x140)
val x143 = x141.update(3,x142)

 (x143)
}
def F2nlb1gb0s01s10sb0s01s10v0_x512: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int))) =>{
val x513 : SpiralS2.ComplexVector = helper._1
val x514 : SpiralS2.ComplexVector = helper._2
val x515 : Int = helper._3
val x516 = x513(0)
val x517 = x513(1)
val x518 = x516 + x517
val x520 = x516 - x517
val x519 = x514.update(0,x518)
val x521 = x519.update(1,x520)

 (x521)
}
def DFTnlbgbs0s1sbs0s1v_x2910: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int,Int))) =>{
val x2911 : SpiralS2.ComplexVector = helper._1
val x2912 : SpiralS2.ComplexVector = helper._2
val x2913 : Int = helper._3
val x2914 : Int = helper._4
val x2915 : Int = helper._5
val x2916 : Int = helper._6
val x2917 : Int = helper._7
val x2918 : Int = helper._8
val x2919 : Int = helper._9
val x2920 : Int = helper._10
val x2921 : Int = helper._11
val x2922 = x2913 < 10
val x4633 = if (x2922) {
val x2925 = x2913 < 4
val x3731 = if (x2925) {
val x2928 = x2913 < 2
val x3192 = if (x2928) {
val x2931 = x2913 < 1
val x3064 = if (x2931) {
val x3013 = if (x2931) {
val x2971 =  Basen0lbgbs0s1sbs0s1v_x2936(x2911, x2912, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x2972 = x2971
(x2972)
} else {
val x3010 =  Basen1lbgbs0s1sbs0s1v_x2975(x2911, x2912, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x3011 = x3010
(x3011)
}
val x3014 = x3013
(x3014)
} else {
val x3061 = if (x2928) {
val x3010 =  Basen1lbgbs0s1sbs0s1v_x2975(x2911, x2912, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x3020 = x3010
(x3020)
} else {
val x3058 =  Basen2lbgbs0s1sbs0s1v_x3023(x2911, x2912, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x3059 = x3058
(x3059)
}
val x3062 = x3061
(x3062)
}
val x3065 = x3064
(x3065)
} else {
val x3068 = x2913 < 3
val x3189 = if (x3068) {
val x3115 = if (x3068) {
val x3058 =  Basen2lbgbs0s1sbs0s1v_x3023(x2911, x2912, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x3074 = x3058
(x3074)
} else {
val x3112 =  Basen3lbgbs0s1sbs0s1v_x3077(x2911, x2912, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x3113 = x3112
(x3113)
}
val x3116 = x3115
(x3116)
} else {
val x3186 = if (x2925) {
val x3112 =  Basen3lbgbs0s1sbs0s1v_x3077(x2911, x2912, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x3122 = x3112
(x3122)
} else {
val x3183 =  Basen4lbgbs0s1sbs0s1v_x3125(x2911, x2912, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x3184 = x3183
(x3184)
}
val x3187 = x3186
(x3187)
}
val x3190 = x3189
(x3190)
}
val x3193 = x3192
(x3193)
} else {
val x3196 = x2913 < 6
val x3728 = if (x3196) {
val x3199 = x2913 < 5
val x3352 = if (x3199) {
val x3269 = if (x3199) {
val x3183 =  Basen4lbgbs0s1sbs0s1v_x3125(x2911, x2912, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x3205 = x3183
(x3205)
} else {
val x3266 =  Basen5lbgbs0s1sbs0s1v_x3208(x2911, x2912, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x3267 = x3266
(x3267)
}
val x3270 = x3269
(x3270)
} else {
val x3349 = if (x3196) {
val x3266 =  Basen5lbgbs0s1sbs0s1v_x3208(x2911, x2912, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x3276 = x3266
(x3276)
} else {
val x3346 =  Basen6lbgbs0s1sbs0s1v_x3279(x2911, x2912, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x3347 = x3346
(x3347)
}
val x3350 = x3349
(x3350)
}
val x3353 = x3352
(x3353)
} else {
val x3356 = x2913 < 7
val x3725 = if (x3356) {
val x3435 = if (x3356) {
val x3346 =  Basen6lbgbs0s1sbs0s1v_x3279(x2911, x2912, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x3362 = x3346
(x3362)
} else {
val x3432 =  Basen7lbgbs0s1sbs0s1v_x3365(x2911, x2912, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x3433 = x3432
(x3433)
}
val x3436 = x3435
(x3436)
} else {
val x3439 = x2913 < 8
val x3722 = if (x3439) {
val x3571 = if (x3439) {
val x3432 =  Basen7lbgbs0s1sbs0s1v_x3365(x2911, x2912, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x3445 = x3432
(x3445)
} else {
val x3568 =  Basen8lbgbs0s1sbs0s1v_x3448(x2911, x2912, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x3569 = x3568
(x3569)
}
val x3572 = x3571
(x3572)
} else {
val x3575 = x2913 < 9
val x3719 = if (x3575) {
val x3568 =  Basen8lbgbs0s1sbs0s1v_x3448(x2911, x2912, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x3579 = x3568
(x3579)
} else {
val x3716 =  DFTn9lbgbs0s1sbs0s1v_x3582(x2911, x2912, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x3717 = x3716
(x3717)
}
val x3720 = x3719
(x3720)
}
val x3723 = x3722
(x3723)
}
val x3726 = x3725
(x3726)
}
val x3729 = x3728
(x3729)
}
val x3732 = x3731
(x3732)
} else {
val x2925 = x2913 < 4
val x4630 = if (x2925) {
val x3773 =  F2nlbgbs0s1sbs0s1v_x3737(x2911, x2912, x2913, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x3774 = x3773
(x3774)
} else {
val x4627 =  DFT_CTnlbgbs0s1sbs0s1v_x3777(x2911, x2912, x2913, x2914, x2915, x2916, x2917, x2918, x2919, x2920, x2921)

val x4628 = x4627
(x4628)
}
val x4631 = x4630
(x4631)
}
val x4634 = x4633

 (x4634)
}
def Basen7lbgbs01s1sbs01s1v_x1730: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x1731 : SpiralS2.ComplexVector = helper._1
val x1732 : SpiralS2.ComplexVector = helper._2
val x1733 : Int = helper._3
val x1734 : Int = helper._4
val x1735 : Int = helper._5
val x1736 : Int = helper._6
val x1737 : Int = helper._7
val x1738 : Int = helper._8
val x1789 = (0 until x1733).foldLeft( x1731 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x1741 : SpiralS2.ComplexVector = helper._1
val x1742 : Int = helper._2
val x1745 = x1738 * x1742
val x1743 = x1736 * x1742
val x1746 = x1737 + x1745
val x1778 = x1746 + 2
val x1744 = x1735 + x1743
val x1755 = x1744 + 3
val x1756 = x1731(x1755)
val x1759 = x1746 + 3
val x1752 = x1746 + 1
val x1748 = x1744 + 1
val x1780 = x1759 + 2
val x1762 = x1752 + 3
val x1757 = x1748 + 3
val x1758 = x1731(x1757)
val x1760 = x1756 + x1758
val x1763 = x1756 - x1758
val x1761 = x1732.update(x1759,x1760)
val x1764 = x1761.update(x1762,x1763)
val x1779 = x1764(x1778)
val x1781 = x1764(x1780)
val x1782 = x1779 + x1781
val x1784 = x1779 - x1781
val x1783 = x1732.update(x1778,x1782)
val x1785 = x1783.update(x1780,x1784)
(x1785)
})
val x1790 = x1789

 (x1790)
}
def Basen8lbimbs0s1twb0s0s11v_x4284: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x4285 : SpiralS2.ComplexVector = helper._1
val x4286 : SpiralS2.ComplexVector = helper._2
val x4287 : Int = helper._3
val x4288 : Int = helper._4
val x4289 : Int = helper._5
val x4290 : Int = helper._6
val x4291 : Int = helper._7
val x4292 : Int = helper._8
val x4388 = (0 until x4287).foldLeft( x4285 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x4295 : SpiralS2.ComplexVector = helper._1
val x4296 : Int = helper._2
val x4376 = x4290 * 3
val x4297 = x4290 * 4
val x4298 = x4291 * x4296
val x4300 = x4290 * 2
val x4299 = x4289 + x4298
val x4377 = x4299 + x4376
val x4329 = x4299 + x4297
val x4331 = x4329 + x4290
val x4337 = x4329 + x4300
val x4339 = x4331 + x4300
val x4379 = x4329 + x4376
val x4352 = x4337 + x4290
val x4340 = x4285(x4339)
val x4338 = x4285(x4337)
val x4341 = x4338 + x4340
val x4343 = x4338 - x4340
val x4342 = x4286.update(x4337,x4341)
val x4344 = x4342.update(x4339,x4343)
val x4351 = x4344(x4331)
val x4353 = x4344(x4352)
val x4354 = x4351 + x4353
val x4356 = x4351 - x4353
val x4355 = x4286.update(x4331,x4354)
val x4357 = x4355.update(x4352,x4356)
val x4378 = x4357(x4377)
val x4380 = x4357(x4379)
val x4381 = x4378 + x4380
val x4383 = x4378 - x4380
val x4382 = x4286.update(x4377,x4381)
val x4384 = x4382.update(x4379,x4383)
(x4384)
})
val x4389 = x4388

 (x4389)
}
def Basen3lbimb0s0s11twb0s0s11v0_x4821: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x4822 : SpiralS2.ComplexVector = helper._1
val x4823 : SpiralS2.ComplexVector = helper._2
val x4824 : Int = helper._3
val x4825 : Int = helper._4
val x4826 : Int = helper._5
val x4841 = (0 until x4824).foldLeft( x4822 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x4829 : SpiralS2.ComplexVector = helper._1
val x4830 : Int = helper._2
val x4831 = x4822(x4830)
val x4832 = x4825 + x4830
val x4833 = x4822(x4832)
val x4834 = x4831 + x4833
val x4836 = x4831 - x4833
val x4835 = x4823.update(x4830,x4834)
val x4837 = x4835.update(x4832,x4836)
(x4837)
})
val x4842 = x4841

 (x4842)
}
def Basen3lbgbs01s1sbs01s1v_x1457: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x1458 : SpiralS2.ComplexVector = helper._1
val x1459 : SpiralS2.ComplexVector = helper._2
val x1460 : Int = helper._3
val x1461 : Int = helper._4
val x1462 : Int = helper._5
val x1463 : Int = helper._6
val x1464 : Int = helper._7
val x1465 : Int = helper._8
val x1487 = (0 until x1460).foldLeft( x1458 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x1468 : SpiralS2.ComplexVector = helper._1
val x1469 : Int = helper._2
val x1470 = x1463 * x1469
val x1471 = x1462 + x1470
val x1472 = x1458(x1471)
val x1476 = x1465 * x1469
val x1480 = x1464 + 1
val x1473 = x1462 + 1
val x1477 = x1464 + x1476
val x1481 = x1480 + x1476
val x1474 = x1473 + x1470
val x1475 = x1458(x1474)
val x1478 = x1472 + x1475
val x1482 = x1472 - x1475
val x1479 = x1459.update(x1477,x1478)
val x1483 = x1479.update(x1481,x1482)
(x1483)
})
val x1488 = x1487

 (x1488)
}
def DFTn9lbimbs0s11twb0s0s11v_x2724: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int))) =>{
val x2725 : SpiralS2.ComplexVector = helper._1
val x2726 : SpiralS2.ComplexVector = helper._2
val x2727 : Int = helper._3
val x2728 : Int = helper._4
val x2729 : Int = helper._5
val x2730 : Int = helper._6
val x2731 : Int = helper._7
val x2837 =  DFT_CTn9lbimbs0s11twb0s0s11v_x2732(x2725, x2726, x2727, x2728, x2729, x2730, x2731)

val x2838 = x2837

 (x2838)
}
def Basen0lbgb0s01s1sb0s01s1v0_x554: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x555 : SpiralS2.ComplexVector = helper._1
val x556 : SpiralS2.ComplexVector = helper._2
val x557 : Int = helper._3
val x558 : Int = helper._4
val x559 : Int = helper._5
val x577 = (0 until x557).foldLeft( x555 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x562 : SpiralS2.ComplexVector = helper._1
val x563 : Int = helper._2
val x568 = x559 * x563
val x564 = x558 * x563
val x571 = 1 + x568
val x565 = x555(x564)
val x566 = 1 + x564
val x567 = x555(x566)
val x569 = x565 + x567
val x572 = x565 - x567
val x570 = x556.update(x568,x569)
val x573 = x570.update(x571,x572)
(x573)
})
val x578 = x577

 (x578)
}
def F2nlbimbs0s1twb0s0s11v_x4545: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int))) =>{
val x4546 : SpiralS2.ComplexVector = helper._1
val x4547 : SpiralS2.ComplexVector = helper._2
val x4548 : Int = helper._3
val x4549 : Int = helper._4
val x4550 : Int = helper._5
val x4551 : Int = helper._6
val x4552 : Int = helper._7
val x4553 : Int = helper._8
val x4554 : Int = helper._9
val x4572 = (0 until x4549).foldLeft( x4546 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x4557 : SpiralS2.ComplexVector = helper._1
val x4558 : Int = helper._2
val x4562 = x4551 + x4552
val x4559 = x4553 * x4558
val x4560 = x4551 + x4559
val x4563 = x4562 + x4559
val x4561 = x4546(x4560)
val x4564 = x4546(x4563)
val x4565 = x4561 + x4564
val x4567 = x4561 - x4564
val x4566 = x4547.update(x4560,x4565)
val x4568 = x4566.update(x4563,x4567)
(x4568)
})
val x4573 = x4572

 (x4573)
}
def Basen1lbimbs0s11twb0s0s11v_x2196: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int))) =>{
val x2197 : SpiralS2.ComplexVector = helper._1
val x2198 : SpiralS2.ComplexVector = helper._2
val x2199 : Int = helper._3
val x2200 : Int = helper._4
val x2201 : Int = helper._5
val x2202 : Int = helper._6
val x2203 : Int = helper._7
val x2220 = (0 until x2199).foldLeft( x2197 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x2206 : SpiralS2.ComplexVector = helper._1
val x2207 : Int = helper._2
val x2210 = x2201 + x2202
val x2208 = x2201 + x2207
val x2211 = x2210 + x2207
val x2209 = x2197(x2208)
val x2212 = x2197(x2211)
val x2213 = x2209 + x2212
val x2215 = x2209 - x2212
val x2214 = x2198.update(x2208,x2213)
val x2216 = x2214.update(x2211,x2215)
(x2216)
})
val x2221 = x2220

 (x2221)
}
def DFTn9lbgb0s01s1sb0s01s1v0_x1114: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x1115 : SpiralS2.ComplexVector = helper._1
val x1116 : SpiralS2.ComplexVector = helper._2
val x1117 : Int = helper._3
val x1118 : Int = helper._4
val x1119 : Int = helper._5
val x1228 =  DFT_CTn9lbgb0s01s1sb0s01s1v0_x1120(x1115, x1116, x1117, x1118, x1119)

val x1229 = x1228

 (x1229)
}
def Basen5lbimbs0s1twb0s0s11v_x4068: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x4069 : SpiralS2.ComplexVector = helper._1
val x4070 : SpiralS2.ComplexVector = helper._2
val x4071 : Int = helper._3
val x4072 : Int = helper._4
val x4073 : Int = helper._5
val x4074 : Int = helper._6
val x4075 : Int = helper._7
val x4076 : Int = helper._8
val x4115 = (0 until x4071).foldLeft( x4069 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x4079 : SpiralS2.ComplexVector = helper._1
val x4080 : Int = helper._2
val x4082 = x4075 * x4080
val x4081 = x4074 * 2
val x4083 = x4073 + x4082
val x4085 = x4083 + x4074
val x4091 = x4083 + x4081
val x4093 = x4085 + x4081
val x4092 = x4069(x4091)
val x4106 = x4091 + x4074
val x4094 = x4069(x4093)
val x4095 = x4092 + x4094
val x4097 = x4092 - x4094
val x4096 = x4070.update(x4091,x4095)
val x4098 = x4096.update(x4093,x4097)
val x4105 = x4098(x4085)
val x4107 = x4098(x4106)
val x4108 = x4105 + x4107
val x4110 = x4105 - x4107
val x4109 = x4070.update(x4085,x4108)
val x4111 = x4109.update(x4106,x4110)
(x4111)
})
val x4116 = x4115

 (x4116)
}
def Basen7lbimb0s0s11twb0s0s11v0_x5058: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x5059 : SpiralS2.ComplexVector = helper._1
val x5060 : SpiralS2.ComplexVector = helper._2
val x5061 : Int = helper._3
val x5062 : Int = helper._4
val x5063 : Int = helper._5
val x5109 = (0 until x5061).foldLeft( x5059 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x5066 : SpiralS2.ComplexVector = helper._1
val x5067 : Int = helper._2
val x5068 = x5062 * 3
val x5070 = x5067 + x5062
val x5097 = x5062 * 2
val x5076 = x5067 + x5068
val x5077 = x5059(x5076)
val x5098 = x5067 + x5097
val x5100 = x5076 + x5097
val x5078 = x5070 + x5068
val x5079 = x5059(x5078)
val x5080 = x5077 + x5079
val x5082 = x5077 - x5079
val x5081 = x5060.update(x5076,x5080)
val x5083 = x5081.update(x5078,x5082)
val x5099 = x5083(x5098)
val x5101 = x5083(x5100)
val x5102 = x5099 + x5101
val x5104 = x5099 - x5101
val x5103 = x5060.update(x5098,x5102)
val x5105 = x5103.update(x5100,x5104)
(x5105)
})
val x5110 = x5109

 (x5110)
}
def Basen1lb1gb0s01s10sb0s01s10v0_x41: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector))) =>{
val x42 : SpiralS2.ComplexVector = helper._1
val x43 : SpiralS2.ComplexVector = helper._2
val x45 = x42(1)
val x44 = x42(0)
val x46 = x44 + x45
val x48 = x44 - x45
val x47 = x43.update(0,x46)
val x49 = x47.update(1,x48)

 (x49)
}
def Basen2lbimbs0s1twb0s0s11v_x3903: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x3904 : SpiralS2.ComplexVector = helper._1
val x3905 : SpiralS2.ComplexVector = helper._2
val x3906 : Int = helper._3
val x3907 : Int = helper._4
val x3908 : Int = helper._5
val x3909 : Int = helper._6
val x3910 : Int = helper._7
val x3911 : Int = helper._8
val x3929 = (0 until x3906).foldLeft( x3904 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x3914 : SpiralS2.ComplexVector = helper._1
val x3915 : Int = helper._2
val x3919 = x3908 + x3909
val x3916 = x3910 * x3915
val x3917 = x3908 + x3916
val x3920 = x3919 + x3916
val x3918 = x3904(x3917)
val x3921 = x3904(x3920)
val x3922 = x3918 + x3921
val x3924 = x3918 - x3921
val x3923 = x3905.update(x3917,x3922)
val x3925 = x3923.update(x3920,x3924)
(x3925)
})
val x3930 = x3929

 (x3930)
}
def Basen6lbgbs0s1sbs0s1v_x3279: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int))) =>{
val x3280 : SpiralS2.ComplexVector = helper._1
val x3281 : SpiralS2.ComplexVector = helper._2
val x3282 : Int = helper._3
val x3283 : Int = helper._4
val x3284 : Int = helper._5
val x3285 : Int = helper._6
val x3286 : Int = helper._7
val x3287 : Int = helper._8
val x3288 : Int = helper._9
val x3289 : Int = helper._10
val x3343 = (0 until x3282).foldLeft( x3280 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x3292 : SpiralS2.ComplexVector = helper._1
val x3293 : Int = helper._2
val x3297 = x3288 * 3
val x3294 = x3285 * 3
val x3298 = x3289 * x3293
val x3295 = x3286 * x3293
val x3331 = x3288 * 2
val x3299 = x3287 + x3298
val x3332 = x3299 + x3331
val x3312 = x3299 + x3297
val x3305 = x3299 + x3288
val x3296 = x3284 + x3295
val x3308 = x3296 + x3294
val x3301 = x3296 + x3285
val x3310 = x3301 + x3294
val x3311 = x3280(x3310)
val x3315 = x3305 + x3297
val x3334 = x3312 + x3331
val x3309 = x3280(x3308)
val x3313 = x3309 + x3311
val x3316 = x3309 - x3311
val x3314 = x3281.update(x3312,x3313)
val x3317 = x3314.update(x3315,x3316)
val x3333 = x3317(x3332)
val x3335 = x3317(x3334)
val x3336 = x3333 + x3335
val x3338 = x3333 - x3335
val x3337 = x3281.update(x3332,x3336)
val x3339 = x3337.update(x3334,x3338)
(x3339)
})
val x3344 = x3343

 (x3344)
}
def Basen0lbgbs0s1sbs0s1v_x2936: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int))) =>{
val x2937 : SpiralS2.ComplexVector = helper._1
val x2938 : SpiralS2.ComplexVector = helper._2
val x2939 : Int = helper._3
val x2940 : Int = helper._4
val x2941 : Int = helper._5
val x2942 : Int = helper._6
val x2943 : Int = helper._7
val x2944 : Int = helper._8
val x2945 : Int = helper._9
val x2946 : Int = helper._10
val x2968 = (0 until x2939).foldLeft( x2937 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x2949 : SpiralS2.ComplexVector = helper._1
val x2950 : Int = helper._2
val x2957 = x2946 * x2950
val x2954 = x2941 + x2942
val x2958 = x2944 + x2957
val x2951 = x2943 * x2950
val x2961 = x2944 + x2945
val x2952 = x2941 + x2951
val x2955 = x2954 + x2951
val x2962 = x2961 + x2957
val x2953 = x2937(x2952)
val x2956 = x2937(x2955)
val x2959 = x2953 + x2956
val x2963 = x2953 - x2956
val x2960 = x2938.update(x2958,x2959)
val x2964 = x2960.update(x2962,x2963)
(x2964)
})
val x2969 = x2968

 (x2969)
}
def DFT_CTn9lb1gb0s01s10sb0s01s10v0_x412: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector))) =>{
val x413 : SpiralS2.ComplexVector = helper._1
val x414 : SpiralS2.ComplexVector = helper._2
val x446 = x413(7)
val x445 = x413(6)
val x447 = x445 + x446
val x449 = x445 - x446
val x448 = x414.update(6,x447)
val x450 = x448.update(7,x449)
val x457 = x450(5)
val x458 = x450(7)
val x459 = x457 + x458
val x461 = x457 - x458
val x460 = x414.update(5,x459)
val x462 = x460.update(7,x461)
val x481 = x462(3)
val x482 = x462(7)
val x483 = x481 + x482
val x485 = x481 - x482
val x484 = x414.update(3,x483)
val x486 = x484.update(7,x485)

 (x486)
}
def Basen6lbimbs0s1twb0s0s11v_x4131: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x4132 : SpiralS2.ComplexVector = helper._1
val x4133 : SpiralS2.ComplexVector = helper._2
val x4134 : Int = helper._3
val x4135 : Int = helper._4
val x4136 : Int = helper._5
val x4137 : Int = helper._6
val x4138 : Int = helper._7
val x4139 : Int = helper._8
val x4187 = (0 until x4134).foldLeft( x4132 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x4142 : SpiralS2.ComplexVector = helper._1
val x4143 : Int = helper._2
val x4144 = x4137 * 3
val x4175 = x4137 * 2
val x4145 = x4138 * x4143
val x4146 = x4136 + x4145
val x4176 = x4146 + x4175
val x4154 = x4146 + x4144
val x4148 = x4146 + x4137
val x4155 = x4132(x4154)
val x4178 = x4154 + x4175
val x4156 = x4148 + x4144
val x4157 = x4132(x4156)
val x4158 = x4155 + x4157
val x4160 = x4155 - x4157
val x4159 = x4133.update(x4154,x4158)
val x4161 = x4159.update(x4156,x4160)
val x4177 = x4161(x4176)
val x4179 = x4161(x4178)
val x4180 = x4177 + x4179
val x4182 = x4177 - x4179
val x4181 = x4133.update(x4176,x4180)
val x4183 = x4181.update(x4178,x4182)
(x4183)
})
val x4188 = x4187

 (x4188)
}
def Basen2lb1gb0s01s10sb0s01s10v0_x64: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector))) =>{
val x65 : SpiralS2.ComplexVector = helper._1
val x66 : SpiralS2.ComplexVector = helper._2
val x67 = x65(0)
val x68 = x65(1)
val x69 = x67 + x68
val x71 = x67 - x68
val x70 = x66.update(0,x69)
val x72 = x70.update(1,x71)

 (x72)
}
def Basen1lbgb0s01s1sb0s01s1v0_x584: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x585 : SpiralS2.ComplexVector = helper._1
val x586 : SpiralS2.ComplexVector = helper._2
val x587 : Int = helper._3
val x588 : Int = helper._4
val x589 : Int = helper._5
val x607 = (0 until x587).foldLeft( x585 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x592 : SpiralS2.ComplexVector = helper._1
val x593 : Int = helper._2
val x598 = x589 * x593
val x594 = x588 * x593
val x601 = 1 + x598
val x595 = x585(x594)
val x596 = 1 + x594
val x597 = x585(x596)
val x599 = x595 + x597
val x602 = x595 - x597
val x600 = x586.update(x598,x599)
val x603 = x600.update(x601,x602)
(x603)
})
val x608 = x607

 (x608)
}
def Basen6lbgb0s01s1sb0s01s1v0_x843: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x844 : SpiralS2.ComplexVector = helper._1
val x845 : SpiralS2.ComplexVector = helper._2
val x846 : Int = helper._3
val x847 : Int = helper._4
val x848 : Int = helper._5
val x897 = (0 until x846).foldLeft( x844 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x851 : SpiralS2.ComplexVector = helper._1
val x852 : Int = helper._2
val x854 = x848 * x852
val x853 = x847 * x852
val x856 = x853 + 1
val x863 = x853 + 3
val x864 = x844(x863)
val x867 = x854 + 3
val x888 = x867 + 2
val x865 = x856 + 3
val x886 = x854 + 2
val x860 = x854 + 1
val x866 = x844(x865)
val x870 = x860 + 3
val x868 = x864 + x866
val x871 = x864 - x866
val x869 = x845.update(x867,x868)
val x872 = x869.update(x870,x871)
val x887 = x872(x886)
val x889 = x872(x888)
val x890 = x887 + x889
val x892 = x887 - x889
val x891 = x845.update(x886,x890)
val x893 = x891.update(x888,x892)
(x893)
})
val x898 = x897

 (x898)
}
def Basen4lbgbs01s1sbs01s1v_x1503: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x1504 : SpiralS2.ComplexVector = helper._1
val x1505 : SpiralS2.ComplexVector = helper._2
val x1506 : Int = helper._3
val x1507 : Int = helper._4
val x1508 : Int = helper._5
val x1509 : Int = helper._6
val x1510 : Int = helper._7
val x1511 : Int = helper._8
val x1554 = (0 until x1506).foldLeft( x1504 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x1514 : SpiralS2.ComplexVector = helper._1
val x1515 : Int = helper._2
val x1518 = x1511 * x1515
val x1516 = x1509 * x1515
val x1519 = x1510 + x1518
val x1517 = x1508 + x1516
val x1521 = x1517 + 1
val x1530 = x1521 + 2
val x1525 = x1519 + 1
val x1535 = x1525 + 2
val x1528 = x1517 + 2
val x1531 = x1504(x1530)
val x1532 = x1519 + 2
val x1529 = x1504(x1528)
val x1545 = x1532 + 1
val x1533 = x1529 + x1531
val x1536 = x1529 - x1531
val x1534 = x1505.update(x1532,x1533)
val x1537 = x1534.update(x1535,x1536)
val x1544 = x1537(x1525)
val x1546 = x1537(x1545)
val x1547 = x1544 + x1546
val x1549 = x1544 - x1546
val x1548 = x1505.update(x1525,x1547)
val x1550 = x1548.update(x1545,x1549)
(x1550)
})
val x1555 = x1554

 (x1555)
}
def Basen2lbgbs0s1sbs0s1v_x3023: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int))) =>{
val x3024 : SpiralS2.ComplexVector = helper._1
val x3025 : SpiralS2.ComplexVector = helper._2
val x3026 : Int = helper._3
val x3027 : Int = helper._4
val x3028 : Int = helper._5
val x3029 : Int = helper._6
val x3030 : Int = helper._7
val x3031 : Int = helper._8
val x3032 : Int = helper._9
val x3033 : Int = helper._10
val x3055 = (0 until x3026).foldLeft( x3024 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x3036 : SpiralS2.ComplexVector = helper._1
val x3037 : Int = helper._2
val x3044 = x3033 * x3037
val x3038 = x3030 * x3037
val x3048 = x3031 + x3032
val x3049 = x3048 + x3044
val x3041 = x3028 + x3029
val x3039 = x3028 + x3038
val x3045 = x3031 + x3044
val x3042 = x3041 + x3038
val x3040 = x3024(x3039)
val x3043 = x3024(x3042)
val x3046 = x3040 + x3043
val x3050 = x3040 - x3043
val x3047 = x3025.update(x3045,x3046)
val x3051 = x3047.update(x3049,x3050)
(x3051)
})
val x3056 = x3055

 (x3056)
}
def Basen3lbgbs0s1sbs0s1v_x3077: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int))) =>{
val x3078 : SpiralS2.ComplexVector = helper._1
val x3079 : SpiralS2.ComplexVector = helper._2
val x3080 : Int = helper._3
val x3081 : Int = helper._4
val x3082 : Int = helper._5
val x3083 : Int = helper._6
val x3084 : Int = helper._7
val x3085 : Int = helper._8
val x3086 : Int = helper._9
val x3087 : Int = helper._10
val x3109 = (0 until x3080).foldLeft( x3078 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x3090 : SpiralS2.ComplexVector = helper._1
val x3091 : Int = helper._2
val x3102 = x3085 + x3086
val x3095 = x3082 + x3083
val x3098 = x3087 * x3091
val x3092 = x3084 * x3091
val x3103 = x3102 + x3098
val x3099 = x3085 + x3098
val x3093 = x3082 + x3092
val x3096 = x3095 + x3092
val x3094 = x3078(x3093)
val x3097 = x3078(x3096)
val x3100 = x3094 + x3097
val x3104 = x3094 - x3097
val x3101 = x3079.update(x3099,x3100)
val x3105 = x3101.update(x3103,x3104)
(x3105)
})
val x3110 = x3109

 (x3110)
}
def Basen5lbgbs0s1sbs0s1v_x3208: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int))) =>{
val x3209 : SpiralS2.ComplexVector = helper._1
val x3210 : SpiralS2.ComplexVector = helper._2
val x3211 : Int = helper._3
val x3212 : Int = helper._4
val x3213 : Int = helper._5
val x3214 : Int = helper._6
val x3215 : Int = helper._7
val x3216 : Int = helper._8
val x3217 : Int = helper._9
val x3218 : Int = helper._10
val x3263 = (0 until x3211).foldLeft( x3209 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x3221 : SpiralS2.ComplexVector = helper._1
val x3222 : Int = helper._2
val x3226 = x3217 * 2
val x3223 = x3214 * 2
val x3224 = x3215 * x3222
val x3227 = x3218 * x3222
val x3225 = x3213 + x3224
val x3228 = x3216 + x3227
val x3230 = x3225 + x3214
val x3239 = x3230 + x3223
val x3237 = x3225 + x3223
val x3234 = x3228 + x3217
val x3240 = x3209(x3239)
val x3238 = x3209(x3237)
val x3242 = x3238 + x3240
val x3241 = x3228 + x3226
val x3243 = x3210.update(x3241,x3242)
val x3254 = x3241 + x3217
val x3244 = x3234 + x3226
val x3245 = x3238 - x3240
val x3246 = x3243.update(x3244,x3245)
val x3253 = x3246(x3234)
val x3255 = x3246(x3254)
val x3256 = x3253 + x3255
val x3258 = x3253 - x3255
val x3257 = x3210.update(x3234,x3256)
val x3259 = x3257.update(x3254,x3258)
(x3259)
})
val x3264 = x3263

 (x3264)
}
def Basen7lbgb0s01s1sb0s01s1v0_x919: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x920 : SpiralS2.ComplexVector = helper._1
val x921 : SpiralS2.ComplexVector = helper._2
val x922 : Int = helper._3
val x923 : Int = helper._4
val x924 : Int = helper._5
val x973 = (0 until x922).foldLeft( x920 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x927 : SpiralS2.ComplexVector = helper._1
val x928 : Int = helper._2
val x930 = x924 * x928
val x962 = x930 + 2
val x936 = x930 + 1
val x943 = x930 + 3
val x929 = x923 * x928
val x964 = x943 + 2
val x932 = x929 + 1
val x939 = x929 + 3
val x946 = x936 + 3
val x941 = x932 + 3
val x940 = x920(x939)
val x942 = x920(x941)
val x944 = x940 + x942
val x947 = x940 - x942
val x945 = x921.update(x943,x944)
val x948 = x945.update(x946,x947)
val x963 = x948(x962)
val x965 = x948(x964)
val x966 = x963 + x965
val x968 = x963 - x965
val x967 = x921.update(x962,x966)
val x969 = x967.update(x964,x968)
(x969)
})
val x974 = x973

 (x974)
}
def Basen0lbimb0s0s11twb0s0s11v0_x4716: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x4717 : SpiralS2.ComplexVector = helper._1
val x4718 : SpiralS2.ComplexVector = helper._2
val x4719 : Int = helper._3
val x4720 : Int = helper._4
val x4721 : Int = helper._5
val x4736 = (0 until x4719).foldLeft( x4717 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x4724 : SpiralS2.ComplexVector = helper._1
val x4725 : Int = helper._2
val x4726 = x4717(x4725)
val x4727 = x4720 + x4725
val x4728 = x4717(x4727)
val x4729 = x4726 + x4728
val x4731 = x4726 - x4728
val x4730 = x4718.update(x4725,x4729)
val x4732 = x4730.update(x4727,x4731)
(x4732)
})
val x4737 = x4736

 (x4737)
}
def DFT_CTnlbgbs01s1sbs01s1v_x2119: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int))) =>{
val x2120 : SpiralS2.ComplexVector = helper._1
val x2121 : SpiralS2.ComplexVector = helper._2
val x2122 : Int = helper._3
val x2123 : Int = helper._4
val x2124 : Int = helper._5
val x2125 : Int = helper._6
val x2126 : Int = helper._7
val x2127 : Int = helper._8
val x2128 : Int = helper._9
val x4661 = (0 until x2123).foldLeft( x2120 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x2133 : SpiralS2.ComplexVector = helper._1
val x2134 : Int = helper._2
val x2137 = x2128 * x2134
val x2129 = x2122 / 2
val x2135 = x2126 * x2134
val x2136 = x2125 + x2135
val x2130 = x2122 / x2129
val x2138 = x2127 + x2137
val x2140 =  DFTnlbgbs01s1sbs01s1v_x1298(x2120, x2121, x2129, x2130, x2134, x2136, x2129, x2138, x2129)

val x2141 = x2140
val x4656 =  DFTnlbimbs0s11twb0s0s11v_x2142(x2141, x2121, x2130, x2129, x2134, x2138, x2129, x2129)

val x4657 = x4656
(x4657)
})
val x4662 = x4661

 (x4662)
}
def Basen5lb1gb0s01s10sb0s01s10v0_x172: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector))) =>{
val x173 : SpiralS2.ComplexVector = helper._1
val x174 : SpiralS2.ComplexVector = helper._2
val x182 = x173(3)
val x181 = x173(2)
val x183 = x181 + x182
val x185 = x181 - x182
val x184 = x174.update(2,x183)
val x186 = x184.update(3,x185)
val x193 = x186(1)
val x194 = x186(3)
val x195 = x193 + x194
val x197 = x193 - x194
val x196 = x174.update(1,x195)
val x198 = x196.update(3,x197)

 (x198)
}
def DFTn9lbimb0s0s11twb0s0s11v0_x5244: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x5245 : SpiralS2.ComplexVector = helper._1
val x5246 : SpiralS2.ComplexVector = helper._2
val x5247 : Int = helper._3
val x5248 : Int = helper._4
val x5249 : Int = helper._5
val x5352 =  DFT_CTn9lbimb0s0s11twb0s0s11v0_x5250(x5245, x5246, x5247, x5248, x5249)

val x5353 = x5352

 (x5353)
}
def Basen8lb1gb0s01s10sb0s01s10v0_x318: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector))) =>{
val x319 : SpiralS2.ComplexVector = helper._1
val x320 : SpiralS2.ComplexVector = helper._2
val x352 = x319(7)
val x351 = x319(6)
val x353 = x351 + x352
val x355 = x351 - x352
val x354 = x320.update(6,x353)
val x356 = x354.update(7,x355)
val x363 = x356(5)
val x364 = x356(7)
val x365 = x363 + x364
val x367 = x363 - x364
val x366 = x320.update(5,x365)
val x368 = x366.update(7,x367)
val x387 = x368(3)
val x388 = x368(7)
val x389 = x387 + x388
val x391 = x387 - x388
val x390 = x320.update(3,x389)
val x392 = x390.update(7,x391)

 (x392)
}
def Basen8lbimbs0s11twb0s0s11v_x2605: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int))) =>{
val x2606 : SpiralS2.ComplexVector = helper._1
val x2607 : SpiralS2.ComplexVector = helper._2
val x2608 : Int = helper._3
val x2609 : Int = helper._4
val x2610 : Int = helper._5
val x2611 : Int = helper._6
val x2612 : Int = helper._7
val x2707 = (0 until x2608).foldLeft( x2606 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x2615 : SpiralS2.ComplexVector = helper._1
val x2616 : Int = helper._2
val x2618 = x2610 + x2616
val x2695 = x2611 * 3
val x2696 = x2618 + x2695
val x2617 = x2611 * 4
val x2619 = x2611 * 2
val x2648 = x2618 + x2617
val x2650 = x2648 + x2611
val x2656 = x2648 + x2619
val x2671 = x2656 + x2611
val x2698 = x2648 + x2695
val x2657 = x2606(x2656)
val x2658 = x2650 + x2619
val x2659 = x2606(x2658)
val x2660 = x2657 + x2659
val x2662 = x2657 - x2659
val x2661 = x2607.update(x2656,x2660)
val x2663 = x2661.update(x2658,x2662)
val x2670 = x2663(x2650)
val x2672 = x2663(x2671)
val x2673 = x2670 + x2672
val x2675 = x2670 - x2672
val x2674 = x2607.update(x2650,x2673)
val x2676 = x2674.update(x2671,x2675)
val x2697 = x2676(x2696)
val x2699 = x2676(x2698)
val x2700 = x2697 + x2699
val x2702 = x2697 - x2699
val x2701 = x2607.update(x2696,x2700)
val x2703 = x2701.update(x2698,x2702)
(x2703)
})
val x2708 = x2707

 (x2708)
}
def Basen6lb1gb0s01s10sb0s01s10v0_x213: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector))) =>{
val x214 : SpiralS2.ComplexVector = helper._1
val x215 : SpiralS2.ComplexVector = helper._2
val x222 = x214(3)
val x223 = x214(4)
val x224 = x222 + x223
val x226 = x222 - x223
val x225 = x215.update(3,x224)
val x227 = x225.update(4,x226)
val x240 = x227(2)
val x241 = x227(5)
val x242 = x240 + x241
val x244 = x240 - x241
val x243 = x215.update(2,x242)
val x245 = x243.update(5,x244)

 (x245)
}
def Basen6lbimb0s0s11twb0s0s11v0_x4985: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x4986 : SpiralS2.ComplexVector = helper._1
val x4987 : SpiralS2.ComplexVector = helper._2
val x4988 : Int = helper._3
val x4989 : Int = helper._4
val x4990 : Int = helper._5
val x5036 = (0 until x4988).foldLeft( x4986 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x4993 : SpiralS2.ComplexVector = helper._1
val x4994 : Int = helper._2
val x4995 = x4989 * 3
val x4997 = x4994 + x4989
val x5024 = x4989 * 2
val x5003 = x4994 + x4995
val x5005 = x4997 + x4995
val x5027 = x5003 + x5024
val x5006 = x4986(x5005)
val x5025 = x4994 + x5024
val x5004 = x4986(x5003)
val x5007 = x5004 + x5006
val x5009 = x5004 - x5006
val x5008 = x4987.update(x5003,x5007)
val x5010 = x5008.update(x5005,x5009)
val x5026 = x5010(x5025)
val x5028 = x5010(x5027)
val x5029 = x5026 + x5028
val x5031 = x5026 - x5028
val x5030 = x4987.update(x5025,x5029)
val x5032 = x5030.update(x5027,x5031)
(x5032)
})
val x5037 = x5036

 (x5037)
}
def Basen1lbimbs0s1twb0s0s11v_x3861: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x3862 : SpiralS2.ComplexVector = helper._1
val x3863 : SpiralS2.ComplexVector = helper._2
val x3864 : Int = helper._3
val x3865 : Int = helper._4
val x3866 : Int = helper._5
val x3867 : Int = helper._6
val x3868 : Int = helper._7
val x3869 : Int = helper._8
val x3887 = (0 until x3864).foldLeft( x3862 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x3872 : SpiralS2.ComplexVector = helper._1
val x3873 : Int = helper._2
val x3877 = x3866 + x3867
val x3874 = x3868 * x3873
val x3875 = x3866 + x3874
val x3878 = x3877 + x3874
val x3876 = x3862(x3875)
val x3879 = x3862(x3878)
val x3880 = x3876 + x3879
val x3882 = x3876 - x3879
val x3881 = x3863.update(x3875,x3880)
val x3883 = x3881.update(x3878,x3882)
(x3883)
})
val x3888 = x3887

 (x3888)
}
def Basen3lbgb0s01s1sb0s01s1v0_x668: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x669 : SpiralS2.ComplexVector = helper._1
val x670 : SpiralS2.ComplexVector = helper._2
val x671 : Int = helper._3
val x672 : Int = helper._4
val x673 : Int = helper._5
val x691 = (0 until x671).foldLeft( x669 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x676 : SpiralS2.ComplexVector = helper._1
val x677 : Int = helper._2
val x682 = x673 * x677
val x678 = x672 * x677
val x685 = 1 + x682
val x679 = x669(x678)
val x680 = 1 + x678
val x681 = x669(x680)
val x683 = x679 + x681
val x686 = x679 - x681
val x684 = x670.update(x682,x683)
val x687 = x684.update(x685,x686)
(x687)
})
val x692 = x691

 (x692)
}
def Basen7lb1gb0s01s10sb0s01s10v0_x267: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector))) =>{
val x268 : SpiralS2.ComplexVector = helper._1
val x269 : SpiralS2.ComplexVector = helper._2
val x276 = x268(3)
val x277 = x268(4)
val x278 = x276 + x277
val x280 = x276 - x277
val x279 = x269.update(3,x278)
val x281 = x279.update(4,x280)
val x294 = x281(2)
val x295 = x281(5)
val x296 = x294 + x295
val x298 = x294 - x295
val x297 = x269.update(2,x296)
val x299 = x297.update(5,x298)

 (x299)
}
def Basen7lbimbs0s1twb0s0s11v_x4209: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x4210 : SpiralS2.ComplexVector = helper._1
val x4211 : SpiralS2.ComplexVector = helper._2
val x4212 : Int = helper._3
val x4213 : Int = helper._4
val x4214 : Int = helper._5
val x4215 : Int = helper._6
val x4216 : Int = helper._7
val x4217 : Int = helper._8
val x4265 = (0 until x4212).foldLeft( x4210 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x4220 : SpiralS2.ComplexVector = helper._1
val x4221 : Int = helper._2
val x4222 = x4215 * 3
val x4253 = x4215 * 2
val x4223 = x4216 * x4221
val x4224 = x4214 + x4223
val x4254 = x4224 + x4253
val x4232 = x4224 + x4222
val x4226 = x4224 + x4215
val x4233 = x4210(x4232)
val x4256 = x4232 + x4253
val x4234 = x4226 + x4222
val x4235 = x4210(x4234)
val x4236 = x4233 + x4235
val x4238 = x4233 - x4235
val x4237 = x4211.update(x4232,x4236)
val x4239 = x4237.update(x4234,x4238)
val x4255 = x4239(x4254)
val x4257 = x4239(x4256)
val x4258 = x4255 + x4257
val x4260 = x4255 - x4257
val x4259 = x4211.update(x4254,x4258)
val x4261 = x4259.update(x4256,x4260)
(x4261)
})
val x4266 = x4265

 (x4266)
}
def DFTn9lbimbs0s1twb0s0s11v_x4405: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x4406 : SpiralS2.ComplexVector = helper._1
val x4407 : SpiralS2.ComplexVector = helper._2
val x4408 : Int = helper._3
val x4409 : Int = helper._4
val x4410 : Int = helper._5
val x4411 : Int = helper._6
val x4412 : Int = helper._7
val x4413 : Int = helper._8
val x4521 =  DFT_CTn9lbimbs0s1twb0s0s11v_x4414(x4406, x4407, x4408, x4409, x4410, x4411, x4412, x4413)

val x4522 = x4521

 (x4522)
}
def F2nlbgbs0s1sbs0s1v_x3737: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int,Int))) =>{
val x3738 : SpiralS2.ComplexVector = helper._1
val x3739 : SpiralS2.ComplexVector = helper._2
val x3740 : Int = helper._3
val x3741 : Int = helper._4
val x3742 : Int = helper._5
val x3743 : Int = helper._6
val x3744 : Int = helper._7
val x3745 : Int = helper._8
val x3746 : Int = helper._9
val x3747 : Int = helper._10
val x3748 : Int = helper._11
val x3770 = (0 until x3741).foldLeft( x3738 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x3751 : SpiralS2.ComplexVector = helper._1
val x3752 : Int = helper._2
val x3756 = x3743 + x3744
val x3763 = x3746 + x3747
val x3759 = x3748 * x3752
val x3753 = x3745 * x3752
val x3760 = x3746 + x3759
val x3764 = x3763 + x3759
val x3754 = x3743 + x3753
val x3757 = x3756 + x3753
val x3755 = x3738(x3754)
val x3758 = x3738(x3757)
val x3761 = x3755 + x3758
val x3765 = x3755 - x3758
val x3762 = x3739.update(x3760,x3761)
val x3766 = x3762.update(x3764,x3765)
(x3766)
})
val x3771 = x3770

 (x3771)
}
def DFT_CTn9lbimb0s0s11twb0s0s11v0_x5250: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x5251 : SpiralS2.ComplexVector = helper._1
val x5252 : SpiralS2.ComplexVector = helper._2
val x5253 : Int = helper._3
val x5254 : Int = helper._4
val x5255 : Int = helper._5
val x5349 = (0 until x5253).foldLeft( x5251 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x5258 : SpiralS2.ComplexVector = helper._1
val x5259 : Int = helper._2
val x5337 = x5254 * 3
val x5338 = x5259 + x5337
val x5260 = x5254 * 4
val x5261 = x5254 * 2
val x5290 = x5259 + x5260
val x5292 = x5290 + x5254
val x5298 = x5290 + x5261
val x5313 = x5298 + x5254
val x5340 = x5290 + x5337
val x5299 = x5251(x5298)
val x5300 = x5292 + x5261
val x5301 = x5251(x5300)
val x5302 = x5299 + x5301
val x5304 = x5299 - x5301
val x5303 = x5252.update(x5298,x5302)
val x5305 = x5303.update(x5300,x5304)
val x5312 = x5305(x5292)
val x5314 = x5305(x5313)
val x5315 = x5312 + x5314
val x5317 = x5312 - x5314
val x5316 = x5252.update(x5292,x5315)
val x5318 = x5316.update(x5313,x5317)
val x5339 = x5318(x5338)
val x5341 = x5318(x5340)
val x5342 = x5339 + x5341
val x5344 = x5339 - x5341
val x5343 = x5252.update(x5338,x5342)
val x5345 = x5343.update(x5340,x5344)
(x5345)
})
val x5350 = x5349

 (x5350)
}
def Basen4lbgb0s01s1sb0s01s1v0_x707: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x708 : SpiralS2.ComplexVector = helper._1
val x709 : SpiralS2.ComplexVector = helper._2
val x710 : Int = helper._3
val x711 : Int = helper._4
val x712 : Int = helper._5
val x753 = (0 until x710).foldLeft( x708 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x715 : SpiralS2.ComplexVector = helper._1
val x716 : Int = helper._2
val x718 = x712 * x716
val x717 = x711 * x716
val x724 = x718 + 1
val x734 = x724 + 2
val x727 = x717 + 2
val x731 = x718 + 2
val x720 = x717 + 1
val x728 = x708(x727)
val x744 = x731 + 1
val x729 = x720 + 2
val x730 = x708(x729)
val x732 = x728 + x730
val x735 = x728 - x730
val x733 = x709.update(x731,x732)
val x736 = x733.update(x734,x735)
val x743 = x736(x724)
val x745 = x736(x744)
val x746 = x743 + x745
val x748 = x743 - x745
val x747 = x709.update(x724,x746)
val x749 = x747.update(x744,x748)
(x749)
})
val x754 = x753

 (x754)
}
def DFT_CTn9lbimbs0s11twb0s0s11v_x2732: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int))) =>{
val x2733 : SpiralS2.ComplexVector = helper._1
val x2734 : SpiralS2.ComplexVector = helper._2
val x2735 : Int = helper._3
val x2736 : Int = helper._4
val x2737 : Int = helper._5
val x2738 : Int = helper._6
val x2739 : Int = helper._7
val x2834 = (0 until x2735).foldLeft( x2733 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x2742 : SpiralS2.ComplexVector = helper._1
val x2743 : Int = helper._2
val x2822 = x2738 * 3
val x2744 = x2738 * 4
val x2746 = x2738 * 2
val x2745 = x2737 + x2743
val x2823 = x2745 + x2822
val x2775 = x2745 + x2744
val x2777 = x2775 + x2738
val x2783 = x2775 + x2746
val x2825 = x2775 + x2822
val x2798 = x2783 + x2738
val x2785 = x2777 + x2746
val x2784 = x2733(x2783)
val x2786 = x2733(x2785)
val x2787 = x2784 + x2786
val x2789 = x2784 - x2786
val x2788 = x2734.update(x2783,x2787)
val x2790 = x2788.update(x2785,x2789)
val x2797 = x2790(x2777)
val x2799 = x2790(x2798)
val x2800 = x2797 + x2799
val x2802 = x2797 - x2799
val x2801 = x2734.update(x2777,x2800)
val x2803 = x2801.update(x2798,x2802)
val x2824 = x2803(x2823)
val x2826 = x2803(x2825)
val x2827 = x2824 + x2826
val x2829 = x2824 - x2826
val x2828 = x2734.update(x2823,x2827)
val x2830 = x2828.update(x2825,x2829)
(x2830)
})
val x2835 = x2834

 (x2835)
}
def F2nlbgb0s01s1sb0s01s1v0_x1252: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int))) =>{
val x1253 : SpiralS2.ComplexVector = helper._1
val x1254 : SpiralS2.ComplexVector = helper._2
val x1255 : Int = helper._3
val x1256 : Int = helper._4
val x1257 : Int = helper._5
val x1258 : Int = helper._6
val x1276 = (0 until x1256).foldLeft( x1253 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x1261 : SpiralS2.ComplexVector = helper._1
val x1262 : Int = helper._2
val x1267 = x1258 * x1262
val x1263 = x1257 * x1262
val x1270 = 1 + x1267
val x1264 = x1253(x1263)
val x1265 = 1 + x1263
val x1266 = x1253(x1265)
val x1268 = x1264 + x1266
val x1271 = x1264 - x1266
val x1269 = x1254.update(x1267,x1268)
val x1272 = x1269.update(x1270,x1271)
(x1272)
})
val x1277 = x1276

 (x1277)
}
def DFTn9lb1gb0s01s10sb0s01s10v0_x409: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector))) =>{
val x410 : SpiralS2.ComplexVector = helper._1
val x411 : SpiralS2.ComplexVector = helper._2
val x488 =  DFT_CTn9lb1gb0s01s10sb0s01s10v0_x412(x410, x411)

val x489 = x488

 (x489)
}
def Basen8lbgbs0s1sbs0s1v_x3448: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int))) =>{
val x3449 : SpiralS2.ComplexVector = helper._1
val x3450 : SpiralS2.ComplexVector = helper._2
val x3451 : Int = helper._3
val x3452 : Int = helper._4
val x3453 : Int = helper._5
val x3454 : Int = helper._6
val x3455 : Int = helper._7
val x3456 : Int = helper._8
val x3457 : Int = helper._9
val x3458 : Int = helper._10
val x3565 = (0 until x3451).foldLeft( x3449 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x3461 : SpiralS2.ComplexVector = helper._1
val x3462 : Int = helper._2
val x3466 = x3457 * 4
val x3463 = x3454 * 4
val x3467 = x3458 * x3462
val x3468 = x3456 + x3467
val x3503 = x3468 + x3466
val x3509 = x3503 + x3457
val x3470 = x3457 * 2
val x3519 = x3509 + x3470
val x3516 = x3503 + x3470
val x3529 = x3516 + x3457
val x3464 = x3455 * x3462
val x3469 = x3454 * 2
val x3553 = x3457 * 3
val x3465 = x3453 + x3464
val x3554 = x3468 + x3553
val x3556 = x3503 + x3553
val x3502 = x3465 + x3463
val x3512 = x3502 + x3469
val x3505 = x3502 + x3454
val x3513 = x3449(x3512)
val x3514 = x3505 + x3469
val x3515 = x3449(x3514)
val x3517 = x3513 + x3515
val x3520 = x3513 - x3515
val x3518 = x3450.update(x3516,x3517)
val x3521 = x3518.update(x3519,x3520)
val x3528 = x3521(x3509)
val x3530 = x3521(x3529)
val x3531 = x3528 + x3530
val x3533 = x3528 - x3530
val x3532 = x3450.update(x3509,x3531)
val x3534 = x3532.update(x3529,x3533)
val x3555 = x3534(x3554)
val x3557 = x3534(x3556)
val x3558 = x3555 + x3557
val x3560 = x3555 - x3557
val x3559 = x3450.update(x3554,x3558)
val x3561 = x3559.update(x3556,x3560)
(x3561)
})
val x3566 = x3565

 (x3566)
}
def Basen0lbimbs0s11twb0s0s11v_x2165: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int))) =>{
val x2166 : SpiralS2.ComplexVector = helper._1
val x2167 : SpiralS2.ComplexVector = helper._2
val x2168 : Int = helper._3
val x2169 : Int = helper._4
val x2170 : Int = helper._5
val x2171 : Int = helper._6
val x2172 : Int = helper._7
val x2189 = (0 until x2168).foldLeft( x2166 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x2175 : SpiralS2.ComplexVector = helper._1
val x2176 : Int = helper._2
val x2179 = x2170 + x2171
val x2177 = x2170 + x2176
val x2180 = x2179 + x2176
val x2178 = x2166(x2177)
val x2181 = x2166(x2180)
val x2182 = x2178 + x2181
val x2184 = x2178 - x2181
val x2183 = x2167.update(x2177,x2182)
val x2185 = x2183.update(x2180,x2184)
(x2185)
})
val x2190 = x2189

 (x2190)
}
def Basen0lbimbs0s1twb0s0s11v_x3828: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x3829 : SpiralS2.ComplexVector = helper._1
val x3830 : SpiralS2.ComplexVector = helper._2
val x3831 : Int = helper._3
val x3832 : Int = helper._4
val x3833 : Int = helper._5
val x3834 : Int = helper._6
val x3835 : Int = helper._7
val x3836 : Int = helper._8
val x3854 = (0 until x3831).foldLeft( x3829 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x3839 : SpiralS2.ComplexVector = helper._1
val x3840 : Int = helper._2
val x3844 = x3833 + x3834
val x3841 = x3835 * x3840
val x3842 = x3833 + x3841
val x3845 = x3844 + x3841
val x3843 = x3829(x3842)
val x3846 = x3829(x3845)
val x3847 = x3843 + x3846
val x3849 = x3843 - x3846
val x3848 = x3830.update(x3842,x3847)
val x3850 = x3848.update(x3845,x3849)
(x3850)
})
val x3855 = x3854

 (x3855)
}
def Basen4lbimbs0s11twb0s0s11v_x2322: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int))) =>{
val x2323 : SpiralS2.ComplexVector = helper._1
val x2324 : SpiralS2.ComplexVector = helper._2
val x2325 : Int = helper._3
val x2326 : Int = helper._4
val x2327 : Int = helper._5
val x2328 : Int = helper._6
val x2329 : Int = helper._7
val x2367 = (0 until x2325).foldLeft( x2323 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x2332 : SpiralS2.ComplexVector = helper._1
val x2333 : Int = helper._2
val x2335 = x2327 + x2333
val x2334 = x2328 * 2
val x2337 = x2335 + x2328
val x2343 = x2335 + x2334
val x2345 = x2337 + x2334
val x2344 = x2323(x2343)
val x2358 = x2343 + x2328
val x2346 = x2323(x2345)
val x2347 = x2344 + x2346
val x2349 = x2344 - x2346
val x2348 = x2324.update(x2343,x2347)
val x2350 = x2348.update(x2345,x2349)
val x2357 = x2350(x2337)
val x2359 = x2350(x2358)
val x2360 = x2357 + x2359
val x2362 = x2357 - x2359
val x2361 = x2324.update(x2337,x2360)
val x2363 = x2361.update(x2358,x2362)
(x2363)
})
val x2368 = x2367

 (x2368)
}
def Basen1lbgbs0s1sbs0s1v_x2975: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int))) =>{
val x2976 : SpiralS2.ComplexVector = helper._1
val x2977 : SpiralS2.ComplexVector = helper._2
val x2978 : Int = helper._3
val x2979 : Int = helper._4
val x2980 : Int = helper._5
val x2981 : Int = helper._6
val x2982 : Int = helper._7
val x2983 : Int = helper._8
val x2984 : Int = helper._9
val x2985 : Int = helper._10
val x3007 = (0 until x2978).foldLeft( x2976 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x2988 : SpiralS2.ComplexVector = helper._1
val x2989 : Int = helper._2
val x2996 = x2985 * x2989
val x2990 = x2982 * x2989
val x3000 = x2983 + x2984
val x3001 = x3000 + x2996
val x2991 = x2980 + x2990
val x2997 = x2983 + x2996
val x2993 = x2980 + x2981
val x2992 = x2976(x2991)
val x2994 = x2993 + x2990
val x2995 = x2976(x2994)
val x2998 = x2992 + x2995
val x3002 = x2992 - x2995
val x2999 = x2977.update(x2997,x2998)
val x3003 = x2999.update(x3001,x3002)
(x3003)
})
val x3008 = x3007

 (x3008)
}
def DFTnlbimb0s0s11twb0s0s11v0_x4695: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int))) =>{
val x4696 : SpiralS2.ComplexVector = helper._1
val x4697 : SpiralS2.ComplexVector = helper._2
val x4698 : Int = helper._3
val x4699 : Int = helper._4
val x4700 : Int = helper._5
val x4701 : Int = helper._6
val x4702 = x4698 < 10
val x5436 = if (x4702) {
val x4705 = x4698 < 4
val x5370 = if (x4705) {
val x4708 = x4698 < 2
val x4911 = if (x4708) {
val x4711 = x4698 < 1
val x4808 = if (x4711) {
val x4769 = if (x4711) {
val x4739 =  Basen0lbimb0s0s11twb0s0s11v0_x4716(x4696, x4697, x4699, x4700, x4701)

val x4740 = x4739
(x4740)
} else {
val x4766 =  Basen1lbimb0s0s11twb0s0s11v0_x4743(x4696, x4697, x4699, x4700, x4701)

val x4767 = x4766
(x4767)
}
val x4770 = x4769
(x4770)
} else {
val x4805 = if (x4708) {
val x4766 =  Basen1lbimb0s0s11twb0s0s11v0_x4743(x4696, x4697, x4699, x4700, x4701)

val x4776 = x4766
(x4776)
} else {
val x4802 =  Basen2lbimb0s0s11twb0s0s11v0_x4779(x4696, x4697, x4699, x4700, x4701)

val x4803 = x4802
(x4803)
}
val x4806 = x4805
(x4806)
}
val x4809 = x4808
(x4809)
} else {
val x4812 = x4698 < 3
val x4908 = if (x4812) {
val x4847 = if (x4812) {
val x4802 =  Basen2lbimb0s0s11twb0s0s11v0_x4779(x4696, x4697, x4699, x4700, x4701)

val x4818 = x4802
(x4818)
} else {
val x4844 =  Basen3lbimb0s0s11twb0s0s11v0_x4821(x4696, x4697, x4699, x4700, x4701)

val x4845 = x4844
(x4845)
}
val x4848 = x4847
(x4848)
} else {
val x4905 = if (x4705) {
val x4844 =  Basen3lbimb0s0s11twb0s0s11v0_x4821(x4696, x4697, x4699, x4700, x4701)

val x4854 = x4844
(x4854)
} else {
val x4902 =  Basen4lbimb0s0s11twb0s0s11v0_x4857(x4696, x4697, x4699, x4700, x4701)

val x4903 = x4902
(x4903)
}
val x4906 = x4905
(x4906)
}
val x4909 = x4908
(x4909)
}
val x4912 = x4911
(x4912)
} else {
val x4915 = x4698 < 6
val x5367 = if (x4915) {
val x4918 = x4698 < 5
val x5045 = if (x4918) {
val x4975 = if (x4918) {
val x4902 =  Basen4lbimb0s0s11twb0s0s11v0_x4857(x4696, x4697, x4699, x4700, x4701)

val x4924 = x4902
(x4924)
} else {
val x4972 =  Basen5lbimb0s0s11twb0s0s11v0_x4927(x4696, x4697, x4699, x4700, x4701)

val x4973 = x4972
(x4973)
}
val x4976 = x4975
(x4976)
} else {
val x5042 = if (x4915) {
val x4972 =  Basen5lbimb0s0s11twb0s0s11v0_x4927(x4696, x4697, x4699, x4700, x4701)

val x4982 = x4972
(x4982)
} else {
val x5039 =  Basen6lbimb0s0s11twb0s0s11v0_x4985(x4696, x4697, x4699, x4700, x4701)

val x5040 = x5039
(x5040)
}
val x5043 = x5042
(x5043)
}
val x5046 = x5045
(x5046)
} else {
val x5049 = x4698 < 7
val x5364 = if (x5049) {
val x5115 = if (x5049) {
val x5039 =  Basen6lbimb0s0s11twb0s0s11v0_x4985(x4696, x4697, x4699, x4700, x4701)

val x5055 = x5039
(x5055)
} else {
val x5112 =  Basen7lbimb0s0s11twb0s0s11v0_x5058(x4696, x4697, x4699, x4700, x4701)

val x5113 = x5112
(x5113)
}
val x5116 = x5115
(x5116)
} else {
val x5119 = x4698 < 8
val x5361 = if (x5119) {
val x5233 = if (x5119) {
val x5112 =  Basen7lbimb0s0s11twb0s0s11v0_x5058(x4696, x4697, x4699, x4700, x4701)

val x5125 = x5112
(x5125)
} else {
val x5230 =  Basen8lbimb0s0s11twb0s0s11v0_x5128(x4696, x4697, x4699, x4700, x4701)

val x5231 = x5230
(x5231)
}
val x5234 = x5233
(x5234)
} else {
val x5237 = x4698 < 9
val x5358 = if (x5237) {
val x5230 =  Basen8lbimb0s0s11twb0s0s11v0_x5128(x4696, x4697, x4699, x4700, x4701)

val x5241 = x5230
(x5241)
} else {
val x5355 =  DFTn9lbimb0s0s11twb0s0s11v0_x5244(x4696, x4697, x4699, x4700, x4701)

val x5356 = x5355
(x5356)
}
val x5359 = x5358
(x5359)
}
val x5362 = x5361
(x5362)
}
val x5365 = x5364
(x5365)
}
val x5368 = x5367
(x5368)
}
val x5371 = x5370
(x5371)
} else {
val x4705 = x4698 < 4
val x5433 = if (x4705) {
val x5400 =  F2nlbimb0s0s11twb0s0s11v0_x5376(x4696, x4697, x4698, x4699, x4700, x4701)

val x5401 = x5400
(x5401)
} else {
val x5430 =  DFT_CTnlbimb0s0s11twb0s0s11v0_x5404(x4696, x4697, x4698, x4699, x4700, x4701)

val x5431 = x5430
(x5431)
}
val x5434 = x5433
(x5434)
}
val x5437 = x5436

 (x5437)
}
def DFT_CTn9lbimbs0s1twb0s0s11v_x4414: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x4415 : SpiralS2.ComplexVector = helper._1
val x4416 : SpiralS2.ComplexVector = helper._2
val x4417 : Int = helper._3
val x4418 : Int = helper._4
val x4419 : Int = helper._5
val x4420 : Int = helper._6
val x4421 : Int = helper._7
val x4422 : Int = helper._8
val x4518 = (0 until x4417).foldLeft( x4415 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x4425 : SpiralS2.ComplexVector = helper._1
val x4426 : Int = helper._2
val x4428 = x4421 * x4426
val x4429 = x4419 + x4428
val x4427 = x4420 * 4
val x4506 = x4420 * 3
val x4430 = x4420 * 2
val x4459 = x4429 + x4427
val x4467 = x4459 + x4430
val x4507 = x4429 + x4506
val x4461 = x4459 + x4420
val x4482 = x4467 + x4420
val x4509 = x4459 + x4506
val x4469 = x4461 + x4430
val x4468 = x4415(x4467)
val x4470 = x4415(x4469)
val x4471 = x4468 + x4470
val x4473 = x4468 - x4470
val x4472 = x4416.update(x4467,x4471)
val x4474 = x4472.update(x4469,x4473)
val x4481 = x4474(x4461)
val x4483 = x4474(x4482)
val x4484 = x4481 + x4483
val x4486 = x4481 - x4483
val x4485 = x4416.update(x4461,x4484)
val x4487 = x4485.update(x4482,x4486)
val x4508 = x4487(x4507)
val x4510 = x4487(x4509)
val x4511 = x4508 + x4510
val x4513 = x4508 - x4510
val x4512 = x4416.update(x4507,x4511)
val x4514 = x4512.update(x4509,x4513)
(x4514)
})
val x4519 = x4518

 (x4519)
}
def Basen6lbgbs01s1sbs01s1v_x1649: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x1650 : SpiralS2.ComplexVector = helper._1
val x1651 : SpiralS2.ComplexVector = helper._2
val x1652 : Int = helper._3
val x1653 : Int = helper._4
val x1654 : Int = helper._5
val x1655 : Int = helper._6
val x1656 : Int = helper._7
val x1657 : Int = helper._8
val x1708 = (0 until x1652).foldLeft( x1650 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x1660 : SpiralS2.ComplexVector = helper._1
val x1661 : Int = helper._2
val x1664 = x1657 * x1661
val x1662 = x1655 * x1661
val x1665 = x1656 + x1664
val x1671 = x1665 + 1
val x1697 = x1665 + 2
val x1663 = x1654 + x1662
val x1667 = x1663 + 1
val x1676 = x1667 + 3
val x1677 = x1650(x1676)
val x1674 = x1663 + 3
val x1678 = x1665 + 3
val x1681 = x1671 + 3
val x1675 = x1650(x1674)
val x1699 = x1678 + 2
val x1679 = x1675 + x1677
val x1682 = x1675 - x1677
val x1680 = x1651.update(x1678,x1679)
val x1683 = x1680.update(x1681,x1682)
val x1698 = x1683(x1697)
val x1700 = x1683(x1699)
val x1701 = x1698 + x1700
val x1703 = x1698 - x1700
val x1702 = x1651.update(x1697,x1701)
val x1704 = x1702.update(x1699,x1703)
(x1704)
})
val x1709 = x1708

 (x1709)
}
def DFT_CTn9lbgbs0s1sbs0s1v_x3593: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int,Int))) =>{
val x3594 : SpiralS2.ComplexVector = helper._1
val x3595 : SpiralS2.ComplexVector = helper._2
val x3596 : Int = helper._3
val x3597 : Int = helper._4
val x3598 : Int = helper._5
val x3599 : Int = helper._6
val x3600 : Int = helper._7
val x3601 : Int = helper._8
val x3602 : Int = helper._9
val x3603 : Int = helper._10
val x3710 = (0 until x3596).foldLeft( x3594 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x3606 : SpiralS2.ComplexVector = helper._1
val x3607 : Int = helper._2
val x3698 = x3602 * 3
val x3612 = x3603 * x3607
val x3609 = x3600 * x3607
val x3610 = x3598 + x3609
val x3611 = x3602 * 4
val x3608 = x3599 * 4
val x3647 = x3610 + x3608
val x3615 = x3602 * 2
val x3650 = x3647 + x3599
val x3614 = x3599 * 2
val x3613 = x3601 + x3612
val x3648 = x3613 + x3611
val x3661 = x3648 + x3615
val x3657 = x3647 + x3614
val x3699 = x3613 + x3698
val x3654 = x3648 + x3602
val x3664 = x3654 + x3615
val x3674 = x3661 + x3602
val x3659 = x3650 + x3614
val x3658 = x3594(x3657)
val x3701 = x3648 + x3698
val x3660 = x3594(x3659)
val x3662 = x3658 + x3660
val x3665 = x3658 - x3660
val x3663 = x3595.update(x3661,x3662)
val x3666 = x3663.update(x3664,x3665)
val x3673 = x3666(x3654)
val x3675 = x3666(x3674)
val x3676 = x3673 + x3675
val x3678 = x3673 - x3675
val x3677 = x3595.update(x3654,x3676)
val x3679 = x3677.update(x3674,x3678)
val x3700 = x3679(x3699)
val x3702 = x3679(x3701)
val x3703 = x3700 + x3702
val x3705 = x3700 - x3702
val x3704 = x3595.update(x3699,x3703)
val x3706 = x3704.update(x3701,x3705)
(x3706)
})
val x3711 = x3710

 (x3711)
}
def Basen7lbimbs0s11twb0s0s11v_x2532: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int))) =>{
val x2533 : SpiralS2.ComplexVector = helper._1
val x2534 : SpiralS2.ComplexVector = helper._2
val x2535 : Int = helper._3
val x2536 : Int = helper._4
val x2537 : Int = helper._5
val x2538 : Int = helper._6
val x2539 : Int = helper._7
val x2586 = (0 until x2535).foldLeft( x2533 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x2542 : SpiralS2.ComplexVector = helper._1
val x2543 : Int = helper._2
val x2544 = x2538 * 3
val x2574 = x2538 * 2
val x2545 = x2537 + x2543
val x2575 = x2545 + x2574
val x2553 = x2545 + x2544
val x2547 = x2545 + x2538
val x2554 = x2533(x2553)
val x2577 = x2553 + x2574
val x2555 = x2547 + x2544
val x2556 = x2533(x2555)
val x2557 = x2554 + x2556
val x2559 = x2554 - x2556
val x2558 = x2534.update(x2553,x2557)
val x2560 = x2558.update(x2555,x2559)
val x2576 = x2560(x2575)
val x2578 = x2560(x2577)
val x2579 = x2576 + x2578
val x2581 = x2576 - x2578
val x2580 = x2534.update(x2575,x2579)
val x2582 = x2580.update(x2577,x2581)
(x2582)
})
val x2587 = x2586

 (x2587)
}
def Basen0lb1gb0s01s10sb0s01s10v0_x26: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector))) =>{
val x27 : SpiralS2.ComplexVector = helper._1
val x28 : SpiralS2.ComplexVector = helper._2
val x30 = x27(0)
val x31 = x27(1)
val x32 = x30 + x31
val x34 = x30 - x31
val x33 = x28.update(0,x32)
val x35 = x33.update(1,x34)

 (x35)
}
def Basen5lbgb0s01s1sb0s01s1v0_x781: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x782 : SpiralS2.ComplexVector = helper._1
val x783 : SpiralS2.ComplexVector = helper._2
val x784 : Int = helper._3
val x785 : Int = helper._4
val x786 : Int = helper._5
val x827 = (0 until x784).foldLeft( x782 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x789 : SpiralS2.ComplexVector = helper._1
val x790 : Int = helper._2
val x792 = x786 * x790
val x791 = x785 * x790
val x798 = x792 + 1
val x805 = x792 + 2
val x794 = x791 + 1
val x808 = x798 + 2
val x818 = x805 + 1
val x801 = x791 + 2
val x803 = x794 + 2
val x802 = x782(x801)
val x804 = x782(x803)
val x806 = x802 + x804
val x809 = x802 - x804
val x807 = x783.update(x805,x806)
val x810 = x807.update(x808,x809)
val x817 = x810(x798)
val x819 = x810(x818)
val x820 = x817 + x819
val x822 = x817 - x819
val x821 = x783.update(x798,x820)
val x823 = x821.update(x818,x822)
(x823)
})
val x828 = x827

 (x828)
}
def F2nlbgbs01s1sbs01s1v_x2081: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int))) =>{
val x2082 : SpiralS2.ComplexVector = helper._1
val x2083 : SpiralS2.ComplexVector = helper._2
val x2084 : Int = helper._3
val x2085 : Int = helper._4
val x2086 : Int = helper._5
val x2087 : Int = helper._6
val x2088 : Int = helper._7
val x2089 : Int = helper._8
val x2090 : Int = helper._9
val x2112 = (0 until x2085).foldLeft( x2082 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x2093 : SpiralS2.ComplexVector = helper._1
val x2094 : Int = helper._2
val x2105 = x2089 + 1
val x2101 = x2090 * x2094
val x2098 = x2087 + 1
val x2095 = x2088 * x2094
val x2099 = x2098 + x2095
val x2100 = x2082(x2099)
val x2096 = x2087 + x2095
val x2102 = x2089 + x2101
val x2106 = x2105 + x2101
val x2097 = x2082(x2096)
val x2103 = x2097 + x2100
val x2107 = x2097 - x2100
val x2104 = x2083.update(x2102,x2103)
val x2108 = x2104.update(x2106,x2107)
(x2108)
})
val x2113 = x2112

 (x2113)
}
def Basen4lbimbs0s1twb0s0s11v_x3993: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x3994 : SpiralS2.ComplexVector = helper._1
val x3995 : SpiralS2.ComplexVector = helper._2
val x3996 : Int = helper._3
val x3997 : Int = helper._4
val x3998 : Int = helper._5
val x3999 : Int = helper._6
val x4000 : Int = helper._7
val x4001 : Int = helper._8
val x4040 = (0 until x3996).foldLeft( x3994 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x4004 : SpiralS2.ComplexVector = helper._1
val x4005 : Int = helper._2
val x4007 = x4000 * x4005
val x4006 = x3999 * 2
val x4008 = x3998 + x4007
val x4010 = x4008 + x3999
val x4016 = x4008 + x4006
val x4018 = x4010 + x4006
val x4017 = x3994(x4016)
val x4031 = x4016 + x3999
val x4019 = x3994(x4018)
val x4020 = x4017 + x4019
val x4022 = x4017 - x4019
val x4021 = x3995.update(x4016,x4020)
val x4023 = x4021.update(x4018,x4022)
val x4030 = x4023(x4010)
val x4032 = x4023(x4031)
val x4033 = x4030 + x4032
val x4035 = x4030 - x4032
val x4034 = x3995.update(x4010,x4033)
val x4036 = x4034.update(x4031,x4035)
(x4036)
})
val x4041 = x4040

 (x4041)
}
def Basen8lbimb0s0s11twb0s0s11v0_x5128: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x5129 : SpiralS2.ComplexVector = helper._1
val x5130 : SpiralS2.ComplexVector = helper._2
val x5131 : Int = helper._3
val x5132 : Int = helper._4
val x5133 : Int = helper._5
val x5227 = (0 until x5131).foldLeft( x5129 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x5136 : SpiralS2.ComplexVector = helper._1
val x5137 : Int = helper._2
val x5215 = x5132 * 3
val x5138 = x5132 * 4
val x5139 = x5132 * 2
val x5216 = x5137 + x5215
val x5168 = x5137 + x5138
val x5170 = x5168 + x5132
val x5176 = x5168 + x5139
val x5178 = x5170 + x5139
val x5177 = x5129(x5176)
val x5218 = x5168 + x5215
val x5179 = x5129(x5178)
val x5191 = x5176 + x5132
val x5180 = x5177 + x5179
val x5182 = x5177 - x5179
val x5181 = x5130.update(x5176,x5180)
val x5183 = x5181.update(x5178,x5182)
val x5190 = x5183(x5170)
val x5192 = x5183(x5191)
val x5193 = x5190 + x5192
val x5195 = x5190 - x5192
val x5194 = x5130.update(x5170,x5193)
val x5196 = x5194.update(x5191,x5195)
val x5217 = x5196(x5216)
val x5219 = x5196(x5218)
val x5220 = x5217 + x5219
val x5222 = x5217 - x5219
val x5221 = x5130.update(x5216,x5220)
val x5223 = x5221.update(x5218,x5222)
(x5223)
})
val x5228 = x5227

 (x5228)
}
def Basen5lbimb0s0s11twb0s0s11v0_x4927: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x4928 : SpiralS2.ComplexVector = helper._1
val x4929 : SpiralS2.ComplexVector = helper._2
val x4930 : Int = helper._3
val x4931 : Int = helper._4
val x4932 : Int = helper._5
val x4969 = (0 until x4930).foldLeft( x4928 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x4935 : SpiralS2.ComplexVector = helper._1
val x4936 : Int = helper._2
val x4937 = x4931 * 2
val x4939 = x4936 + x4931
val x4945 = x4936 + x4937
val x4947 = x4939 + x4937
val x4946 = x4928(x4945)
val x4960 = x4945 + x4931
val x4948 = x4928(x4947)
val x4949 = x4946 + x4948
val x4951 = x4946 - x4948
val x4950 = x4929.update(x4945,x4949)
val x4952 = x4950.update(x4947,x4951)
val x4959 = x4952(x4939)
val x4961 = x4952(x4960)
val x4962 = x4959 + x4961
val x4964 = x4959 - x4961
val x4963 = x4929.update(x4939,x4962)
val x4965 = x4963.update(x4960,x4964)
(x4965)
})
val x4970 = x4969

 (x4970)
}
def DFTnlbimbs0s11twb0s0s11v_x2142: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x2143 : SpiralS2.ComplexVector = helper._1
val x2144 : SpiralS2.ComplexVector = helper._2
val x2145 : Int = helper._3
val x2146 : Int = helper._4
val x2147 : Int = helper._5
val x2148 : Int = helper._6
val x2149 : Int = helper._7
val x2150 : Int = helper._8
val x2151 = x2145 < 10
val x4653 = if (x2151) {
val x2154 = x2145 < 4
val x2855 = if (x2154) {
val x2157 = x2145 < 2
val x2379 = if (x2157) {
val x2160 = x2145 < 1
val x2269 = if (x2160) {
val x2226 = if (x2160) {
val x2192 =  Basen0lbimbs0s11twb0s0s11v_x2165(x2143, x2144, x2146, x2147, x2148, x2149, x2150)

val x2193 = x2192
(x2193)
} else {
val x2223 =  Basen1lbimbs0s11twb0s0s11v_x2196(x2143, x2144, x2146, x2147, x2148, x2149, x2150)

val x2224 = x2223
(x2224)
}
val x2227 = x2226
(x2227)
} else {
val x2266 = if (x2157) {
val x2223 =  Basen1lbimbs0s11twb0s0s11v_x2196(x2143, x2144, x2146, x2147, x2148, x2149, x2150)

val x2233 = x2223
(x2233)
} else {
val x2263 =  Basen2lbimbs0s11twb0s0s11v_x2236(x2143, x2144, x2146, x2147, x2148, x2149, x2150)

val x2264 = x2263
(x2264)
}
val x2267 = x2266
(x2267)
}
val x2270 = x2269
(x2270)
} else {
val x2273 = x2145 < 3
val x2376 = if (x2273) {
val x2312 = if (x2273) {
val x2263 =  Basen2lbimbs0s11twb0s0s11v_x2236(x2143, x2144, x2146, x2147, x2148, x2149, x2150)

val x2279 = x2263
(x2279)
} else {
val x2309 =  Basen3lbimbs0s11twb0s0s11v_x2282(x2143, x2144, x2146, x2147, x2148, x2149, x2150)

val x2310 = x2309
(x2310)
}
val x2313 = x2312
(x2313)
} else {
val x2373 = if (x2154) {
val x2309 =  Basen3lbimbs0s11twb0s0s11v_x2282(x2143, x2144, x2146, x2147, x2148, x2149, x2150)

val x2319 = x2309
(x2319)
} else {
val x2370 =  Basen4lbimbs0s11twb0s0s11v_x2322(x2143, x2144, x2146, x2147, x2148, x2149, x2150)

val x2371 = x2370
(x2371)
}
val x2374 = x2373
(x2374)
}
val x2377 = x2376
(x2377)
}
val x2380 = x2379
(x2380)
} else {
val x2383 = x2145 < 6
val x2852 = if (x2383) {
val x2386 = x2145 < 5
val x2519 = if (x2386) {
val x2446 = if (x2386) {
val x2370 =  Basen4lbimbs0s11twb0s0s11v_x2322(x2143, x2144, x2146, x2147, x2148, x2149, x2150)

val x2392 = x2370
(x2392)
} else {
val x2443 =  Basen5lbimbs0s11twb0s0s11v_x2395(x2143, x2144, x2146, x2147, x2148, x2149, x2150)

val x2444 = x2443
(x2444)
}
val x2447 = x2446
(x2447)
} else {
val x2516 = if (x2383) {
val x2443 =  Basen5lbimbs0s11twb0s0s11v_x2395(x2143, x2144, x2146, x2147, x2148, x2149, x2150)

val x2453 = x2443
(x2453)
} else {
val x2513 =  Basen6lbimbs0s11twb0s0s11v_x2456(x2143, x2144, x2146, x2147, x2148, x2149, x2150)

val x2514 = x2513
(x2514)
}
val x2517 = x2516
(x2517)
}
val x2520 = x2519
(x2520)
} else {
val x2523 = x2145 < 7
val x2849 = if (x2523) {
val x2592 = if (x2523) {
val x2513 =  Basen6lbimbs0s11twb0s0s11v_x2456(x2143, x2144, x2146, x2147, x2148, x2149, x2150)

val x2529 = x2513
(x2529)
} else {
val x2589 =  Basen7lbimbs0s11twb0s0s11v_x2532(x2143, x2144, x2146, x2147, x2148, x2149, x2150)

val x2590 = x2589
(x2590)
}
val x2593 = x2592
(x2593)
} else {
val x2596 = x2145 < 8
val x2846 = if (x2596) {
val x2713 = if (x2596) {
val x2589 =  Basen7lbimbs0s11twb0s0s11v_x2532(x2143, x2144, x2146, x2147, x2148, x2149, x2150)

val x2602 = x2589
(x2602)
} else {
val x2710 =  Basen8lbimbs0s11twb0s0s11v_x2605(x2143, x2144, x2146, x2147, x2148, x2149, x2150)

val x2711 = x2710
(x2711)
}
val x2714 = x2713
(x2714)
} else {
val x2717 = x2145 < 9
val x2843 = if (x2717) {
val x2710 =  Basen8lbimbs0s11twb0s0s11v_x2605(x2143, x2144, x2146, x2147, x2148, x2149, x2150)

val x2721 = x2710
(x2721)
} else {
val x2840 =  DFTn9lbimbs0s11twb0s0s11v_x2724(x2143, x2144, x2146, x2147, x2148, x2149, x2150)

val x2841 = x2840
(x2841)
}
val x2844 = x2843
(x2844)
}
val x2847 = x2846
(x2847)
}
val x2850 = x2849
(x2850)
}
val x2853 = x2852
(x2853)
}
val x2856 = x2855
(x2856)
} else {
val x2154 = x2145 < 4
val x4650 = if (x2154) {
val x2889 =  F2nlbimbs0s11twb0s0s11v_x2861(x2143, x2144, x2145, x2146, x2147, x2148, x2149, x2150)

val x2890 = x2889
(x2890)
} else {
val x4647 =  DFT_CTnlbimbs0s11twb0s0s11v_x2893(x2143, x2144, x2145, x2146, x2147, x2148, x2149, x2150)

val x4648 = x4647
(x4648)
}
val x4651 = x4650
(x4651)
}
val x4654 = x4653

 (x4654)
}
def DFT_CTnlbimbs0s1twb0s0s11v_x4579: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int))) =>{
val x4580 : SpiralS2.ComplexVector = helper._1
val x4581 : SpiralS2.ComplexVector = helper._2
val x4582 : Int = helper._3
val x4583 : Int = helper._4
val x4584 : Int = helper._5
val x4585 : Int = helper._6
val x4586 : Int = helper._7
val x4587 : Int = helper._8
val x4588 : Int = helper._9
val x4607 = (0 until x4583).foldLeft( x4580 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x4593 : SpiralS2.ComplexVector = helper._1
val x4594 : Int = helper._2
val x4596 = x4587 * x4594
val x4589 = x4582 / 2
val x4597 = x4585 + x4596
val x4595 = x4586 * x4589
val x4590 = x4582 / x4589
val x4599 =  DFTnlbgbs0s1sbs0s1v_x2910(x4580, x4581, x4589, x4590, x4594, x4597, x4586, x4595, x4597, x4586, x4595)

val x4600 = x4599
val x4602 =  DFTnlbimbs0s1twb0s0s11v_x3804(x4600, x4581, x4590, x4589, x4594, x4597, x4595, x4586, x4589)

val x4603 = x4602
(x4603)
})
val x4608 = x4607

 (x4608)
}
def DFT_CTnlbimb0s0s11twb0s0s11v0_x5404: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int))) =>{
val x5405 : SpiralS2.ComplexVector = helper._1
val x5406 : SpiralS2.ComplexVector = helper._2
val x5407 : Int = helper._3
val x5408 : Int = helper._4
val x5409 : Int = helper._5
val x5410 : Int = helper._6
val x5427 = (0 until x5408).foldLeft( x5405 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x5415 : SpiralS2.ComplexVector = helper._1
val x5416 : Int = helper._2
val x5411 = x5407 / 2
val x5412 = x5407 / x5411
val x5417 = x5409 * x5411
val x5419 =  DFTnlbgbs0s1sbs0s1v_x2910(x5405, x5406, x5411, x5412, x5416, x5416, x5409, x5417, x5416, x5409, x5417)

val x5420 = x5419
val x5422 =  DFTnlbimbs0s1twb0s0s11v_x3804(x5420, x5406, x5412, x5411, x5416, x5416, x5417, x5409, x5411)

val x5423 = x5422
(x5423)
})
val x5428 = x5427

 (x5428)
}
def DFTnlbimbs0s1twb0s0s11v_x3804: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int,Int))) =>{
val x3805 : SpiralS2.ComplexVector = helper._1
val x3806 : SpiralS2.ComplexVector = helper._2
val x3807 : Int = helper._3
val x3808 : Int = helper._4
val x3809 : Int = helper._5
val x3810 : Int = helper._6
val x3811 : Int = helper._7
val x3812 : Int = helper._8
val x3813 : Int = helper._9
val x3814 = x3807 < 10
val x4616 = if (x3814) {
val x3817 = x3807 < 4
val x4539 = if (x3817) {
val x3820 = x3807 < 2
val x4052 = if (x3820) {
val x3823 = x3807 < 1
val x3938 = if (x3823) {
val x3893 = if (x3823) {
val x3857 =  Basen0lbimbs0s1twb0s0s11v_x3828(x3805, x3806, x3808, x3809, x3810, x3811, x3812, x3813)

val x3858 = x3857
(x3858)
} else {
val x3890 =  Basen1lbimbs0s1twb0s0s11v_x3861(x3805, x3806, x3808, x3809, x3810, x3811, x3812, x3813)

val x3891 = x3890
(x3891)
}
val x3894 = x3893
(x3894)
} else {
val x3935 = if (x3820) {
val x3890 =  Basen1lbimbs0s1twb0s0s11v_x3861(x3805, x3806, x3808, x3809, x3810, x3811, x3812, x3813)

val x3900 = x3890
(x3900)
} else {
val x3932 =  Basen2lbimbs0s1twb0s0s11v_x3903(x3805, x3806, x3808, x3809, x3810, x3811, x3812, x3813)

val x3933 = x3932
(x3933)
}
val x3936 = x3935
(x3936)
}
val x3939 = x3938
(x3939)
} else {
val x3942 = x3807 < 3
val x4049 = if (x3942) {
val x3983 = if (x3942) {
val x3932 =  Basen2lbimbs0s1twb0s0s11v_x3903(x3805, x3806, x3808, x3809, x3810, x3811, x3812, x3813)

val x3948 = x3932
(x3948)
} else {
val x3980 =  Basen3lbimbs0s1twb0s0s11v_x3951(x3805, x3806, x3808, x3809, x3810, x3811, x3812, x3813)

val x3981 = x3980
(x3981)
}
val x3984 = x3983
(x3984)
} else {
val x4046 = if (x3817) {
val x3980 =  Basen3lbimbs0s1twb0s0s11v_x3951(x3805, x3806, x3808, x3809, x3810, x3811, x3812, x3813)

val x3990 = x3980
(x3990)
} else {
val x4043 =  Basen4lbimbs0s1twb0s0s11v_x3993(x3805, x3806, x3808, x3809, x3810, x3811, x3812, x3813)

val x4044 = x4043
(x4044)
}
val x4047 = x4046
(x4047)
}
val x4050 = x4049
(x4050)
}
val x4053 = x4052
(x4053)
} else {
val x4056 = x3807 < 6
val x4536 = if (x4056) {
val x4059 = x3807 < 5
val x4196 = if (x4059) {
val x4121 = if (x4059) {
val x4043 =  Basen4lbimbs0s1twb0s0s11v_x3993(x3805, x3806, x3808, x3809, x3810, x3811, x3812, x3813)

val x4065 = x4043
(x4065)
} else {
val x4118 =  Basen5lbimbs0s1twb0s0s11v_x4068(x3805, x3806, x3808, x3809, x3810, x3811, x3812, x3813)

val x4119 = x4118
(x4119)
}
val x4122 = x4121
(x4122)
} else {
val x4193 = if (x4056) {
val x4118 =  Basen5lbimbs0s1twb0s0s11v_x4068(x3805, x3806, x3808, x3809, x3810, x3811, x3812, x3813)

val x4128 = x4118
(x4128)
} else {
val x4190 =  Basen6lbimbs0s1twb0s0s11v_x4131(x3805, x3806, x3808, x3809, x3810, x3811, x3812, x3813)

val x4191 = x4190
(x4191)
}
val x4194 = x4193
(x4194)
}
val x4197 = x4196
(x4197)
} else {
val x4200 = x3807 < 7
val x4533 = if (x4200) {
val x4271 = if (x4200) {
val x4190 =  Basen6lbimbs0s1twb0s0s11v_x4131(x3805, x3806, x3808, x3809, x3810, x3811, x3812, x3813)

val x4206 = x4190
(x4206)
} else {
val x4268 =  Basen7lbimbs0s1twb0s0s11v_x4209(x3805, x3806, x3808, x3809, x3810, x3811, x3812, x3813)

val x4269 = x4268
(x4269)
}
val x4272 = x4271
(x4272)
} else {
val x4275 = x3807 < 8
val x4530 = if (x4275) {
val x4394 = if (x4275) {
val x4268 =  Basen7lbimbs0s1twb0s0s11v_x4209(x3805, x3806, x3808, x3809, x3810, x3811, x3812, x3813)

val x4281 = x4268
(x4281)
} else {
val x4391 =  Basen8lbimbs0s1twb0s0s11v_x4284(x3805, x3806, x3808, x3809, x3810, x3811, x3812, x3813)

val x4392 = x4391
(x4392)
}
val x4395 = x4394
(x4395)
} else {
val x4398 = x3807 < 9
val x4527 = if (x4398) {
val x4391 =  Basen8lbimbs0s1twb0s0s11v_x4284(x3805, x3806, x3808, x3809, x3810, x3811, x3812, x3813)

val x4402 = x4391
(x4402)
} else {
val x4524 =  DFTn9lbimbs0s1twb0s0s11v_x4405(x3805, x3806, x3808, x3809, x3810, x3811, x3812, x3813)

val x4525 = x4524
(x4525)
}
val x4528 = x4527
(x4528)
}
val x4531 = x4530
(x4531)
}
val x4534 = x4533
(x4534)
}
val x4537 = x4536
(x4537)
}
val x4540 = x4539
(x4540)
} else {
val x3817 = x3807 < 4
val x4613 = if (x3817) {
val x4575 =  F2nlbimbs0s1twb0s0s11v_x4545(x3805, x3806, x3807, x3808, x3809, x3810, x3811, x3812, x3813)

val x4576 = x4575
(x4576)
} else {
val x4610 =  DFT_CTnlbimbs0s1twb0s0s11v_x4579(x3805, x3806, x3807, x3808, x3809, x3810, x3811, x3812, x3813)

val x4611 = x4610
(x4611)
}
val x4614 = x4613
(x4614)
}
val x4617 = x4616

 (x4617)
}
def Basen3lb1gb0s01s10sb0s01s10v0_x94: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector))) =>{
val x95 : SpiralS2.ComplexVector = helper._1
val x96 : SpiralS2.ComplexVector = helper._2
val x97 = x95(0)
val x98 = x95(1)
val x99 = x97 + x98
val x101 = x97 - x98
val x100 = x96.update(0,x99)
val x102 = x100.update(1,x101)

 (x102)
}
def Basen5lbgbs01s1sbs01s1v_x1582: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x1583 : SpiralS2.ComplexVector = helper._1
val x1584 : SpiralS2.ComplexVector = helper._2
val x1585 : Int = helper._3
val x1586 : Int = helper._4
val x1587 : Int = helper._5
val x1588 : Int = helper._6
val x1589 : Int = helper._7
val x1590 : Int = helper._8
val x1633 = (0 until x1585).foldLeft( x1583 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x1593 : SpiralS2.ComplexVector = helper._1
val x1594 : Int = helper._2
val x1597 = x1590 * x1594
val x1595 = x1588 * x1594
val x1598 = x1589 + x1597
val x1596 = x1587 + x1595
val x1600 = x1596 + 1
val x1609 = x1600 + 2
val x1604 = x1598 + 1
val x1611 = x1598 + 2
val x1624 = x1611 + 1
val x1607 = x1596 + 2
val x1614 = x1604 + 2
val x1610 = x1583(x1609)
val x1608 = x1583(x1607)
val x1612 = x1608 + x1610
val x1615 = x1608 - x1610
val x1613 = x1584.update(x1611,x1612)
val x1616 = x1613.update(x1614,x1615)
val x1623 = x1616(x1604)
val x1625 = x1616(x1624)
val x1626 = x1623 + x1625
val x1628 = x1623 - x1625
val x1627 = x1584.update(x1604,x1626)
val x1629 = x1627.update(x1624,x1628)
(x1629)
})
val x1634 = x1633

 (x1634)
}
def Basen8lbgb0s01s1sb0s01s1v0_x992: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x993 : SpiralS2.ComplexVector = helper._1
val x994 : SpiralS2.ComplexVector = helper._2
val x995 : Int = helper._3
val x996 : Int = helper._4
val x997 : Int = helper._5
val x1097 = (0 until x995).foldLeft( x993 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x1000 : SpiralS2.ComplexVector = helper._1
val x1001 : Int = helper._2
val x1002 = x996 * x1001
val x1003 = x997 * x1001
val x1036 = x1003 + 4
val x1042 = x1036 + 1
val x1086 = x1003 + 3
val x1035 = x1002 + 4
val x1049 = x1036 + 2
val x1038 = x1035 + 1
val x1047 = x1038 + 2
val x1048 = x993(x1047)
val x1052 = x1042 + 2
val x1062 = x1049 + 1
val x1045 = x1035 + 2
val x1088 = x1036 + 3
val x1046 = x993(x1045)
val x1050 = x1046 + x1048
val x1053 = x1046 - x1048
val x1051 = x994.update(x1049,x1050)
val x1054 = x1051.update(x1052,x1053)
val x1061 = x1054(x1042)
val x1063 = x1054(x1062)
val x1064 = x1061 + x1063
val x1066 = x1061 - x1063
val x1065 = x994.update(x1042,x1064)
val x1067 = x1065.update(x1062,x1066)
val x1087 = x1067(x1086)
val x1089 = x1067(x1088)
val x1090 = x1087 + x1089
val x1092 = x1087 - x1089
val x1091 = x994.update(x1086,x1090)
val x1093 = x1091.update(x1088,x1092)
(x1093)
})
val x1098 = x1097

 (x1098)
}
def F2nlbimb0s0s11twb0s0s11v0_x5376: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int))) =>{
val x5377 : SpiralS2.ComplexVector = helper._1
val x5378 : SpiralS2.ComplexVector = helper._2
val x5379 : Int = helper._3
val x5380 : Int = helper._4
val x5381 : Int = helper._5
val x5382 : Int = helper._6
val x5397 = (0 until x5380).foldLeft( x5377 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x5385 : SpiralS2.ComplexVector = helper._1
val x5386 : Int = helper._2
val x5387 = x5377(x5386)
val x5388 = x5381 + x5386
val x5389 = x5377(x5388)
val x5390 = x5387 + x5389
val x5392 = x5387 - x5389
val x5391 = x5378.update(x5386,x5390)
val x5393 = x5391.update(x5388,x5392)
(x5393)
})
val x5398 = x5397

 (x5398)
}
def DFT_CTn9lbgb0s01s1sb0s01s1v0_x1120: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x1121 : SpiralS2.ComplexVector = helper._1
val x1122 : SpiralS2.ComplexVector = helper._2
val x1123 : Int = helper._3
val x1124 : Int = helper._4
val x1125 : Int = helper._5
val x1225 = (0 until x1123).foldLeft( x1121 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x1128 : SpiralS2.ComplexVector = helper._1
val x1129 : Int = helper._2
val x1131 = x1125 * x1129
val x1130 = x1124 * x1129
val x1214 = x1131 + 3
val x1164 = x1131 + 4
val x1163 = x1130 + 4
val x1173 = x1163 + 2
val x1174 = x1121(x1173)
val x1170 = x1164 + 1
val x1177 = x1164 + 2
val x1190 = x1177 + 1
val x1166 = x1163 + 1
val x1180 = x1170 + 2
val x1216 = x1164 + 3
val x1175 = x1166 + 2
val x1176 = x1121(x1175)
val x1178 = x1174 + x1176
val x1181 = x1174 - x1176
val x1179 = x1122.update(x1177,x1178)
val x1182 = x1179.update(x1180,x1181)
val x1189 = x1182(x1170)
val x1191 = x1182(x1190)
val x1192 = x1189 + x1191
val x1194 = x1189 - x1191
val x1193 = x1122.update(x1170,x1192)
val x1195 = x1193.update(x1190,x1194)
val x1215 = x1195(x1214)
val x1217 = x1195(x1216)
val x1218 = x1215 + x1217
val x1220 = x1215 - x1217
val x1219 = x1122.update(x1214,x1218)
val x1221 = x1219.update(x1216,x1220)
(x1221)
})
val x1226 = x1225

 (x1226)
}
def Basen2lbimb0s0s11twb0s0s11v0_x4779: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int))) =>{
val x4780 : SpiralS2.ComplexVector = helper._1
val x4781 : SpiralS2.ComplexVector = helper._2
val x4782 : Int = helper._3
val x4783 : Int = helper._4
val x4784 : Int = helper._5
val x4799 = (0 until x4782).foldLeft( x4780 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x4787 : SpiralS2.ComplexVector = helper._1
val x4788 : Int = helper._2
val x4789 = x4780(x4788)
val x4790 = x4783 + x4788
val x4791 = x4780(x4790)
val x4792 = x4789 + x4791
val x4794 = x4789 - x4791
val x4793 = x4781.update(x4788,x4792)
val x4795 = x4793.update(x4790,x4794)
(x4795)
})
val x4800 = x4799

 (x4800)
}
def DFT_CTn9lbgbs01s1sbs01s1v_x1944: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int)) => ((SpiralS2.ComplexVector)) = 
(helper: ((SpiralS2.ComplexVector,SpiralS2.ComplexVector,Int,Int,Int,Int,Int,Int))) =>{
val x1945 : SpiralS2.ComplexVector = helper._1
val x1946 : SpiralS2.ComplexVector = helper._2
val x1947 : Int = helper._3
val x1948 : Int = helper._4
val x1949 : Int = helper._5
val x1950 : Int = helper._6
val x1951 : Int = helper._7
val x1952 : Int = helper._8
val x2054 = (0 until x1947).foldLeft( x1945 )(
  (acc,ele) => {
 val helper = (acc,ele)

val x1955 : SpiralS2.ComplexVector = helper._1
val x1956 : Int = helper._2
val x1959 = x1952 * x1956
val x1960 = x1951 + x1959
val x2043 = x1960 + 3
val x1957 = x1950 * x1956
val x1993 = x1960 + 4
val x1999 = x1993 + 1
val x2006 = x1993 + 2
val x1958 = x1949 + x1957
val x1992 = x1958 + 4
val x1995 = x1992 + 1
val x2002 = x1992 + 2
val x2045 = x1993 + 3
val x2004 = x1995 + 2
val x2005 = x1945(x2004)
val x2009 = x1999 + 2
val x2019 = x2006 + 1
val x2003 = x1945(x2002)
val x2007 = x2003 + x2005
val x2010 = x2003 - x2005
val x2008 = x1946.update(x2006,x2007)
val x2011 = x2008.update(x2009,x2010)
val x2018 = x2011(x1999)
val x2020 = x2011(x2019)
val x2021 = x2018 + x2020
val x2023 = x2018 - x2020
val x2022 = x1946.update(x1999,x2021)
val x2024 = x2022.update(x2019,x2023)
val x2044 = x2024(x2043)
val x2046 = x2024(x2045)
val x2047 = x2044 + x2046
val x2049 = x2044 - x2046
val x2048 = x1946.update(x2043,x2047)
val x2050 = x2048.update(x2045,x2049)
(x2050)
})
val x2055 = x2054

 (x2055)
}
//bla end!

}

