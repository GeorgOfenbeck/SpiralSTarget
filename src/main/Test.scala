

import java.util.concurrent._
import scala.util.DynamicVariable


object SpiralS2 extends App {

  object common {

    val forkJoinPool = new ForkJoinPool

    abstract class TaskScheduler {
      def schedule[T](body: => T): ForkJoinTask[T]

      def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
        val right = task {
          taskB
        }
        val left = taskA
        (left, right.join())
      }
    }

    class DefaultTaskScheduler extends TaskScheduler {
      def schedule[T](body: => T): ForkJoinTask[T] = {
        val t = new RecursiveTask[T] {
          def compute = body
        }
        Thread.currentThread match {
          case wt: ForkJoinWorkerThread =>
            t.fork()
          case _ =>
            forkJoinPool.execute(t)
        }
        t
      }
    }

    val scheduler =
      new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

    def task[T](body: => T): ForkJoinTask[T] = {
      scheduler.value.schedule(body)
    }

    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      scheduler.value.parallel(taskA, taskB)
    }

    def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
      val ta = task {
        taskA
      }
      val tb = task {
        taskB
      }
      val tc = task {
        taskC
      }
      val td = taskD
      (ta.join(), tb.join(), tc.join(), td)
    }
  }

  object Settings {
    val decompchoice: Map[List[Int], (Int, Boolean, Boolean)] = Map(List(64, 1, 32, 1, 16, -1, 2) -> (-1, true, true), List(64, 1, 32, 1, 16, 1, 8, 1, 4, 1, 2) -> (-1, true, true), List(64, 1, 32, -1, 2) -> (-1, true, true), List(64, 1, 32, 1, 16, 1, 8, 1, 4) -> (2, false, false), List(64, 1, 32, 1, 16, 1, 8, -1, 2) -> (-1, true, true), List(64, 1, 32, 1, 16) -> (8, false, false), List(64) -> (32, false, false), List(64, -1, 2) -> (-1, true, true), List(64, 1, 32, 1, 16, 1, 8) -> (4, false, false), List(64, 1, 32) -> (16, false, false), List(64, 1, 32, 1, 16, 1, 8, 1, 4, -1, 2) -> (-1, true, true))
    val validate = false
    //true
    val WHT = true
    var sanitycheck: Int = 0


  }


  object Twiddle {
    var precompbuffer: Vector[Complex] = Vector.empty
    var dprecompbuffer: Vector[Double] = Vector.empty
    var precomp: Array[Complex] = null
    var dprecomp: Array[Double] = null
    var twindex = 0

    def store(n: Int, d: Int, k: Int, i: Int): Complex = {
      val t = Twiddle.apply(n, d, k, i)
      precompbuffer = precompbuffer :+ t
      t
    }

    def load(): Complex = {
      val t = precomp(twindex)
      twindex = twindex + 1;
      t
    }

    def dstore(n: Int, d: Int, k: Int, i: Int, re: Boolean): Double = {
      val t: Double = if (re) Twiddle.apply(n, d, k, i).re else Twiddle.apply(n, d, k, i).im
      dprecompbuffer = dprecompbuffer :+ t
      t
    }

    def dload(): Double = {
      val t = dprecomp(twindex)
      twindex = twindex + 1;
      t
    }

    def parloop(size: Int, numTasks: Int, ini: ComplexVector, out: ComplexVector, body: ((ComplexVector, Int)) => ComplexVector): ComplexVector = {
      val r = (0 to size by (size / (Math.min(numTasks, size))))
      val ranges1 = (r zip r.tail)
      val last: (Int, Int) = (ranges1.last._1, size)
      val ranges = ranges1.dropRight(1) :+ last

      def blub(r: Range): Unit = {
        r.map(
          p => {
            val t = (ini, p)
            body(t)
          }
        )
      }

      val tasks = ranges.map({ case (from, to) => common.task(blub(from until to)) })
      tasks foreach {
        _.join
      }
      out
    }

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

  class InterleavedVector(val save: Array[Double]) extends AnyVal {
    //class ComplexVector(n: Int) extends AnyVal{
    //val save = new Array[Complex](n)

    def apply(i: Int): Complex = Complex(save(2 * i), save(2 * i + 1))

    def update(i: Int, y: Complex): InterleavedVector = {
      save(2 * i) = y.re
      save(2 * i + 1) = y.im
      this
    }

    def print() = {
      save.map(p => println(p))
    }

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

  def time(): Double = {
    val t = new testClass
    val pre = new PreCompute

    /*for (i <- 0 until 4) {
      for (j <- 0 until 4)
        println(Twiddle.WHT(4, i, j))
      println(" ------------")
    }*/
    val r = new Random()

    val iterateover = (6 until 7)
    val timings = for (twopower <- iterateover) yield {
      val size = 64

      var fail = false
      val fftmatrix = for (i <- Vector(r.nextInt(size)) /*0 until size*/ ) yield {
        //columns
        val one = Complex(1, 0)
        val zero = Complex(0, 0)

        val input = (0 until size).foldLeft(new InterleavedVector(new Array[Double](2 * size))) {
          (acc, ele) => if (ele == i) acc.update(ele, one) else acc.update(ele, zero)
        }
        val out = new InterleavedVector(new Array[Double](2 * size))
        Twiddle.twindex = 0
        Twiddle.precompbuffer = Vector.empty
        Twiddle.dprecompbuffer = Vector.empty
        //VARIOUS PRE CALLS HERE
        val resx = pre.apply(input.save, out.save)
        val res = new InterleavedVector(resx) //, 0, instride, 0, instride, Vector.empty)     // VARIOUS PRE CALLS HERE END
        Twiddle.precomp = Twiddle.precompbuffer.toArray
        Twiddle.dprecomp = Twiddle.dprecompbuffer.toArray
        val txy = for (j <- 0 until 10) yield {
          //Settings.standardConfig measure {
          for (kk <- 0 until 100000) {


            val instride = Vector(1, 1)
            Settings.sanitycheck = 0
            Twiddle.twindex = 0
            //VARIOUS CALLS HERE
            var elapsedTime = System.nanoTime
            val resx = t.apply(input.save, out.save)
            val res = new InterleavedVector(resx) //, 0, instride, 0, instride, Vector.empty)     // VARIOUS CALLS HERE END
            val endtime =  (System.nanoTime - elapsedTime)

            val nlogn = size * Math.log10(size)/Math.log10(2)

            val gflops = nlogn/endtime

            println(s"gflops $gflops")


            if (Settings.validate) {

              for (c <- 0 until size) {
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
          }
        }
        val seqtime = 0.0
        println(s"time: $seqtime ms")
        seqtime

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

      fftmatrix
    }
    println("timings")
    println(timings)
    timings.head.head
  }


  time()

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


  //bla!
  /** ***************************************
    * Emitting Generated Code
    * ******************************************/
  class testClass extends (((Array[Double], Array[Double])) => ((Array[Double]))) {
    def apply(helper: ((Array[Double], Array[Double]))): ((Array[Double])) = {
      val x1: Array[Double] = helper._1
      val x2: Array[Double] = helper._2
      val x236 = {
        for (lc <- 0 until 1) {
          val helper = (lc, x1, x2)

          val x5: Int = helper._1
          val x6: Array[Double] = helper._2
          val x7: Array[Double] = helper._3
          val x10 = new Array[Double](2 * 64)
          //buffer creation
          val x209 = {
            for (lc <- 0 until 2) {
              val helper = (lc, x1, x10)

              val x13: Int = helper._1
              val x14: Array[Double] = helper._2
              val x15: Array[Double] = helper._3
              val x18 = new Array[Double](2 * 32)
              //buffer creation
              val x175 = {
                for (lc <- 0 until 2) {
                  val helper = (lc, x1, x18)

                  val x20: Int = helper._1
                  val x21: Array[Double] = helper._2
                  val x22: Array[Double] = helper._3
                  val x26 = new Array[Double](2 * 16)
                  //buffer creation
                  val x141 = {
                    for (lc <- 0 until 2) {
                      val helper = (lc, x1, x26)

                      val x28: Int = helper._1
                      val x29: Array[Double] = helper._2
                      val x30: Array[Double] = helper._3
                      val x34 = new Array[Double](2 * 8)
                      //buffer creation
                      val x107 = {
                        for (lc <- 0 until 2) {
                          val helper = (lc, x1, x34)

                          val x36: Int = helper._1
                          val x37: Array[Double] = helper._2
                          val x38: Array[Double] = helper._3
                          val x42 = new Array[Double](2 * 4)
                          //buffer creation
                          val x73 = {
                            for (lc <- 0 until 2) {
                              val helper = (lc, x1, x42)

                              val x44: Int = helper._1
                              val x45: Array[Double] = helper._2
                              val x46: Array[Double] = helper._3
                              val x24 = 16 * x20
                              val x32 = 8 * x28
                              val x17 = 32 * x13
                              val x40 = 4 * x36
                              val x25 = x17 + x24
                              val x47 = 2 * x44
                              val x33 = x25 + x32
                              val x61 = 2 * x47
                              val x65 = 1 + x47
                              val x41 = x33 + x40
                              val x53 = x41 + 1
                              val x54 = x53 + x47
                              val x48 = x41 + x47
                              val x49 = 2 * x48
                              val x63 = x61 + 1
                              val x50 = x1(x49)
                              val x55 = 2 * x54
                              val x56 = x1(x55)
                              val x57 = x55 + 1
                              val x59 = x50 + x56
                              val x66 = x50 - x56
                              val x58 = x1(x57)
                              val x51 = x49 + 1
                              val x68 = 2 * x65
                              val x62 = {
                                x42.update(x61, x59); x42
                              }
                              val x52 = x1(x51)
                              val x70 = x68 + 1
                              val x60 = x52 + x58
                              val x67 = x52 - x58
                              val x64 = {
                                x62.update(x63, x60); x62
                              }
                              val x69 = {
                                x64.update(x68, x66); x64
                              }
                              val x71 = {
                                x69.update(x70, x67); x69
                              }
                              (x71)
                            };
                            x42
                          }
                          val x74 = x73
                          val x104 = {
                            for (lc <- 0 until 2) {
                              val helper = (lc, x74, x34)

                              val x76: Int = helper._1
                              val x77: Array[Double] = helper._2
                              val x78: Array[Double] = helper._3
                              val x40 = 4 * x36
                              val x88 = x40 + x76
                              val x91 = 2 * x88
                              val x93 = x91 + 1
                              val x95 = x40 + 2
                              val x79 = 2 * x76
                              val x96 = x95 + x76
                              val x81 = x79 + 1
                              val x80 = x74(x79)
                              val x99 = 2 * x96
                              val x82 = x74(x81)
                              val x83 = 2 + x76
                              val x101 = x99 + 1
                              val x84 = 2 * x83
                              val x85 = x74(x84)
                              val x86 = x84 + 1
                              val x89 = x80 + x85
                              val x97 = x80 - x85
                              val x87 = x74(x86)
                              val x92 = {
                                x34.update(x91, x89); x34
                              }
                              val x90 = x82 + x87
                              val x98 = x82 - x87
                              val x94 = {
                                x92.update(x93, x90); x92
                              }
                              val x100 = {
                                x94.update(x99, x97); x94
                              }
                              val x102 = {
                                x100.update(x101, x98); x100
                              }
                              (x102)
                            };
                            x34
                          }
                          val x105 = x104
                          (x105)
                        };
                        x34
                      }
                      val x108 = x107
                      val x138 = {
                        for (lc <- 0 until 4) {
                          val helper = (lc, x108, x26)

                          val x110: Int = helper._1
                          val x111: Array[Double] = helper._2
                          val x112: Array[Double] = helper._3
                          val x32 = 8 * x28
                          val x117 = 4 + x110
                          val x129 = x32 + 4
                          val x118 = 2 * x117
                          val x120 = x118 + 1
                          val x121 = x108(x120)
                          val x113 = 2 * x110
                          val x115 = x113 + 1
                          val x116 = x108(x115)
                          val x132 = x116 - x121
                          val x124 = x116 + x121
                          val x130 = x129 + x110
                          val x133 = 2 * x130
                          val x135 = x133 + 1
                          val x114 = x108(x113)
                          val x119 = x108(x118)
                          val x122 = x32 + x110
                          val x123 = x114 + x119
                          val x131 = x114 - x119
                          val x125 = 2 * x122
                          val x126 = {
                            x26.update(x125, x123); x26
                          }
                          val x127 = x125 + 1
                          val x128 = {
                            x126.update(x127, x124); x126
                          }
                          val x134 = {
                            x128.update(x133, x131); x128
                          }
                          val x136 = {
                            x134.update(x135, x132); x134
                          }
                          (x136)
                        };
                        x26
                      }
                      val x139 = x138
                      (x139)
                    };
                    x26
                  }
                  val x142 = x141
                  val x172 = {
                    for (lc <- 0 until 8) {
                      val helper = (lc, x142, x18)

                      val x144: Int = helper._1
                      val x145: Array[Double] = helper._2
                      val x146: Array[Double] = helper._3
                      val x24 = 16 * x20
                      val x147 = 2 * x144
                      val x156 = x24 + x144
                      val x148 = x142(x147)
                      val x149 = x147 + 1
                      val x159 = 2 * x156
                      val x161 = x159 + 1
                      val x150 = x142(x149)
                      val x151 = 8 + x144
                      val x163 = x24 + 8
                      val x152 = 2 * x151
                      val x164 = x163 + x144
                      val x153 = x142(x152)
                      val x157 = x148 + x153
                      val x160 = {
                        x18.update(x159, x157); x18
                      }
                      val x165 = x148 - x153
                      val x167 = 2 * x164
                      val x154 = x152 + 1
                      val x169 = x167 + 1
                      val x155 = x142(x154)
                      val x158 = x150 + x155
                      val x166 = x150 - x155
                      val x162 = {
                        x160.update(x161, x158); x160
                      }
                      val x168 = {
                        x162.update(x167, x165); x162
                      }
                      val x170 = {
                        x168.update(x169, x166); x168
                      }
                      (x170)
                    };
                    x18
                  }
                  val x173 = x172
                  (x173)
                };
                x18
              }
              val x176 = x175
              val x206 = {
                for (lc <- 0 until 16) {
                  val helper = (lc, x176, x10)

                  val x178: Int = helper._1
                  val x179: Array[Double] = helper._2
                  val x180: Array[Double] = helper._3
                  val x17 = 32 * x13
                  val x185 = 16 + x178
                  val x197 = x17 + 16
                  val x181 = 2 * x178
                  val x198 = x197 + x178
                  val x201 = 2 * x198
                  val x182 = x176(x181)
                  val x203 = x201 + 1
                  val x186 = 2 * x185
                  val x188 = x186 + 1
                  val x189 = x176(x188)
                  val x187 = x176(x186)
                  val x191 = x182 + x187
                  val x199 = x182 - x187
                  val x190 = x17 + x178
                  val x183 = x181 + 1
                  val x193 = 2 * x190
                  val x184 = x176(x183)
                  val x192 = x184 + x189
                  val x194 = {
                    x10.update(x193, x191); x10
                  }
                  val x195 = x193 + 1
                  val x200 = x184 - x189
                  val x196 = {
                    x194.update(x195, x192); x194
                  }
                  val x202 = {
                    x196.update(x201, x199); x196
                  }
                  val x204 = {
                    x202.update(x203, x200); x202
                  }
                  (x204)
                };
                x10
              }
              val x207 = x206
              (x207)
            };
            x10
          }
          val x210 = x209
          val x233 = {
            for (lc <- 0 until 32) {
              val helper = (lc, x210, x2)

              val x212: Int = helper._1
              val x213: Array[Double] = helper._2
              val x214: Array[Double] = helper._3
              val x215 = 2 * x212
              val x219 = 32 + x212
              val x216 = x210(x215)
              val x217 = x215 + 1
              val x220 = 2 * x219
              val x218 = x210(x217)
              val x221 = x210(x220)
              val x222 = x220 + 1
              val x224 = x216 + x221
              val x228 = x216 - x221
              val x223 = x210(x222)
              val x226 = {
                x2.update(x215, x224); x2
              }
              val x225 = x218 + x223
              val x229 = x218 - x223
              val x227 = {
                x226.update(x217, x225); x226
              }
              val x230 = {
                x227.update(x220, x228); x227
              }
              val x231 = {
                x230.update(x222, x229); x230
              }
              (x231)
            };
            x2
          }
          val x234 = x233
          (x234)
        };
        x2
      }
      val x237 = x236

      (x237)
    }

    /** ***************************************
      * End Main
      * ******************************************/
    //bla end!

  }

  //bla!
  /** ***************************************
    * Emitting Generated Code
    * ******************************************/
  class PreCompute extends (((Array[Double], Array[Double])) => ((Array[Double]))) {
    def apply(helper: ((Array[Double], Array[Double]))): ((Array[Double])) = {
      val x1: Array[Double] = helper._1
      val x2: Array[Double] = helper._2
      val x236 = {
        for (lc <- 0 until 1) {
          val helper = (lc, x1, x2)

          val x5: Int = helper._1
          val x6: Array[Double] = helper._2
          val x7: Array[Double] = helper._3
          val x10 = new Array[Double](2 * 64)
          //buffer creation
          val x209 = {
            for (lc <- 0 until 2) {
              val helper = (lc, x1, x10)

              val x13: Int = helper._1
              val x14: Array[Double] = helper._2
              val x15: Array[Double] = helper._3
              val x18 = new Array[Double](2 * 32)
              //buffer creation
              val x175 = {
                for (lc <- 0 until 2) {
                  val helper = (lc, x1, x18)

                  val x20: Int = helper._1
                  val x21: Array[Double] = helper._2
                  val x22: Array[Double] = helper._3
                  val x26 = new Array[Double](2 * 16)
                  //buffer creation
                  val x141 = {
                    for (lc <- 0 until 2) {
                      val helper = (lc, x1, x26)

                      val x28: Int = helper._1
                      val x29: Array[Double] = helper._2
                      val x30: Array[Double] = helper._3
                      val x34 = new Array[Double](2 * 8)
                      //buffer creation
                      val x107 = {
                        for (lc <- 0 until 2) {
                          val helper = (lc, x1, x34)

                          val x36: Int = helper._1
                          val x37: Array[Double] = helper._2
                          val x38: Array[Double] = helper._3
                          val x42 = new Array[Double](2 * 4)
                          //buffer creation
                          val x73 = {
                            for (lc <- 0 until 2) {
                              val helper = (lc, x1, x42)

                              val x44: Int = helper._1
                              val x45: Array[Double] = helper._2
                              val x46: Array[Double] = helper._3
                              val x24 = 16 * x20
                              val x32 = 8 * x28
                              val x17 = 32 * x13
                              val x40 = 4 * x36
                              val x25 = x17 + x24
                              val x47 = 2 * x44
                              val x33 = x25 + x32
                              val x61 = 2 * x47
                              val x65 = 1 + x47
                              val x41 = x33 + x40
                              val x53 = x41 + 1
                              val x54 = x53 + x47
                              val x48 = x41 + x47
                              val x49 = 2 * x48
                              val x63 = x61 + 1
                              val x50 = x1(x49)
                              val x55 = 2 * x54
                              val x56 = x1(x55)
                              val x57 = x55 + 1
                              val x59 = x50 + x56
                              val x66 = x50 - x56
                              val x58 = x1(x57)
                              val x51 = x49 + 1
                              val x68 = 2 * x65
                              val x62 = {
                                x42.update(x61, x59); x42
                              }
                              val x52 = x1(x51)
                              val x70 = x68 + 1
                              val x60 = x52 + x58
                              val x67 = x52 - x58
                              val x64 = {
                                x62.update(x63, x60); x62
                              }
                              val x69 = {
                                x64.update(x68, x66); x64
                              }
                              val x71 = {
                                x69.update(x70, x67); x69
                              }
                              (x71)
                            };
                            x42
                          }
                          val x74 = x73
                          val x104 = {
                            for (lc <- 0 until 2) {
                              val helper = (lc, x74, x34)

                              val x76: Int = helper._1
                              val x77: Array[Double] = helper._2
                              val x78: Array[Double] = helper._3
                              val x40 = 4 * x36
                              val x88 = x40 + x76
                              val x91 = 2 * x88
                              val x93 = x91 + 1
                              val x95 = x40 + 2
                              val x79 = 2 * x76
                              val x96 = x95 + x76
                              val x81 = x79 + 1
                              val x80 = x74(x79)
                              val x99 = 2 * x96
                              val x82 = x74(x81)
                              val x83 = 2 + x76
                              val x101 = x99 + 1
                              val x84 = 2 * x83
                              val x85 = x74(x84)
                              val x86 = x84 + 1
                              val x89 = x80 + x85
                              val x97 = x80 - x85
                              val x87 = x74(x86)
                              val x92 = {
                                x34.update(x91, x89); x34
                              }
                              val x90 = x82 + x87
                              val x98 = x82 - x87
                              val x94 = {
                                x92.update(x93, x90); x92
                              }
                              val x100 = {
                                x94.update(x99, x97); x94
                              }
                              val x102 = {
                                x100.update(x101, x98); x100
                              }
                              (x102)
                            };
                            x34
                          }
                          val x105 = x104
                          (x105)
                        };
                        x34
                      }
                      val x108 = x107
                      val x138 = {
                        for (lc <- 0 until 4) {
                          val helper = (lc, x108, x26)

                          val x110: Int = helper._1
                          val x111: Array[Double] = helper._2
                          val x112: Array[Double] = helper._3
                          val x32 = 8 * x28
                          val x117 = 4 + x110
                          val x129 = x32 + 4
                          val x118 = 2 * x117
                          val x120 = x118 + 1
                          val x121 = x108(x120)
                          val x113 = 2 * x110
                          val x115 = x113 + 1
                          val x116 = x108(x115)
                          val x132 = x116 - x121
                          val x124 = x116 + x121
                          val x130 = x129 + x110
                          val x133 = 2 * x130
                          val x135 = x133 + 1
                          val x114 = x108(x113)
                          val x119 = x108(x118)
                          val x122 = x32 + x110
                          val x123 = x114 + x119
                          val x131 = x114 - x119
                          val x125 = 2 * x122
                          val x126 = {
                            x26.update(x125, x123); x26
                          }
                          val x127 = x125 + 1
                          val x128 = {
                            x126.update(x127, x124); x126
                          }
                          val x134 = {
                            x128.update(x133, x131); x128
                          }
                          val x136 = {
                            x134.update(x135, x132); x134
                          }
                          (x136)
                        };
                        x26
                      }
                      val x139 = x138
                      (x139)
                    };
                    x26
                  }
                  val x142 = x141
                  val x172 = {
                    for (lc <- 0 until 8) {
                      val helper = (lc, x142, x18)

                      val x144: Int = helper._1
                      val x145: Array[Double] = helper._2
                      val x146: Array[Double] = helper._3
                      val x24 = 16 * x20
                      val x147 = 2 * x144
                      val x156 = x24 + x144
                      val x148 = x142(x147)
                      val x149 = x147 + 1
                      val x159 = 2 * x156
                      val x161 = x159 + 1
                      val x150 = x142(x149)
                      val x151 = 8 + x144
                      val x163 = x24 + 8
                      val x152 = 2 * x151
                      val x164 = x163 + x144
                      val x153 = x142(x152)
                      val x157 = x148 + x153
                      val x160 = {
                        x18.update(x159, x157); x18
                      }
                      val x165 = x148 - x153
                      val x167 = 2 * x164
                      val x154 = x152 + 1
                      val x169 = x167 + 1
                      val x155 = x142(x154)
                      val x158 = x150 + x155
                      val x166 = x150 - x155
                      val x162 = {
                        x160.update(x161, x158); x160
                      }
                      val x168 = {
                        x162.update(x167, x165); x162
                      }
                      val x170 = {
                        x168.update(x169, x166); x168
                      }
                      (x170)
                    };
                    x18
                  }
                  val x173 = x172
                  (x173)
                };
                x18
              }
              val x176 = x175
              val x206 = {
                for (lc <- 0 until 16) {
                  val helper = (lc, x176, x10)

                  val x178: Int = helper._1
                  val x179: Array[Double] = helper._2
                  val x180: Array[Double] = helper._3
                  val x17 = 32 * x13
                  val x185 = 16 + x178
                  val x197 = x17 + 16
                  val x181 = 2 * x178
                  val x198 = x197 + x178
                  val x201 = 2 * x198
                  val x182 = x176(x181)
                  val x203 = x201 + 1
                  val x186 = 2 * x185
                  val x188 = x186 + 1
                  val x189 = x176(x188)
                  val x187 = x176(x186)
                  val x191 = x182 + x187
                  val x199 = x182 - x187
                  val x190 = x17 + x178
                  val x183 = x181 + 1
                  val x193 = 2 * x190
                  val x184 = x176(x183)
                  val x192 = x184 + x189
                  val x194 = {
                    x10.update(x193, x191); x10
                  }
                  val x195 = x193 + 1
                  val x200 = x184 - x189
                  val x196 = {
                    x194.update(x195, x192); x194
                  }
                  val x202 = {
                    x196.update(x201, x199); x196
                  }
                  val x204 = {
                    x202.update(x203, x200); x202
                  }
                  (x204)
                };
                x10
              }
              val x207 = x206
              (x207)
            };
            x10
          }
          val x210 = x209
          val x233 = {
            for (lc <- 0 until 32) {
              val helper = (lc, x210, x2)

              val x212: Int = helper._1
              val x213: Array[Double] = helper._2
              val x214: Array[Double] = helper._3
              val x215 = 2 * x212
              val x219 = 32 + x212
              val x216 = x210(x215)
              val x217 = x215 + 1
              val x220 = 2 * x219
              val x218 = x210(x217)
              val x221 = x210(x220)
              val x222 = x220 + 1
              val x224 = x216 + x221
              val x228 = x216 - x221
              val x223 = x210(x222)
              val x226 = {
                x2.update(x215, x224); x2
              }
              val x225 = x218 + x223
              val x229 = x218 - x223
              val x227 = {
                x226.update(x217, x225); x226
              }
              val x230 = {
                x227.update(x220, x228); x227
              }
              val x231 = {
                x230.update(x222, x229); x230
              }
              (x231)
            };
            x2
          }
          val x234 = x233
          (x234)
        };
        x2
      }
      val x237 = x236

      (x237)
    }

    /** ***************************************
      * End Main
      * ******************************************/
    //bla end!

  }

}

