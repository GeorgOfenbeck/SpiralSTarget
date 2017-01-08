

import scala.annotation.tailrec
import scala.util.Random
import org.scalameter._

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
    val id2ids: Map[Int, (Int, Int)] = Map(0 -> (-1, -1), 1 -> (0, 0))
    val ids2id: Map[(Int, Int), Int] = Map((-1, -1) -> 0, (0, 0) -> 1)
    val id2radix: Map[Int, Int] = Map(0 -> 2, 1 -> 4)
    val size2id: Map[Int, Int] = Map(4 -> 0, 16 -> 1)
    val id2size: Map[Int, Int] = Map(0 -> 4, 1 -> 16)
    val validate = true
    //true
    val WHT = false
    var sanitycheck: Int = 0

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 1000,
      Key.exec.maxWarmupRuns -> 1000,
      Key.exec.benchRuns -> 10000,
      Key.verbose -> false
    ) withWarmer (new Warmer.Default)
  }


  object Twiddle {
    var precompbuffer: Vector[Complex] = Vector.empty
    var dprecompbuffer: Vector[Double] = Vector.empty
    var precomp: Array[Complex] = null
    var dprecomp: Array[Array[Array[Double]]] = null
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

    def dload(n: Int, d: Int, k: Int, i: Int, re: Int): Double = {
      val nidx = Integer.numberOfTrailingZeros(n)
      val didx = Integer.numberOfTrailingZeros(d)
      dprecomp(nidx)(didx)(2 * i + re)
    }

    def TMap2preComp(): Array[Array[Array[Double]]] = {
      TMap.keys.foldLeft(Array.empty[Array[Array[Double]]])((acc, ele) => {
        val (n, d, k) = ele
        val nidx = Integer.numberOfTrailingZeros(n)
        val didx = Integer.numberOfTrailingZeros(d)

        val nsav: Array[Array[Array[Double]]] = if (acc.size < (nidx + 1)) {
          //make bigger
          val bigger = new Array[Array[Array[Double]]](nidx + 1)
          for (j <- 0 until acc.size)
            bigger(j) = acc(j)
          bigger
        } else acc

        val dsav: Array[Array[Double]] = if (nsav(nidx) == null) {
          nsav(nidx) = new Array[Array[Double]](didx + 1)
          nsav(nidx)
        } else {
          val dold = nsav(nidx)
          val dnew = if (dold.size < (didx + 1)) {
            //make bigger
            val bigger = new Array[Array[Double]](didx + 1)
            for (j <- 0 until dold.size)
              bigger(j) = dold(j)
            bigger
          } else dold
          nsav(nidx) = dnew
          dnew
        }
        val cont = TMap((n, d, k)).save.flatMap(e => Array(e.re, e.im))
        dsav(didx) = cont
        nsav
      })
    }

    def parloop(size: Int, numTasks: Int, ini: Array[Double], out: Array[Double], body: (Int) => Array[Double]): Array[Double] = {
      val r = (0 to size by (size / (Math.min(numTasks, size))))
      val ranges1 = (r zip r.tail)
      val last: (Int, Int) = (ranges1.last._1, size)
      val ranges = ranges1.dropRight(1) :+ last

      def blub(r: Range): Unit = {
        r.map(
          p => {
            val t = (p)
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

    def parloopold(size: Int, numTasks: Int, ini: ComplexVector, out: ComplexVector, body: ((ComplexVector, Int)) => ComplexVector): ComplexVector = {
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

    val iterateover = (4 until 5)
    val timings = for (twopower <- iterateover) yield {
      val size = 16

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
        println("pre comp done")
        Twiddle.precomp = Twiddle.precompbuffer.toArray
        Twiddle.dprecomp = Twiddle.TMap2preComp()
        val txy = for (j <- 0 until 10) yield {
          Settings.standardConfig measure {
            Settings.sanitycheck = 0
            Twiddle.twindex = 0
            //VARIOUS CALLS HERE
            val resx = t.apply(input.save, out.save)
            val res = new InterleavedVector(resx) //, 0, instride, 0, instride, Vector.empty)     // VARIOUS CALLS HERE END

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

        val txy2 = for (j <- 0 until 10) yield {
          Settings.standardConfig measure {
            Settings.sanitycheck = 0
            Twiddle.twindex = 0
            //VARIOUS CALLS HERE
            val resx = JTest.apply(input.save, out.save)
            val res = new InterleavedVector(resx) //, 0, instride, 0, instride, Vector.empty)     // VARIOUS CALLS HERE END

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

        {
          val arr = new Array[Double](32)
          val arr2 = new Array[Double](32)
          var i = 0
          while (i < 32) {
            {
              arr(i) = i * 3.3
              arr2(i) = 0.734
            }
            {
              i += 1;
              i - 1
            }
          }
          val repeats_inner = 10000
          var min_time = Double.MaxValue
          var j = 0
          while (j < 1000) {
            {
              var elapsedTime = System.nanoTime
              var i = 0
              while (i < repeats_inner) {
                {
                  JTest.apply(arr, arr2)
                }
                {
                  i += 1;
                  i - 1
                }
              }
              elapsedTime = System.nanoTime - elapsedTime
              elapsedTime = elapsedTime / repeats_inner
              if (elapsedTime < min_time) min_time = elapsedTime
            }
            {
              j += 1;
              j - 1
            }
          }
          val flop = 5 * 16.0 * Math.log10(16) / Math.log10(2)
          def gf(d: Double) = flop/ (d*1000000)
          val gflops = gf(min_time/1000000)
          System.out.println(flop + " flops " + min_time + " time " + gflops + "gflops")
        }

        val scalatime = txy.min
        val javatime = txy2.min

        val flops = 16 * 5 * Math.log10(16)/Math.log10(2)
        def gf(d: Double) = flops/ (d*1000000)
        println(s"${gf(scalatime)}  vs ${gf(javatime)}")
        scalatime

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
  class testClass {
    def apply(x1: Array[Double], x2: Array[Double]): ((Array[Double])) = {

      val x25 = x1(25)
      val x57 = x1(26)
      val x121 = x1(31)
      val x117 = x1(15)
      val x125 = x117 - x121
      val x84 = x1(12)
      val x10 = x1(16)
      val x110 = x1(23)
      val x78 = x1(21)
      val x106 = x1(7)
      val x73 = x1(4)
      val x45 = x1(19)
      val x54 = x1(11)
      val x115 = x1(14)
      val x7 = x1(1)
      val x5 = x1(0)
      val x13 = x5 + x10
      val x130 = 0 - x125
      val x112 = x106 + x110
      val x123 = x117 + x121
      val x129 = x112 - x123
      val x217 = -0.7071067811865476 * x129
      val x74 = x1(5)
      val x80 = x74 + x78
      val x127 = x112 + x123
      val x12 = x1(17)
      val x14 = x7 + x12
      val x16 = x7 - x12
      val x43 = x1(18)
      val x88 = x1(28)
      val x93 = x84 - x88
      val x91 = x84 + x88
      val x41 = x1(3)
      val x49 = x41 - x45
      val x85 = x1(13)
      val x114 = x106 - x110
      val x23 = x1(24)
      val x18 = x1(8)
      val x28 = x18 - x23
      val x38 = x16 - x28
      val x26 = x18 + x23
      val x32 = x13 - x26
      val x76 = x1(20)
      val x81 = x73 - x76
      val x119 = x1(30)
      val x124 = x115 - x119
      val x132 = x114 + x124
      val x134 = x114 - x124
      val x260 = -0.3826834323650898 * x134
      val x180 = 0.3826834323650898 * x132
      val x59 = x1(27)
      val x61 = x54 + x59
      val x63 = x54 - x59
      val x82 = x74 - x78
      val x101 = x82 + x93
      val x103 = x82 - x93
      val x244 = -0.7071067811865476 * x103
      val x162 = 0.7071067811865476 * x101
      val x36 = x16 + x28
      val x262 = -0.9238795325112867 * x134
      val x30 = x13 + x26
      val x52 = x1(10)
      val x60 = x52 + x57
      val x20 = x1(9)
      val x29 = x20 - x25
      val x34 = 0 - x29
      val x27 = x20 + x25
      val x33 = x14 - x27
      val x31 = x14 + x27
      val x108 = x1(22)
      val x242 = 0.7071067811865476 * x103
      val x39 = x1(2)
      val x46 = x39 + x43
      val x64 = x46 + x60
      val x66 = x46 - x60
      val x209 = 0.7071067811865476 * x66
      val x48 = x39 - x43
      val x79 = x73 + x76
      val x97 = x79 - x91
      val x206 = x33 + x97
      val x208 = x33 - x97
      val x95 = x79 + x91
      val x137 = x30 - x95
      val x135 = x30 + x95
      val x47 = x41 + x45
      val x65 = x47 + x61
      val x142 = x65 - x127
      val x140 = x65 + x127
      val x67 = x47 - x61
      val x151 = 0 - x142
      val x152 = x137 + x151
      val x156 = x137 - x151
      val x210 = 0.7071067811865476 * x67
      val x211 = x209 - x210
      val x212 = x210 + x209
      val x15 = x5 - x10
      val x37 = x15 - x34
      val x35 = x15 + x34
      val x68 = 0 - x63
      val x69 = x48 + x68
      val x71 = x48 - x68
      val x251 = 0.3826834323650898 * x71
      val x255 = 0.9238795325112867 * x71
      val x175 = 0.3826834323650898 * x69
      val x171 = 0.9238795325112867 * x69
      val x62 = x52 - x57
      val x70 = x49 + x62
      val x174 = 0.9238795325112867 * x70
      val x176 = x174 + x175
      val x172 = 0.3826834323650898 * x70
      val x173 = x171 - x172
      val x72 = x49 - x62
      val x252 = 0.9238795325112867 * x72
      val x253 = x251 - x252
      val x178 = 0.9238795325112867 * x132
      val x90 = x1(29)
      val x92 = x85 + x90
      val x96 = x80 + x92
      val x138 = x31 - x96
      val x98 = x80 - x92
      val x204 = 0 - x98
      val x205 = x32 + x204
      val x207 = x32 - x204
      val x136 = x31 + x96
      val x148 = x136 - x140
      val x144 = x136 + x140
      val x94 = x85 - x90
      val x99 = 0 - x94
      val x102 = x81 - x99
      val x245 = 0.7071067811865476 * x102
      val x241 = -0.7071067811865476 * x102
      val x243 = x241 - x242
      val x249 = x37 - x243
      val x247 = x37 + x243
      val x246 = x244 + x245
      val x248 = x38 + x246
      val x250 = x38 - x246
      val x254 = 0.3826834323650898 * x72
      val x256 = x254 + x255
      val x122 = x115 + x119
      val x215 = 0.7071067811865476 * x129
      val x100 = x81 + x99
      val x104 = x1(6)
      val x161 = 0.7071067811865476 * x100
      val x164 = x162 + x161
      val x166 = x36 + x164
      val x113 = x104 - x108
      val x133 = x113 - x130
      val x259 = -0.9238795325112867 * x133
      val x261 = x259 - x260
      val x265 = x253 + x261
      val x269 = x247 + x265
      val x267 = x253 - x261
      val x279 = x250 + x267
      val x263 = -0.3826834323650898 * x133
      val x264 = x262 + x263
      val x266 = x256 + x264
      val x270 = x248 + x266
      val x274 = x248 - x266
      val x168 = x36 - x164
      val x273 = x247 - x265
      val x268 = x256 - x264
      val x283 = x250 - x267
      val x131 = x113 + x130
      val x181 = 0.9238795325112867 * x131
      val x177 = 0.3826834323650898 * x131
      val x179 = x177 - x178
      val x185 = x173 - x179
      val x201 = x168 - x185
      val x197 = x168 + x185
      val x182 = x180 + x181
      val x184 = x176 + x182
      val x192 = x166 - x184
      val x188 = x166 + x184
      val x186 = x176 - x182
      val x183 = x173 + x179
      val x195 = 0 - x186
      val x163 = x161 - x162
      val x165 = x35 + x163
      val x191 = x165 - x183
      val x187 = x165 + x183
      val x167 = x35 - x163
      val x196 = x167 + x195
      val x200 = x167 - x195
      val x277 = 0 - x268
      val x111 = x104 + x108
      val x128 = x111 - x122
      val x278 = x249 + x277
      val x282 = x249 - x277
      val x218 = 0.7071067811865476 * x128
      val x214 = -0.7071067811865476 * x128
      val x126 = x111 + x122
      val x216 = x214 - x215
      val x220 = x211 + x216
      val x228 = x205 - x220
      val x224 = x205 + x220
      val x141 = x64 - x126
      val x153 = x138 + x141
      val x157 = x138 - x141
      val x219 = x217 + x218
      val x221 = x212 + x219
      val x229 = x206 - x221
      val x225 = x206 + x221
      val x223 = x212 - x219
      val x139 = x64 + x126
      val x147 = x135 - x139
      val x143 = x135 + x139
      val x222 = x211 - x216
      val x234 = x208 + x222
      val x238 = x208 - x222
      val x145 = {
        x2.update(0, x143); x2
      }
      val x232 = 0 - x223
      val x146 = {
        x145.update(1, x144); x145
      }
      val x233 = x207 + x232
      val x237 = x207 - x232
      val x149 = {
        x146.update(16, x147); x146
      }
      val x150 = {
        x149.update(17, x148); x149
      }
      val x154 = {
        x150.update(8, x152); x150
      }
      val x155 = {
        x154.update(9, x153); x154
      }
      val x158 = {
        x155.update(24, x156); x155
      }
      val x159 = {
        x158.update(25, x157); x158
      }
      val x189 = {
        x159.update(2, x187); x159
      }
      val x190 = {
        x189.update(3, x188); x189
      }
      val x193 = {
        x190.update(18, x191); x190
      }
      val x194 = {
        x193.update(19, x192); x193
      }
      val x198 = {
        x194.update(10, x196); x194
      }
      val x199 = {
        x198.update(11, x197); x198
      }
      val x202 = {
        x199.update(26, x200); x199
      }
      val x203 = {
        x202.update(27, x201); x202
      }
      val x226 = {
        x203.update(4, x224); x203
      }
      val x227 = {
        x226.update(5, x225); x226
      }
      val x230 = {
        x227.update(20, x228); x227
      }
      val x231 = {
        x230.update(21, x229); x230
      }
      val x235 = {
        x231.update(12, x233); x231
      }
      val x236 = {
        x235.update(13, x234); x235
      }
      val x239 = {
        x236.update(28, x237); x236
      }
      val x240 = {
        x239.update(29, x238); x239
      }
      val x271 = {
        x240.update(6, x269); x240
      }
      val x272 = {
        x271.update(7, x270); x271
      }
      val x275 = {
        x272.update(22, x273); x272
      }
      val x276 = {
        x275.update(23, x274); x275
      }
      val x280 = {
        x276.update(14, x278); x276
      }
      val x281 = {
        x280.update(15, x279); x280
      }
      val x284 = {
        x281.update(30, x282); x281
      }
      val x285 = {
        x284.update(31, x283); x284
      }

      (x285)
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
  class PreCompute {
    def apply(x1: Array[Double], x2: Array[Double]): ((Array[Double])) = {

      val x25 = x1(25)
      val x57 = x1(26)
      val x121 = x1(31)
      val x117 = x1(15)
      val x125 = x117 - x121
      val x84 = x1(12)
      val x10 = x1(16)
      val x110 = x1(23)
      val x78 = x1(21)
      val x106 = x1(7)
      val x73 = x1(4)
      val x45 = x1(19)
      val x54 = x1(11)
      val x115 = x1(14)
      val x7 = x1(1)
      val x5 = x1(0)
      val x13 = x5 + x10
      val x130 = 0 - x125
      val x112 = x106 + x110
      val x123 = x117 + x121
      val x129 = x112 - x123
      val x217 = -0.7071067811865476 * x129
      val x74 = x1(5)
      val x80 = x74 + x78
      val x127 = x112 + x123
      val x12 = x1(17)
      val x14 = x7 + x12
      val x16 = x7 - x12
      val x43 = x1(18)
      val x88 = x1(28)
      val x93 = x84 - x88
      val x91 = x84 + x88
      val x41 = x1(3)
      val x49 = x41 - x45
      val x85 = x1(13)
      val x114 = x106 - x110
      val x23 = x1(24)
      val x18 = x1(8)
      val x28 = x18 - x23
      val x38 = x16 - x28
      val x26 = x18 + x23
      val x32 = x13 - x26
      val x76 = x1(20)
      val x81 = x73 - x76
      val x119 = x1(30)
      val x124 = x115 - x119
      val x132 = x114 + x124
      val x134 = x114 - x124
      val x260 = -0.3826834323650898 * x134
      val x180 = 0.3826834323650898 * x132
      val x59 = x1(27)
      val x61 = x54 + x59
      val x63 = x54 - x59
      val x82 = x74 - x78
      val x101 = x82 + x93
      val x103 = x82 - x93
      val x244 = -0.7071067811865476 * x103
      val x162 = 0.7071067811865476 * x101
      val x36 = x16 + x28
      val x262 = -0.9238795325112867 * x134
      val x30 = x13 + x26
      val x52 = x1(10)
      val x60 = x52 + x57
      val x20 = x1(9)
      val x29 = x20 - x25
      val x34 = 0 - x29
      val x27 = x20 + x25
      val x33 = x14 - x27
      val x31 = x14 + x27
      val x108 = x1(22)
      val x242 = 0.7071067811865476 * x103
      val x39 = x1(2)
      val x46 = x39 + x43
      val x64 = x46 + x60
      val x66 = x46 - x60
      val x209 = 0.7071067811865476 * x66
      val x48 = x39 - x43
      val x79 = x73 + x76
      val x97 = x79 - x91
      val x206 = x33 + x97
      val x208 = x33 - x97
      val x95 = x79 + x91
      val x137 = x30 - x95
      val x135 = x30 + x95
      val x47 = x41 + x45
      val x65 = x47 + x61
      val x142 = x65 - x127
      val x140 = x65 + x127
      val x67 = x47 - x61
      val x151 = 0 - x142
      val x152 = x137 + x151
      val x156 = x137 - x151
      val x210 = 0.7071067811865476 * x67
      val x211 = x209 - x210
      val x212 = x210 + x209
      val x15 = x5 - x10
      val x37 = x15 - x34
      val x35 = x15 + x34
      val x68 = 0 - x63
      val x69 = x48 + x68
      val x71 = x48 - x68
      val x251 = 0.3826834323650898 * x71
      val x255 = 0.9238795325112867 * x71
      val x175 = 0.3826834323650898 * x69
      val x171 = 0.9238795325112867 * x69
      val x62 = x52 - x57
      val x70 = x49 + x62
      val x174 = 0.9238795325112867 * x70
      val x176 = x174 + x175
      val x172 = 0.3826834323650898 * x70
      val x173 = x171 - x172
      val x72 = x49 - x62
      val x252 = 0.9238795325112867 * x72
      val x253 = x251 - x252
      val x178 = 0.9238795325112867 * x132
      val x90 = x1(29)
      val x92 = x85 + x90
      val x96 = x80 + x92
      val x138 = x31 - x96
      val x98 = x80 - x92
      val x204 = 0 - x98
      val x205 = x32 + x204
      val x207 = x32 - x204
      val x136 = x31 + x96
      val x148 = x136 - x140
      val x144 = x136 + x140
      val x94 = x85 - x90
      val x99 = 0 - x94
      val x102 = x81 - x99
      val x245 = 0.7071067811865476 * x102
      val x241 = -0.7071067811865476 * x102
      val x243 = x241 - x242
      val x249 = x37 - x243
      val x247 = x37 + x243
      val x246 = x244 + x245
      val x248 = x38 + x246
      val x250 = x38 - x246
      val x254 = 0.3826834323650898 * x72
      val x256 = x254 + x255
      val x122 = x115 + x119
      val x215 = 0.7071067811865476 * x129
      val x100 = x81 + x99
      val x104 = x1(6)
      val x161 = 0.7071067811865476 * x100
      val x164 = x162 + x161
      val x166 = x36 + x164
      val x113 = x104 - x108
      val x133 = x113 - x130
      val x259 = -0.9238795325112867 * x133
      val x261 = x259 - x260
      val x265 = x253 + x261
      val x269 = x247 + x265
      val x267 = x253 - x261
      val x279 = x250 + x267
      val x263 = -0.3826834323650898 * x133
      val x264 = x262 + x263
      val x266 = x256 + x264
      val x270 = x248 + x266
      val x274 = x248 - x266
      val x168 = x36 - x164
      val x273 = x247 - x265
      val x268 = x256 - x264
      val x283 = x250 - x267
      val x131 = x113 + x130
      val x181 = 0.9238795325112867 * x131
      val x177 = 0.3826834323650898 * x131
      val x179 = x177 - x178
      val x185 = x173 - x179
      val x201 = x168 - x185
      val x197 = x168 + x185
      val x182 = x180 + x181
      val x184 = x176 + x182
      val x192 = x166 - x184
      val x188 = x166 + x184
      val x186 = x176 - x182
      val x183 = x173 + x179
      val x195 = 0 - x186
      val x163 = x161 - x162
      val x165 = x35 + x163
      val x191 = x165 - x183
      val x187 = x165 + x183
      val x167 = x35 - x163
      val x196 = x167 + x195
      val x200 = x167 - x195
      val x277 = 0 - x268
      val x111 = x104 + x108
      val x128 = x111 - x122
      val x278 = x249 + x277
      val x282 = x249 - x277
      val x218 = 0.7071067811865476 * x128
      val x214 = -0.7071067811865476 * x128
      val x126 = x111 + x122
      val x216 = x214 - x215
      val x220 = x211 + x216
      val x228 = x205 - x220
      val x224 = x205 + x220
      val x141 = x64 - x126
      val x153 = x138 + x141
      val x157 = x138 - x141
      val x219 = x217 + x218
      val x221 = x212 + x219
      val x229 = x206 - x221
      val x225 = x206 + x221
      val x223 = x212 - x219
      val x139 = x64 + x126
      val x147 = x135 - x139
      val x143 = x135 + x139
      val x222 = x211 - x216
      val x234 = x208 + x222
      val x238 = x208 - x222
      val x145 = {
        x2.update(0, x143); x2
      }
      val x232 = 0 - x223
      val x146 = {
        x145.update(1, x144); x145
      }
      val x233 = x207 + x232
      val x237 = x207 - x232
      val x149 = {
        x146.update(16, x147); x146
      }
      val x150 = {
        x149.update(17, x148); x149
      }
      val x154 = {
        x150.update(8, x152); x150
      }
      val x155 = {
        x154.update(9, x153); x154
      }
      val x158 = {
        x155.update(24, x156); x155
      }
      val x159 = {
        x158.update(25, x157); x158
      }
      val x189 = {
        x159.update(2, x187); x159
      }
      val x190 = {
        x189.update(3, x188); x189
      }
      val x193 = {
        x190.update(18, x191); x190
      }
      val x194 = {
        x193.update(19, x192); x193
      }
      val x198 = {
        x194.update(10, x196); x194
      }
      val x199 = {
        x198.update(11, x197); x198
      }
      val x202 = {
        x199.update(26, x200); x199
      }
      val x203 = {
        x202.update(27, x201); x202
      }
      val x226 = {
        x203.update(4, x224); x203
      }
      val x227 = {
        x226.update(5, x225); x226
      }
      val x230 = {
        x227.update(20, x228); x227
      }
      val x231 = {
        x230.update(21, x229); x230
      }
      val x235 = {
        x231.update(12, x233); x231
      }
      val x236 = {
        x235.update(13, x234); x235
      }
      val x239 = {
        x236.update(28, x237); x236
      }
      val x240 = {
        x239.update(29, x238); x239
      }
      val x271 = {
        x240.update(6, x269); x240
      }
      val x272 = {
        x271.update(7, x270); x271
      }
      val x275 = {
        x272.update(22, x273); x272
      }
      val x276 = {
        x275.update(23, x274); x275
      }
      val x280 = {
        x276.update(14, x278); x276
      }
      val x281 = {
        x280.update(15, x279); x280
      }
      val x284 = {
        x281.update(30, x282); x281
      }
      val x285 = {
        x284.update(31, x283); x284
      }

      (x285)
    }

    /** ***************************************
      * End Main
      * ******************************************/
    //bla end!

  }

}

