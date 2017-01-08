/*
import org.scalameter._

import scala.util.Random

object Settings {


  val standardConfig = config(
    Key.exec.minWarmupRuns -> 100,
    Key.exec.maxWarmupRuns -> 200,
    Key.exec.benchRuns -> 1000,
    Key.verbose -> false
  ) withWarmer (new Warmer.Default)
}

object AutoVecTest extends App {

  val rand = new Random()
  val arr = new Array[Double](4*200)
  val arr2 = new Array[Double](4*200)
  val arr3 = new Array[Double](4*200)
  for (i <- 0 until arr.size)
    arr(i) = rand.nextDouble()



 for (i <- 0 until 10) {
   val seqtime = Settings.standardConfig measure {
     for (i <- 0 until arr.size)
       arr2(i) = 3.0 * arr(i) + 2.3
   }
   println(seqtime)

   val vectime = Settings.standardConfig measure {
     for (j <- 0 until arr.size/4) {
       val i = j * 4
       val a = arr(i)
       val b = arr(i+1)
       val c = arr(i+2)
       val d = arr(i+3)

       val x1 = 3.0 * a + 2.3
       val x2 = 3.0 * b + 2.3
       val x3 = 3.0 * c + 2.3
       val x4 = 3.0 * d + 2.3

       arr3(i) = x1
       arr3(i+1) = x2
       arr3(i+2) = x3
       arr3(i+3) = x4

     }
   }
   println(vectime)

   println("seq first-----------------")
   println(100/seqtime*vectime)
 }

  for (i <- 0 until 10) {


    val vectime = Settings.standardConfig measure {
      for (j <- 0 until arr.size/4) {
        val i = j * 4
        val a = arr(i)
        val b = arr(i+1)
        val c = arr(i+2)
        val d = arr(i+3)

        val x1 = 3.0 * a + 2.3
        val x2 = 3.0 * b + 2.3
        val x3 = 3.0 * c + 2.3
        val x4 = 3.0 * d + 2.3

        arr3(i) = x1
        arr3(i+1) = x2
        arr3(i+2) = x3
        arr3(i+3) = x4

      }
    }
    println(vectime)

    val seqtime = Settings.standardConfig measure {
      for (i <- 0 until arr.size)
        arr2(i) = 3.0 * arr(i) + 2.3
    }
    println(seqtime)

    println("-----------------")
    println(100/seqtime*vectime)
    
  }

  for (i <- 0 until arr.size)
    assert(arr2(i) == arr3(i))
}
*/
