package SpiralS2


import org.scalameter._
import common._
/**
  * Created by rayda on 27-Dec-16.
  */
object ParallelTest extends App{

  val size = 4096
  val i = 0
  val numTasks = 6

  val one = Complex(1, 0)
  val zero = Complex(0, 1)

  val input = (0 until size).foldLeft(new ComplexVector(new Array[Complex](size))) {
    (acc, ele) => if (ele == i) acc.update(ele, one) else acc.update(ele, zero)
  }
  val out = new ComplexVector(new Array[Complex](size))
  val r = (0 to size by (size/(Math.min(numTasks, size))))

  val ranges1 = (r zip r.tail)
  val last: (Int,Int) = (ranges1.last._1, size)
  val ranges = ranges1.dropRight(1) :+ last



  val tasks = ranges.map( { case (from, to) => common.task(add(input, out, from, to)) } )
  tasks foreach {_.join}


  for (i <- (1 until size)) {
    println(i)
    assert(out(i).im == 2.0 && out(i).re == 0.0, "not working")
  }

  println(out)

  def add(src: ComplexVector, dst: ComplexVector, from: Int, end: Int): Unit = {
    for(x <- from until end){
        dst(x) = src(x) + src(x)
      }
    }


}
