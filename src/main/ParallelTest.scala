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
  val zero = Complex(0, 0)

  val input = (0 until size).foldLeft(new ComplexVector(new Array[Complex](size))) {
    (acc, ele) => if (ele == i) acc.update(ele, one) else acc.update(ele, zero)
  }
  val out = new ComplexVector(new Array[Complex](size))
  val r = 0 to size by (size/(Math.min(numTasks, size)))
  var ranges = r zip r.tail
  val tasks = ranges.map( { case (from, to) => common.task(blur(src, dst, from, to, radius)) } )
  tasks foreach {_.join}
}
