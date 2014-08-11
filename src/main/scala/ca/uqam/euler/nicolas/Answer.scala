package ca.uqam.euler.nicolas

import System.currentTimeMillis
object Answer {

  def timed[A](f: => A) = {
    val t1 = currentTimeMillis
    val ret = f
    val t2 = currentTimeMillis
    (ret, t2 - t1)
  }

  def apply[A](f: => A) = {
    val (ret, runTime) = timed(f)
    val str = "Time   : " + runTime + " ms\nAnswer : " + ret
    println("=" * str.lines.maxBy(_.length).length + "\n" + str)
  }
}