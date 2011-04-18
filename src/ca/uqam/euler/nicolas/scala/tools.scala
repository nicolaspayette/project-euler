package ca.uqam.euler.nicolas.scala

object tools {
  def timed(f: => Unit) {
    val start = System.currentTimeMillis
    f
    val stop = System.currentTimeMillis
    val runTime = stop - start
    println("=" * 10 + "\n" + runTime + " ms")
  }
}