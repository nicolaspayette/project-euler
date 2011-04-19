package ca.uqam.euler.nicolas.scala

object Problem029 {

  def main(args: Array[String]): Unit = {
    tools.timed {
      val r = 2 to 100
      val ps = for (a <- r; b <- r) yield (a, b)
      val xs = ps.map(p => BigInt(p._1) pow p._2).toSet
      println(xs.size)
    }
  }

}