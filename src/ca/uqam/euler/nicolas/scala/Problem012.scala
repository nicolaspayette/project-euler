package ca.uqam.euler.nicolas.scala

object Problem012 {

  def naturals = {
    def from(n: Int): Stream[Int] = n #:: from(n + 1)
    from(1)
  }

  def triangleNumbers = {
    def from(n: Int): Stream[Int] =
      naturals.take(n).sum #:: from(n + 1)
    from(1)
  }

  def factors(n: Int) = {
    val d = n.toDouble
    var i = 2
    var fs = Seq(n, 1)
    while (i <= scala.math.sqrt(d)) {
      val xd = d / i
      val xi = xd.toInt
      if (xd == xi)
        fs = xi +: i +: fs
      i += 1
    }
    fs
  }
  
  def firstTriangleNumberWithMoreFactorsThan(n: Int) =
    triangleNumbers.find(factors(_).length > n).get

  def main(args: Array[String]): Unit = {
    util.timed {
      println(firstTriangleNumberWithMoreFactorsThan(500))
    }
  }

}