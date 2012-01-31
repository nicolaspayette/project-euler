package ca.uqam.euler.nicolas.scala

// Answer : 76576500
object Problem012 {

  def triangleNumbers = {
    def from(n: Int): Stream[Int] =
      (1 to n).sum #:: from(n + 1)
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

  def main(args: Array[String]) = Answer {
    firstTriangleNumberWithMoreFactorsThan(500)
  }

}