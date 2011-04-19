package ca.uqam.euler.nicolas.scala

object Problem4 {

  def r(nbDigits: Int) = nbDigits match {
    case 1 => (1 to 9)
    case 2 => (10 to 99)
    case 3 => (100 to 999)
    case 4 => (1000 to 9999)
    case _ => error("Not implemented yet!")
  }

  def isPalindrome(n: Int) = {
    val s = n.toString
    s == s.reverse
  }

  def largestForNbDigits(n: Int) = {
    r(n).flatMap(x =>
      r(n).takeWhile(_ <= x).
        map(_ * x)).filter(isPalindrome).max
  }

  def main(args: Array[String]): Unit = {
    println(largestForNbDigits(3))
  }

}