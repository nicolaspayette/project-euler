package ca.uqam.euler.nicolas.scala

import BigInt._
object Problem030 {

  def test(n: Long, exp: Int) =
    n == n.toString.map(_.asDigit).map(_ pow exp).sum

  def upperBound(exp: Int) =
    Stream.from(2)
      .map { n => n -> 9.pow(exp) * n }
      .takeWhile { case (n, m) => m.toString.size >= n }
      .last._2.toInt

  def main(args: Array[String]): Unit = tools.timed {
    val exp = 5
    val answer = (10 to upperBound(exp)).filter(test(_, exp)).sum
    println(answer)
  }

}