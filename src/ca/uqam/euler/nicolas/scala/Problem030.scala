package ca.uqam.euler.nicolas.scala

import BigInt._
object Problem030 {

  val exp = 5

  def test(n: Int) =
    n == n.toString.map(_.asDigit).map(_ pow exp).sum

  val uBound =
    Stream.from(2)
      .map { n => n -> 9.pow(exp) * n }
      .takeWhile { case (n, m) => m.toString.size >= n }
      .last._2.toInt

  def main(args: Array[String]) = Answer {
    (10 to uBound).filter(test).sum
  }

}