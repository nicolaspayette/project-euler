package ca.uqam.euler.nicolas.scala

/**
 * If p is the perimeter of a right angle triangle with integral length
 * sides, {a,b,c}, there are exactly three solutions for p = 120.
 *
 * {20,48,52}, {24,45,51}, {30,40,50}
 *
 * For which value of p ≤ 1000, is the number of solutions maximised?
 *
 */
object Problem039 {
  import math._
  def nbSolutions(p: Int) =
    (for {
      a ← 1 to p - 2
      b ← a + 1 to p - a - 1
      c = p - a - b
      if (a * a) + (b * b) == (c * c)
    } yield 1).sum

  def main(args: Array[String]): Unit = Answer {
    1 to 1000 maxBy nbSolutions
  }

}