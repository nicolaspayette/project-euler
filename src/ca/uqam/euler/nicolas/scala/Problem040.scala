package ca.uqam.euler.nicolas.scala

/**
 * An irrational decimal fraction is created by concatenating the positive integers:
 *
 * 0.123456789101112131415161718192021...
 *
 * It can be seen that the 12th digit of the fractional part is 1.
 *
 * If dn represents the nth digit of the fractional part, find the value of the following expression.
 *
 * d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
 *
 */
object Problem040 {

  // note: make the stream 0-based cause the d formula is 1-based
  def digits(n: Int = 0, s: String = ""): Stream[Int] =
    s match {
      case "" ⇒ digits(n + 1, n.toString)
      case _  ⇒ s.head.asDigit #:: digits(n, s.tail)
    }

  def main(args: Array[String]): Unit = Answer {
    val d = digits()
    d(1) * d(10) * d(100) * d(1000) * d(10000) * d(100000) * d(1000000)
  }

}