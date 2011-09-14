package ca.uqam.euler.nicolas.scala

/**
 * A googol (10^100) is a massive number: one followed by one-hundred zeros;
 * 100^100 is almost unimaginably large: one followed by two-hundred zeros.
 * Despite their size, the sum of the digits in each number is only 1.
 *
 * Considering natural numbers of the form, a^b, where a, b < 100,
 * what is the maximum digital sum?
 */
object Problem056 {

  def main(args: Array[String]): Unit = Answer {
    def dsum(a: Int, b: Int) = BigInt(a).pow(b).toString.map(_ asDigit).sum
    (for {
      a ← 1 to 100
      b ← 1 to 100
    } yield dsum(a, b)).max
  }

}