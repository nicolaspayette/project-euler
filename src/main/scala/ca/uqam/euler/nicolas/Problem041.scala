package ca.uqam.euler.nicolas

/**
 * We shall say that an n-digit number is pandigital if it
 * makes use of all the digits 1 to n exactly once. For
 * example, 2143 is a 4-digit pandigital and is also prime.
 *
 * What is the largest n-digit pandigital prime that exists?
 *
 */
// Answer: 7652413
object Problem041 {
  def main(args: Array[String]): Unit = Answer {
    def panDigits(n: Int) =
      (1 to n)
        .map(_.toString)
        .mkString
        .permutations
        .map(BigInt.apply)
        .toSet
    (2 to 9)
      .map(panDigits)
      .flatten
      .sorted
      .reverse
      .find(Util.isPrime)
      .get
  }

}