package ca.uqam.euler.nicolas

/**
 * The prime 41, can be written as the sum of six consecutive primes:
 * 41 = 2 + 3 + 5 + 7 + 11 + 13
 *
 * This is the longest sum of consecutive primes that adds to a prime below one-hundred.
 *
 * The longest sum of consecutive primes below one-thousand that adds to a prime,
 * contains 21 terms, and is equal to 953.
 *
 * Which prime, below one-million, can be written as the sum of the most consecutive primes?
 *
 */
object Problem050 {
  import Util._
  def main(args: Array[String]) = Answer {
    val under = 1000000
    val ps = primes
    def sums(n: Int) = (ps sliding n) takeWhile (_.sum <= under) filter (xs ⇒ isPrime(xs.sum))
    Stream.from(5).takeWhile(ps.take(_).sum <= under).map(sums).flatMap(_.toList).maxBy(_.size).sum
  }

}