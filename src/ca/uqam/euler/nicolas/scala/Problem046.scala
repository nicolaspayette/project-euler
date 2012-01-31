package ca.uqam.euler.nicolas.scala
/**
 * It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
 *
 * 9 = 7 + 2×1^2
 * 15 = 7 + 2×2^2
 * 21 = 3 + 2×3^2
 * 25 = 7 + 2×3^2
 * 27 = 19 + 2×2^2
 * 33 = 31 + 2×1^2
 *
 * It turns out that the conjecture was false.
 *
 * What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
 *
 */
object Problem046 {
  import Util._
  def oddComposites = Stream.from(2).filter(n ⇒ n % 2 != 0 && !isPrime(n))
  val primes = Stream.from(2).filter(isPrime)
  val doubledSquares = Stream.from(1).map(n ⇒ 2 * n * n)
  def goldbach(n: Int) = (for {
    s ← doubledSquares.takeWhile(_ < n)
    p ← primes.takeWhile(_ + s <= n)
  } yield (p + s)).exists(_ == n)
  def main(args: Array[String]): Unit = Answer {
    oddComposites.find(!goldbach(_)).get
  }
}