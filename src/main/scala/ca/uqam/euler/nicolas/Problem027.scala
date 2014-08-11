package ca.uqam.euler.nicolas

/**
 * Euler published the remarkable quadratic formula:
 *
 * n² + n + 41
 *
 * It turns out that the formula will produce 40 primes for
 * the consecutive values n = 0 to 39. However, when
 * n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41,
 * and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.
 *
 * Using computers, the incredible formula  n² − 79n + 1601 was discovered,
 * which produces 80 primes for the consecutive values n = 0 to 79.
 * The product of the coefficients, −79 and 1601, is −126479.
 *
 * Considering quadratics of the form:
 *
 * n² + an + b, where |a| < 1000 and |b| < 1000
 *
 * where |n| is the modulus/absolute value of n
 * e.g. |11| = 11 and |−4| = 4
 *
 * Find the product of the coefficients, a and b, for the
 * quadratic expression that produces the maximum number
 * of primes for consecutive values of n, starting with n = 0.
 *
 */

object Problem027 {

  def isPrime(n: Int) =
    n > 1 && (2 #:: Stream.from(3, 2))
      .takeWhile(d => d * d <= n)
      .forall(n % _ != 0)

  def nbConsecutivePrimes(a: Int, b: Int) =
    Stream.from(0)
      .map(n => (n * n) + (a * n) + b)
      .takeWhile(isPrime)
      .size

  def main(args: Array[String]) = Answer {
    val r = (-999 to 999)
    val pairs = for (a <- r; b <- r) yield (a, b)
    pairs.maxBy((nbConsecutivePrimes _).tupled)
  }

}