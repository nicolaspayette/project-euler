package ca.uqam.euler.nicolas

/** What is the largest prime factor of the number 600851475143 ? */
/* Answer: 6857 */
object Problem3 {

  def primes = {
    def primesFrom(lastPrime: Int, knownPrimes: Seq[Int]): Stream[Int] = {
      val nextPrime = Stream.from(lastPrime + 1).find(n ⇒ !knownPrimes.exists(n % _ == 0)).get
      Stream.cons(lastPrime, primesFrom(nextPrime, knownPrimes :+ lastPrime))
    }
    primesFrom(2, List())
  }

  def largestPrimeFactor(n: Long) = {
    (math.sqrt(n).toLong to 2 by -1).find(m ⇒ n % m == 0 && isPrime(m))
  }

  def longs = Iterator.iterate(1L)(_ + 1)
  def r(start: Long, end: Long) = longs.dropWhile(_ < start).takeWhile(_ <= end)
  def factors(n: Long) = r(1, n).filter(n % _ == 0L)
  def candidateFactors(n: Long) = factors(n).drop(1).takeWhile(_ <= math.sqrt(n).toLong)
  def isPrime(n: Long) = n > 1 && candidateFactors(n).size == 0

  def primeFactors(n: Long) = candidateFactors(n).filter(isPrime)

  def main(args: Array[String]): Unit = Answer {
    largestPrimeFactor(600851475143L)
  }

}