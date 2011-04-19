package ca.uqam.euler.nicolas.scala

/** What is the 10001st prime number? */
/* Answer: 104743 */
object Problem7 {

  def primes = {
    def primesFrom(lastPrime: Int, knownPrimes: Seq[Int]): Stream[Int] = {
      val nextPrime = Stream.from(lastPrime + 1).find(n => !knownPrimes.exists(n % _ == 0)).get
      Stream.cons(lastPrime, primesFrom(nextPrime, knownPrimes :+ lastPrime))
    }
    primesFrom(2, List())
  }

  def nthPrime(n: Int) = {
	// for some weird reason, primes.apply or primes.drop 
	//  runs out of heap space, so we do it like this: 
    def f(i: Int, stream: Stream[Int]): Int = {    	
    	if (i == n) stream.head
    	else f(i + 1, stream.tail)
    }
    f(1, primes)
  }

  def main(args: Array[String]): Unit = {
    println(nthPrime(10001))
  }

}