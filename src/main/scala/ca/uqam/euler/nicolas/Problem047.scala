package ca.uqam.euler.nicolas

object Problem047 {

  import Util._

  def primeFactors(n: Int): List[Int] =
    if (isPrime(n))
      List(n)
    else {
      val ld = leastDivisor(n)
      ld :: primeFactors(n / ld)
    }

  def main(args: Array[String]) = Answer {
    Stream.from(2)
      .sliding(4)
      .find(_.forall(primeFactors(_).distinct.size == 4))
      .get.head
  }
}