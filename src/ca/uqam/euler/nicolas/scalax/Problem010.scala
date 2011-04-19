package ca.uqam.euler.nicolas.scalax

/** Find the sum of all the primes below two million. */
// caveat: the sum needs to be Long; an Int would overflow
// 142913828922
object Problem10 {

  /** not used for now, but I'm keeping it just in case... */
  def primesDownFrom(n: Int) = {
    require(n > 2)
    def f(primes: Seq[Int], candidates: Seq[Int]): Seq[Int] =
      if (candidates.isEmpty) primes
      else {
        val xs = candidates.filter(_ % primes.head != 0)
        println(xs.head)
        f(xs.head +: primes, xs.tail)
      }
    f(Seq(2), (3 to n).toList)
  }

  /* could be optimzed by stopping at sqrt(n); see solution on projecteuler.net */
  def sumOfprimesDownFrom(n: Int) = {
    require(n > 2)
    def f(sum: Long, nextPrime: Int, candidates: Seq[Int]): Long = {
      //println("Sum: " + sum + "; nextPrime: " + nextPrime + "; candidates: " + candidates)
      if (candidates.isEmpty) sum + nextPrime
      else {
        val xs = candidates.filter(_ % nextPrime != 0)
        f(sum + nextPrime, xs.head, xs.tail)
      }
    }
    f(0, 2, (3 to n).toList)
  }


  def main(args: Array[String]): Unit = {
    val t1 = System.currentTimeMillis
    println(sumOfprimesDownFrom(1000000))
    val t2 = System.currentTimeMillis
    println(t2 - t1 + "ms")
  }

}