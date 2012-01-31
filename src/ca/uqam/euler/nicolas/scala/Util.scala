package ca.uqam.euler.nicolas.scala
import scala.annotation.tailrec
import math._
import Stream._

object Util {

  def memoize[A, B](f: (A) ⇒ B) = {
    var cache = Map[A, B]()
    (x: A) ⇒
      if (cache contains x)
        cache(x)
      else {
        val res = f(x)
        cache += (x -> res)
        res
      }
  }

  def gcd[@specialized(Int) N](a: N, b: N)(implicit num: Integral[N]): N = {
    import num._
    b match {
      case 0 ⇒ a
      case _ ⇒ gcd(b, a % b)
    }
  }

  def leastDivisor[@specialized(Int) N](n: N)(implicit num: Integral[N]) = {
    import num._
    def ldFrom(k: N, n: N): N = {
      if (n % k == 0) k
      else if ((k * k) > n) n
      else ldFrom(k + one, n)
    }
    ldFrom(one + one, n)
  }

  def primes = from(2) filter isPrime

  def isPrime[@specialized(Int) N](n: N)(implicit num: Integral[N]) = {
    import num._
    if (n < (one + one)) false
    else leastDivisor(n) == n
  }

  def nbDigits(n: Int) =
    (floor(log10(abs(n))) + 1).toInt

  def digits(n: Int) =
    for (i ← nbDigits(n) - 1 to 0 by -1)
      yield (n / pow(10, i).toInt) % 10

  def fromDigits(ds: Seq[Int]) =
    ds.reverse.zipWithIndex.map {
      case (n, i) ⇒ n * math.pow(10, i)
    }.sum.toInt

}