package ca.uqam.euler.nicolas.scala
import scala.annotation.tailrec
import math._

object Util {
  def gcd[@specialized(Int) N](a: N, b: N)(implicit num: Integral[N]): N = {
    import num._
    b match {
      case 0 ⇒ a
      case _ ⇒ gcd(b, a % b)
    }
  }

  def ld[@specialized(Int) N](n: N)(implicit num: Integral[N]) = {
    import num._
    def ldFrom(k: N, n: N): N = {
      if (n % k == 0) k
      else if ((k * k) > n) n
      else ldFrom(k + one, n)
    }
    ldFrom(one + one, n)
  }

  def isPrime[@specialized(Int) N](n: N)(implicit num: Integral[N]) = {
    import num._
    if (n < (one + one)) false
    else ld(n) == n
  }

}