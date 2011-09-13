package ca.uqam.euler.nicolas.scala
import scala.annotation.tailrec
import math._

object Util {
  @tailrec def gcd(a: Int, b: Int): Int = b match {
    case 0 ⇒ a
    case _ ⇒ gcd(b, a % b)
  }

  def ld(n: Int) = {
    @tailrec def ldFrom(k: Int, n: Int): Int =
      if (k % n == 0) k
      else if (pow(k, 2) > n) n
      else ldFrom(k + 1, n)
    ldFrom(2, n)
  }

  def isPrime(n: Int) =
    if (n < 2) false else ld(n) == n

}