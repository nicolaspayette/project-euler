package ca.uqam.euler.nicolas

/**
 * A perfect number is a number for which the sum of its proper
 * divisors is exactly equal to the number. For example, the sum
 * of the proper divisors of 28 would be
 *
 * 1 + 2 + 4 + 7 + 14 = 28,
 *
 * which means that 28 is a perfect number.
 * A number n is called deficient if the sum of its proper divisors
 * is less than n and it is called abundant if this sum exceeds n.
 * As 12 is the smallest abundant number,
 *
 * 1 + 2 + 3 + 4 + 6 = 16,
 *
 * the smallest number that can be written
 * as the sum of two abundant numbers is 24. By mathematical analysis,
 * it can be shown that all integers greater than 28123 can be written
 * as the sum of two abundant numbers. However, this upper limit cannot
 * be reduced any further by analysis even though it is known that the
 * greatest number that cannot be expressed as the sum of two abundant
 * numbers is less than this limit. Find the sum of all the positive
 * integers which cannot be written as the sum of two abundant numbers.
 *
 */
import scala.Stream

object Problem023 {

  def properDivisors(n: Long) = {
    @scala.annotation.tailrec
    def f(next: Long, divs: Seq[Long], limit: Long): Seq[Long] = {
      if (next >= limit)
        divs
      else if (n % next == 0) {
        val c = (n / next)
        val found =
          if (c != next)
            c +: next +: divs
          else
            next +: divs
        f(next + 1, found, c)
      } else
        f(next + 1, divs, limit)
    }
    f(2, Seq(1), Math.sqrt(n).toInt)
  }

  val m = collection.mutable.Map[Long, Boolean]()
  def isAboundant(n: Long) = {
    if (!m.contains(n))
      m(n) = (properDivisors(n).sum > n)
    m(n)
  }

  def sumsOf(n: Long) = {
    def f(a: Long, b: Long): Stream[Tuple2[Long, Long]] =
      (a, b) #:: (if (b - a > 1) f(a + 1, b - 1) else Stream.empty)
    f(1, n - 1)
  }

  def main(args: Array[String]): Unit = {
    val ns = (1 to 28123).filter { n =>
      sumsOf(n).forall { s =>
        !(isAboundant(s._1) && isAboundant(s._2))
      }
    }
    println(ns.sum)
  }

}