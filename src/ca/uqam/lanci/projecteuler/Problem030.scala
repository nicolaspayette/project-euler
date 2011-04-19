package ca.uqam.lanci.projecteuler
import scala.math.pow
/**
 * 
 * Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
 *
 *  1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)
 *  8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)
 *  9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)
 *
 * As 1 = 1^(4) is not a sum it is not included.
 *
 * The sum of these numbers is 1634 + 8208 + 9474 = 19316.
 *
 * Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
 *
 */
object Problem030 {
  // I don't know how to get an upper limit...
  def digits(n: Long) = n.toString.map(_.asDigit)
  def sumOfPowers(n: Long, p: Int) = digits(n).map(pow(_, p)).sum
  def isSumOfPowers(n: Long, p: Int) = n == sumOfPowers(n, p)
  def main(args: Array[String]): Unit = {
    util.timed {
    	(1 to 9).map(pow(_,4)).mkString("\n")
      for (i <- 10 to Int.MaxValue if isSumOfPowers(i, 5)) println(i)
    }
  }

}