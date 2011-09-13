package ca.uqam.euler.nicolas.scala

/**
 * If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
 * The sum of these multiples is 23.
 *
 * Find the sum of all the multiples of 3 or 5 below 1000.
 *
 */

object Problem001 {

  def sumOfMultiples(upperBound: Int, multipliers: Int*) = {
    def isMultiple(x: Int) = multipliers.exists(x % _ == 0.0)
    (1 until upperBound).filter(isMultiple).sum
  }
  def main(args: Array[String]) = Answer {
    sumOfMultiples(1000, 3, 5)
  }

}