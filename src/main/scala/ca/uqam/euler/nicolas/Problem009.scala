package ca.uqam.euler.nicolas

/**
 *
 * A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
 * a^(2) + b^(2) = c^(2)
 * For example, 3^(2) + 4^(2) = 9 + 16 = 25 = 5^(2).
 * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 * Find the product abc.
 *
 */
object Problem9 {

  case class Triplet(val a: Int, val b: Int) {
    lazy val c = math.sqrt((a * a) + (b * b))
    def sum = a + b + c
    def product = a * b * c
  }

  def findTriplet(targetSum: Int) = {
    (1 to targetSum).flatMap { a =>
      (a + 1 to targetSum - a).map { b =>
        Triplet(a, b)
      }
    }.find(_.sum == targetSum).get
  }

  def main(args: Array[String]): Unit = {
    println(findTriplet(1000))
    println(findTriplet(1000).product.toInt)
  }

}