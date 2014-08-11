package ca.uqam.euler.nicolas
import scala.collection.immutable.SortedSet

/**
 * Take the number 192 and multiply it by each of 1, 2, and 3:
 *
 * 192 × 1 = 192
 * 192 × 2 = 384
 * 192 × 3 = 576
 *
 * By concatenating each product we get the 1 to 9 pandigital, 192384576.
 * We will call 192384576 the concatenated product of 192 and (1,2,3)
 *
 * The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5,
 * giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
 *
 * What is the largest 1 to 9 pandigital 9-digit number that can be formed as the
 * concatenated product of an integer with (1,2, ... , n) where n > 1?
 *
 */
object Problem038 {
  val panDigits = "123456789".permutations.map(BigInt.apply).toSet
  def concat(a: Int, n: Int) = BigInt((1 to n).map(a * _).mkString)
  def f(n: Int) = Iterator.from(2).map(concat(n, _)).takeWhile(_ <= 987654321).filter(panDigits.contains)
  def main(args: Array[String]) = Answer {
    val uBound = 9877 // Iterator.from(1).find(concat(i,2) >= 987654321).get
    (1 to uBound).flatMap(f).max
  }
}