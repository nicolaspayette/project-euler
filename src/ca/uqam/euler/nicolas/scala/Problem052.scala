package ca.uqam.euler.nicolas.scala

/**
 * It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.
 *
 * Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
 *
 */
object Problem052 {
  def main(args: Array[String]): Unit = Answer {
    def digits(n: Int): Set[Int] = n.toString.map(_.asDigit).toSet
    Stream.from(2).filter { x ⇒
      (2 to 6).forall { n ⇒ digits(x) == digits(n * x) }
    }.head
  }
}