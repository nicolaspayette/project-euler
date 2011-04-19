package ca.uqam.euler.nicolas.scala

/**
 * A permutation is an ordered arrangement of objects.
 * For example, 3124 is one possible permutation of the
 * digits 1, 2, 3 and 4. If all of the permutations are
 * listed numerically or alphabetically, we call it
 * lexicographic order. The lexicographic permutations
 * of 0, 1 and 2 are:
 *
 * 012   021   102   120   201   210
 *
 * What is the millionth lexicographic permutation of
 * the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
 */
object Problem024 {

  def perms(digits: Seq[Int], nth: Int) = {
    var i = 0
    var found = ""
    def f(used: Seq[Int], left: Seq[Int]) {
      if (found == "")
        if (left.isEmpty) {
          i += 1
          if (i == nth) found = used.mkString
        } else
          left.foreach(d => f(used :+ d, left.filter(_ != d)))
    }
    f(Nil, digits)
    found
  }

  def main(args: Array[String]): Unit = {
    println(perms(0 to 9, 1000000))
  }

}