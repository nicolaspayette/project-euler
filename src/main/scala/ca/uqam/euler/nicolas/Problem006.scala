package ca.uqam.euler.nicolas

/** Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum. */
object Problem6 {

  def sq(x: Int) = x * x

  def difference(n: Int) = {
    val xs = 1 to n
    sq(xs.sum) - (xs map sq).sum
  }

  def main(args: Array[String]): Unit = {
    println(difference(100))
  }

}