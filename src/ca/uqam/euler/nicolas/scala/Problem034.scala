package ca.uqam.euler.nicolas.scala

object Problem034 {
  def main(args: Array[String]) = Answer {
    val factorials = (0 to 9).map { i ⇒ (1 to i).fold(1)(_ * _) }
    val uBound = factorials(9) * 7
    (3 to uBound).filter(n ⇒ n == n.toString.map(d ⇒ factorials(d.asDigit)).sum).sum
  }
}