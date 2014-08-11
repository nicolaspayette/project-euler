package ca.uqam.euler.nicolas
import Util._

object Problem035 {
  def main(args: Array[String]) = Answer {
    def rotations(n: Int) = {
      val s = n.toString
      val it = Iterator.iterate(s)(s ⇒ s.last +: s.init)
      it.take(s.length).map(_.toInt)
    }
    def isCircularPrime(n: Int) = rotations(n).forall(isPrime)
    (1 to 1000000).filter(isCircularPrime).size
  }
}