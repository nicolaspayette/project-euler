package ca.uqam.euler.nicolas.scala
import Util._

object Problem037 {
  def left(s: String) = s.tails.takeWhile(_.nonEmpty)
  def right(s: String) = left(s.reverse).map(_.reverse)
  def truncs(n: Int, f: String ⇒ Iterator[String]) = f(n.toString).map(_.toInt)
  def test(n: Int) = truncs(n, left).forall(isPrime) && truncs(n, right).forall(isPrime)
  def main(args: Array[String]) = Answer {
    Iterator.from(10).filter(test).take(11).sum
  }
}
