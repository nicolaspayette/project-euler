package ca.uqam.euler.nicolas
import math._
object Problem079 {
  def main(args: Array[String]) = Answer {
    val attempts = io.Source.fromFile("keylog.txt")
      .getLines
      .map(_.map(_.asDigit))
      .toSet
    val minLength = attempts.flatten.size
    val start = BigInt(10) pow (minLength - 1)
    def digits(n: BigInt) = n.toString.map(_.asDigit)
    def plausible(pwdDigits: Seq[Int], attempt: Seq[Int]): Boolean =
      attempt match {
        case Seq() ⇒ true
        case Seq(hd, tl @ _*) ⇒
          pwdDigits.indexWhere(_ == hd) match {
            case -1 ⇒ false
            case i  ⇒ plausible(pwdDigits.drop(i + 1), tl)
          }
      }
    Iterator.iterate(start)(_ + 1)
      .find(pwd ⇒ attempts.forall(plausible(digits(pwd), _)))
      .get
  }
}