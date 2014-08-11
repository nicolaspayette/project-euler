package ca.uqam.euler.nicolas

import Util.gcd

object Problem033 {

  case class Fraction(n: Int, d: Int) {
    override def toString = n + "/" + d
    def *(x: Fraction) = new Fraction(n * x.n, d * x.d)
    def simplified = {
      val g = gcd(n, d)
      new Fraction(n / g, d / g)
    }
    def equivalent(that: Fraction) = this.simplified == that.simplified
  }
  def main(args: Array[String]) = Answer {
    def other(c: Char, s: String) = (if (s(0) == c) s(1) else s(0)).asDigit
    val xs = for {
      d ← 10 to 99
      den = d.toString
      n ← 10 until d
      num = n.toString
      c ← num.distinct
      if c != '0' && den.contains(c)
      f1 = Fraction(n, d)
      f2 = Fraction(other(c, num), other(c, den))
      if f1 equivalent f2
    } yield f1
    xs.reduce(_ * _).simplified.d
  }

}