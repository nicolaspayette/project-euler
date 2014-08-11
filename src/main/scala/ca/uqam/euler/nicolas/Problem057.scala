package ca.uqam.euler.nicolas

//object Problem057 {
//
//  def nbDigits(n: BigInt) = Iterator.iterate(n)(_ / 10).takeWhile(_ > 0).size
//
//  case class R(_n: BigInt, _d: BigInt = 1) {
//    val (n, d) = { val g = _n gcd _d; (_n / g, _d / g) }
//    def +(r: R) = R(n * r.d + r.n * d, d * r.d)
//    def /(r: R) = R(n * r.d, d * r.n)
//    def p = nbDigits(n) > nbDigits(d)
//  }
//
//  def f(n: Int) = R(1) + R(1) / g(n - 1)
//  def g(n: Int): R = R(2) + (if (n == 0) R(0) else R(1) / g(n - 1))
//
//  def main(args: Array[String]) = {
//    println((1 to 1000) map f filter (_.p) size)
//  }
//
//}

object Problem057 {
  import Iterator._
  val nbDigits = iterate(_: BigInt)(_ / 10).takeWhile(_ > 0).size
  def main(args: Array[String]) {
    println(iterate((BigInt(3), BigInt(2))) { case (n, d) ⇒ (2 * d + n, n + d) }
      .take(1000)
      .filter { case (n, d) ⇒ nbDigits(n) > nbDigits(d) }
      .size)
  }
}