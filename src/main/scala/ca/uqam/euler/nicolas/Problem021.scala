package ca.uqam.euler.nicolas

/** Evaluate the sum of all amicable pairs under 10000. */
// Answer: ??
object Problem021 {

  def properDivisors(n: Int) = {
    val d = n.toDouble
    var i = 2
    var fs = Seq(1)
    while (i <= scala.math.sqrt(d)) {
      val xd = d / i
      val xi = xd.toInt
      if (xd == xi)
        fs = xi +: i +: fs
      i += 1
    }
    fs
  }

  def properDivisorsF(n: Int) = {
    // much slower, hence not used
    val d = n.toDouble
    def f(toCheck: Seq[Int], found: Seq[Int]): Seq[Int] =
      if (toCheck.isEmpty) found
      else {
        val i = toCheck.head
        val xd = d / i
        val xi = xd.toInt
        if (xd == xi)
          f(toCheck.tail, xi +: i +: found)
        else
          f(toCheck.tail, found)
      }
    f((2 to scala.math.sqrt(n).toInt), Seq(1))
  }

  def amicables(under: Int) = {
    val r = (1 until under)
    val d = r.map(n => n -> properDivisors(n).sum).toMap
    def f(ns: Set[Int], found: Seq[Int]): Seq[Int] =
      if (ns.isEmpty)
        found
      else {
        val a = ns.head
        val b = d(a)
        if (a != b && d.get(b) == Some(a))
          f(ns - a - b, a +: b +: found)
        else
          f(ns - a, found)
      }
    f(r.toSet, Nil)
  }

  def problem021(under: Int) = amicables(under).sum

  def main(args: Array[String]) = Answer {
    problem021(10000)
  }

}