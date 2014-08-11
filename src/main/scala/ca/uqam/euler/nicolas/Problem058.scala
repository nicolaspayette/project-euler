package ca.uqam.euler.nicolas

/* 

Starting with 1 and spiralling anticlockwise in the following way, 
a square spiral with side length 7 is formed.

37 36 35 34 33 32 31
38 17 16 15 14 13 30
39 18  5  4  3 12 29
40 19  6  1  2 11 28
41 20  7  8  9 10 27
42 21 22 23 24 25 26
43 44 45 46 47 48 49

It is interesting to note that the odd squares lie along the bottom 
right diagonal, but what is more interesting is that 8 out of the 13 
numbers lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.

If one complete new layer is wrapped around the spiral above, a square 
spiral with side length 9 will be formed. If this process is continued, 
what is the side length of the square spiral for which the ratio of primes 
along both diagonals first falls below 10%?

 */
object Problem058 {

  import Util._

  def diags: Stream[(Int, Int)] = {
    def from(xs: Stream[Int], n: Int): Stream[(Int, Int)] = {
      val (ring, rest) = xs.splitAt(n * 4 - 4)
      val diagonals = ring.filter(x => (x - 1) % (n - 1) == 0)
      val primes = diagonals.filter(isPrime)
      Stream.cons(n -> primes.size, from(rest, n + 2))
    }
    (1, 0) #:: from(Stream.from(2), 3)
  }
  

  def main(args: Array[String]) {
    val sides = Stream.from(1, 2)
    val totalPrimes = diags.scanLeft(0)(_ + _._2).tail
    val totalDiags = 1 #:: Stream.from(5, 4)
    println((totalPrimes zip totalDiags).map(x => x._1 / x._2.toDouble).tail
      .takeWhile(_ >= 0.12)
      //      .take(4000)
      .map { x => println(x); x }
      .last)
  }

}