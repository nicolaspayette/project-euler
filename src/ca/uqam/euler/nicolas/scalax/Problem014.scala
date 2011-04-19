package ca.uqam.euler.nicolas.scalax

import scala.Stream._

/**
 * The following iterative sequence is defined for the set of positive integers:
 * 	n -> n/2 (n is even)
 *  n -> 3n + 1 (n is odd)
 *  Which starting number, under one million, produces the longest chain?
 */
object Problem014 {

  def largestChain(upTo: Long) = {
    var max = 0L
    var maxSeed = 0L
    var current = upTo
    while (current > 1) {
      var n = current
      var count = 1L
      while (n != 1) {
        count += 1
        n = if (n % 2 == 0) n / 2 else (3 * n) + 1
      }
      if (count > max) {
        max = count
        maxSeed = current
      }
      current -= 1
    }
    maxSeed
  }

  def collatz(n: Long): Stream[Long] = Stream.cons(n,
    if (n == 1) empty
    else if (n % 2 == 0) collatz(n / 2)
    else collatz((3 * n) + 1))

  def main(args: Array[String]): Unit = {
    val t1 = System.currentTimeMillis
    println(largestChain(1000000))
    val t2 = System.currentTimeMillis
    println(t2 - t1 + "ms")

    val t3 = System.currentTimeMillis
    println((1 to 1000000).map(n => collatz(n).length -> n).toMap.max._2)
    val t4 = System.currentTimeMillis
    println(t4 - t3 + "ms")

  }

}