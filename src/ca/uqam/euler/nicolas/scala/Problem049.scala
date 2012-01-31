package ca.uqam.euler.nicolas.scala
/**
 * The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330,
 * is unusual in two ways:
 *
 * (i) each of the three terms are prime, and,
 * (ii) each of the 4-digit numbers are permutations of one another.
 *
 * There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
 * exhibiting this property, but there is one other 4-digit increasing sequence.
 *
 * What 12-digit number do you form by concatenating the three terms in this sequence?
 *
 */
object Problem049 {
  import Util._
  def main(args: Array[String]) = Answer {
    
    (for {
      as <- Seq.fill(3)(1 to 9).flatten combinations 4
      bs = as.permutations.map(fromDigits).filter(isPrime).toIndexedSeq
      cs <- (bs combinations 3)
      if cs(0) != 1487
      if cs(2) - cs(1) == cs(1) - cs(0)
    } yield cs.mkString).mkString

  }
}