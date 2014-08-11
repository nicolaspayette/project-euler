package ca.uqam.euler.nicolas

import scala.annotation.tailrec
import java.math.RoundingMode
import java.math.{ BigDecimal => JBD }

/**
 * A unit fraction contains 1 in the numerator. The decimal representation
 * of the unit fractions with denominators 2 to 10 are given:
 *
 * ^(1)/_(2)	= 	0.5
 * ^(1)/_(3)	= 	0.(3)
 * ^(1)/_(4)	= 	0.25
 * ^(1)/_(5)	= 	0.2
 * ^(1)/_(6)	= 	0.1(6)
 * ^(1)/_(7)	= 	0.(142857)
 * ^(1)/_(8)	= 	0.125
 * ^(1)/_(9)	= 	0.(1)
 * ^(1)/_(10)	= 	0.1
 *
 * Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle.
 * It can be seen that ^(1)/_(7) has a 6-digit recurring cycle.
 * Find the value of d < 1000 for which ^(1)/_(d) contains the longest
 * recurring cycle in its decimal fraction part.
 */
object Problem026 {

  def nonTerminating(d: Int) =
    try {
      new JBD(1) divide new JBD(d)
      false
    } catch {
      case _: ArithmeticException => true
    }

  def div(d: Int, prec: Int) =
    new JBD(1) divide (new JBD(d), prec, RoundingMode.HALF_UP)

  def digitString(d: Int, nbDigits: Int) =
    div(d, nbDigits + 1).toPlainString.drop(2).init

  def findSeqIn(d: Int, nbDigits: Int) = {
    val digits = digitString(d, nbDigits)
    val minRepeats = 5
    val maxPrefixLenght = 10
    val start = math.max(1, nbDigits - maxPrefixLenght)
    (start to nbDigits).find { size =>
      val seq = digits.takeRight(size)
      val nextDigits = digitString(d, nbDigits + size * minRepeats).drop(nbDigits)
      val groups = nextDigits.grouped(size)
      groups.forall(seq == _)
    }
  }

  def main(args: Array[String]) {
    @tailrec
    def loop(candidates: Seq[Int], nbDigits: Int): (Int, Int) = {
      println(candidates)
      println(nbDigits)
      if (candidates.size == 1) {
        val d = candidates.head
        val hasSeq = findSeqIn(d, nbDigits)
        hasSeq match {
          case Some(n) => d -> n
          case None    => loop(candidates, nbDigits + 1)
        }
      } else {
        loop(candidates.filterNot(findSeqIn(_, nbDigits).isDefined), nbDigits + 1)
      }
    }
    val res = loop((1 until 1000).filter(nonTerminating), 1)
    println(res)
  }
}