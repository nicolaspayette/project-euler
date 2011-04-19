package ca.uqam.lanci.projecteuler
import java.math.RoundingMode
import java.math.{ BigDecimal => JBD }
import scala.annotation.tailrec
import scala.math.max

object Problem026 {

	def nonTerminating(d: Int) =
    try {
      new JBD(1) divide new JBD(d)
      false
    } catch {
      case _ => true
    }

  def div(d: Int, prec: Int) =
    new JBD(1) divide (new JBD(d), prec, RoundingMode.HALF_UP)

  def digitString(d: Int, nbDigits: Int) =
    div(d, nbDigits + 1).toPlainString.drop(2).init

  def findSeqIn(d: Int, nbDigits: Int) = {
    val digits = digitString(d, nbDigits)
    val minRepeats = 5
    val maxPrefixLenght = 10
    val start = max(1, nbDigits - maxPrefixLenght)
    (start to nbDigits).find { size =>
      val seq = digits.takeRight(size)
      val nextDigits = digitString(d, nbDigits + size * minRepeats).drop(nbDigits)
      val groups = nextDigits.grouped(size)
      groups.forall(seq ==)
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
          case None => loop(candidates, nbDigits + 1)
        }
      } else {
        loop(candidates.filterNot(findSeqIn(_, nbDigits).isDefined), nbDigits + 1)
      }
    }
    val res = loop((1 until 1000).filter(nonTerminating), 1)
    println(res)
  }
}