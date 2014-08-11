package ca.uqam.euler.nicolas

import scala.collection.immutable.SortedMap

object Problem031 {

  object CoinMap {
    val coins = List(200, 100, 50, 20, 10, 5, 2, 1)
    val empty = CoinMap(coins.map(_ -> 0).toMap)
  }
  case class CoinMap(m: Map[Int, Int]) {
    val sum = m.map(e => e._1 * e._2).sum
    def add(coin: Int) = CoinMap(m.updated(coin, m(coin) + 1))
  }

  import CoinMap._

  def combinations(
    candidates: Set[CoinMap],
    found: Set[CoinMap],
    target: Int): Set[CoinMap] = {
    val newFound = found ++ candidates.filter(_.sum == target)
    val smallers = candidates.filter(_.sum < target)
    if (smallers.isEmpty)
      newFound
    else {
      val newCandidates = for (s <- smallers; c <- coins) yield s.add(c)
      combinations(newCandidates, newFound, target)
    }
  }
  def main(args: Array[String]) = Answer {
    // Answer : 73682
    // Note: not very efficient -- there should be a better, simpler way - see forum 
    val xs = combinations(Set(empty), Set[CoinMap](), 200)
    println(xs.mkString("\n"))
    xs.size
  }

}