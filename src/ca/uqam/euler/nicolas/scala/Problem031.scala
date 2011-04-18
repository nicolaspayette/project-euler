package ca.uqam.euler.nicolas.scala

object Problem031 {

  val coins = List(200, 100, 50, 20, 10, 5, 2, 1)

  def total(xs: Seq[Int]) = (xs zip coins).map(p => p._1 * p._2).sum

  def f(target: Int) = {
    
  }

  def combinations(candidates: Set[List[Int]], found: Set[List[Int]], target: Int): Set[List[Int]] = {
    if (candidates.isEmpty)
      found
    else {
      val toKeep = candidates.view.filter(_.sum <= target)
      val (equals, smallers) = toKeep.partition(_.sum == target)
      val newFound = found ++ equals
      if (smallers.isEmpty)
        newFound
      else {
        val newCandidates =
          for (c <- coins; s <- smallers)
            yield (c +: s).sorted
        combinations(newCandidates, newFound, target)
      }
    }
  }

  def main(args: Array[String]): Unit = tools.timed {
    val xs = combinations(coins.map(List(_)), Set[List[Int]](), 15)
    println(xs.mkString("\n"))
    println(xs.size)
  }

}