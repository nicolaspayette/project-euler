package ca.uqam.euler.nicolas

object Problem032 {

  def main1(args: Array[String]) = Answer {
    def split(digits: String, pos: (Int, Int)) = {
      val (a, bc) = digits.splitAt(pos._1)
      val (b, c) = bc.splitAt(pos._2)
      (a.toInt, b.toInt, c.toInt)
    }
    val xs = for {
      digits <- (1 to 9).permutations.map(_.mkString)
      pos <- List((1, 4), (2, 2), (2, 3), (3, 2), (3, 3), (4, 1))
      t3 = split(digits, pos)
      if t3._1 * t3._2 == t3._3
    } yield t3._3
    xs.toSet.sum
  }
}