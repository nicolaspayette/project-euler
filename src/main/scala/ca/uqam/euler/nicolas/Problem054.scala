package ca.uqam.euler.nicolas

object Problem054 {
  case class Card(value: Int, suite: Char)
  def rank(cards: Seq[Card]) = {
    val cs = cards.sortBy(_.value)
    val isFlush = cs.forall(_.suite == cs.head.suite)
    val isStraight = cs.map(_.value) == (cs.head.value to cs.head.value + 4).toSeq
    val groups = (1 to 4).map(n ⇒ n -> cs
      .groupBy(_.value)
      .filter(_._2.size == n)
      .map(_._1).toSeq
      .sortBy(-_)).toMap
    if (isFlush && isStraight && cs.head.value == 10) Seq(10) // Royal Flush
    else if (isFlush && isStraight) Seq(9, cs.head.value) // Straight Flush
    else if (groups(4).nonEmpty) Seq(8, groups(4).head, groups(1).head) // Four of a Kind
    else if (groups(3).nonEmpty && groups(2).nonEmpty) Seq(7, groups(3).head, groups(2).head) // Full House
    else if (isFlush) Seq(6, cs.head.value) // Flush
    else if (isStraight) Seq(5, cs.head.value) // Straight
    else if (groups(3).nonEmpty) 4 +: (groups(3) ++ groups(1)) // Three of a Kind
    else if (groups(2).size == 2) 3 +: (groups(2) ++ groups(1)) // Two pairs
    else if (groups(2).nonEmpty) 2 +: (groups(2) ++ groups(1)) // One Pair
    else 1 +: groups(1) // High Card
  }
  def playerOneWins(str: String) = {
    val (h1, h2) = str.split(" ").splitAt(5)
    val stringToCard = (str: String) ⇒
      Card(Map('T' -> 10, 'J' -> 11, 'Q' -> 12, 'K' -> 13, 'A' -> 14)
        .getOrElse(str(0), str(0).asDigit), str(1))
    (rank(h1.map(stringToCard)), rank(h2.map(stringToCard)))
      .zipped
      .map(_ compare _)
      .dropWhile(_ == 0)
      .head == 1
  }
  def main(args: Array[String]) = {
    println(io.Source.fromFile("poker.txt")
      .getLines()
      .count(playerOneWins))
  }
}