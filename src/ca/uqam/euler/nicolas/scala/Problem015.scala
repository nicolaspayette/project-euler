package ca.uqam.euler.nicolas.scala

object PathProblem {

  def nbPaths(size: Int) = {

    case class Pos(x: Int, y: Int) {
      def valid = (x <= size && y <= size)
    }

    def nextPosFrom(p: Pos) =
      Seq(Pos(p.x + 1, p.y), Pos(p.x, p.y + 1)).filter(_.valid)

    val m = collection.mutable.Map(Pos(size, size) -> 1L)

    def nbPathsFrom(p: Pos): Long = {
      if (!m.contains(p))
        m(p) = nextPosFrom(p).map(nbPathsFrom).sum
      m(p)
    }

    nbPathsFrom(Pos(0, 0))
  }

  def main(args: Array[String]): Unit = {
	println(nbPaths(20))
  }

}