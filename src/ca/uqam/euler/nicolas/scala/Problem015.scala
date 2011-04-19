package ca.uqam.euler.nicolas.scala

object Problem015 {

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

object Problem015B {
  
  // TODO: vérifier quelle version est la bonne

  def problem015(gridSize: Int) = {
    case class V(x: Int, y: Int) { def +(v: V) = V(v.x + x, v.y + y) }
    
    //@scala.annotation.tailrec
    def search(p: V, nbRoutes: Int): Int =
      if (p == V(gridSize, gridSize))
        nbRoutes + 1
      else if (p.x > gridSize || p.y > gridSize)
        nbRoutes
      else search(p + V(1, 0), nbRoutes) + search(p + V(0, 1), nbRoutes)
      
    search(V(0, 0), 0)
  }
  def main(args: Array[String]): Unit = {
    tools.timed {
      println(problem015(20))
    }
  }

}