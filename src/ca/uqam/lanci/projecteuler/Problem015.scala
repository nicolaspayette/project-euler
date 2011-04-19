package ca.uqam.lanci.projecteuler

object Problem015 {

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
    util.timed {
      println(problem015(20))
    }
  }

}