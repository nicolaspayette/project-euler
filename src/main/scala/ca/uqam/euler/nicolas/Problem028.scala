package ca.uqam.euler.nicolas

/**
 * Starting with the number 1 and moving to the right
 * in a clockwise direction a 5 by 5 spiral is formed
 * as follows:
 *
 * 21 22 23 24 25
 * 20  7  8  9 10
 * 19  6  1  2 11
 * 18  5  4  3 12
 * 17 16 15 14 13
 *
 * It can be verified that the sum of the numbers on the
 * diagonals is 101. What is the sum of the numbers on
 * the diagonals in a 1001 by 1001 spiral formed in the
 * same way?
 *
 */
object Problem028 {

  case class V(x: Int, y: Int) { def +(v: V) = V(x + v.x, y + v.y) }
  type M = IndexedSeq[IndexedSeq[Int]]

  def dirs = {
    def f(unused: Seq[V], used: Seq[V]): Stream[V] =
      if (unused.nonEmpty)
        unused.head #:: f(unused.tail, unused.head +: used)
      else
        f(used.reverse, Nil)
    f(Seq(V(-1, 0), V(0, 1), V(1, 0), V(0, -1)), Nil)
  }

  def makeSpiral(size: Int) = {
    assert(size % 2 == 1)

    def look(m: M, p: V) =
      if (m.isDefinedAt(p.x))
        if (m(p.x).isDefinedAt(p.y))
          Some(m(p.x)(p.y))
        else None
      else None

    def updated(m: M, pos: V, i: Int) =
      m.updated(pos.x, m(pos.x).updated(pos.y, i))

    def f(i: Int, p: V, dirs: Stream[V], m: M): M = {
      val q = p + dirs.tail.head
      if (look(m, q) == Some(0))
        f(i + 1, q, dirs.tail, updated(m, p, i))
      else if (look(m, p) == None)
        m
      else
        f(i + 1, p + dirs.head, dirs, updated(m, p, i))
    }

    f(1, V(size / 2, size / 2), dirs,
      IndexedSeq.tabulate(size, size)((x, y) => 0))

  }

  def diagSum(m: M) =
    (0 until m.size).map { x =>
      m(x)(x) + m(x)(m.size - x - 1)
    }.sum - 1

  def main(args: Array[String]) = Answer {
    diagSum(makeSpiral(1001))
  }
}