package ca.uqam.euler.nicolas
object Problem045 {
//  import Stream._
//  class Numbers(var ns: Stream[Long]) {
//    def countains(n: Long) = {
//      ns = ns.dropWhile(_ < n)
//      ns.head == n
//    }
//  }
//  val ps = new Numbers(from(1) map { n ⇒ n.toLong * (3 * n - 1) / 2 })
//  val hs = new Numbers(from(1) map { n ⇒ n.toLong * (2 * n - 1) })
//  val ts = from(1) map { n ⇒ n.toLong * (n + 1) / 2 }
//  def main(args: Array[String]) {
//    println(
//      ts.drop(285)
//        .find(t ⇒ hs.countains(t) && ps.countains(t))
//        .get)
//  }
//
//  import java.util.Random
//  import scalaz._; import Scalaz._
//  def dice() = state[Random, Int](r ⇒ (r, r.nextInt(6) + 1))
//  def TwoDice() = for {
//    r1 ← dice()
//    r2 ← dice()
//  } yield (r1, r2)
//  val list = List.fill(10)(TwoDice())
//  type StateRandom[x] = State[Random,x]
//  val list2 = list.sequence[StateRandom, (Int, Int)]

}