package ca.uqam.euler.nicolas.scala

object Problem025 {

  def fib = new Iterator[BigInt]() {
    def hasNext = true
    var pos = 1
    var oneBack = BigInt(1)
    var twoBack = BigInt(1)
    def next() = {
      val next =
        if (pos == 1 || pos == 2)
          BigInt(1)
        else
          oneBack + twoBack
      twoBack = oneBack
      oneBack = next
      pos += 1
      next
    }
  }

  def findFirstWithNDigits(n: Int) =
    fib.indexWhere(_.toString.length == n) + 1

  def main(args: Array[String]): Unit = {
    tools.timed {
      println(findFirstWithNDigits(1000))
    }
  }

}