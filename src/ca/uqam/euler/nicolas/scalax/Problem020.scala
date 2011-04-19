package ca.uqam.euler.nicolas.scalax

/** Find the sum of the digits in the number 100! */
object Problem020 {

  def sumOfDigitsForFactorial(n: Int) = {

    @scala.annotation.tailrec
    def f(product: BigInt, next: Int): BigInt = {
      if (next == 0) product
      else f(product * next, next - 1)
    }
    f(BigInt(n), n - 1).toString.foldLeft(0)(_ + _.asDigit)
  }

  def main(args: Array[String]): Unit = {
    val t1 = System.currentTimeMillis

    println(sumOfDigitsForFactorial(100))

    val t2 = System.currentTimeMillis
    println(t2 - t1 + "ms")
  }
}