package ca.uqam.euler.nicolas.scala

/** What is the sum of the digits of the number 2^(1000)? */
// Answer: 1366
object Problem016 {

  def sumOfDigits(base: Int, power: Int) = {
    BigInt(base).pow(power).toString.foldLeft(0)(_ + _.asDigit)
  }

  def main(args: Array[String]): Unit = {
    val t1 = System.currentTimeMillis

    println(sumOfDigits(2, 1000))

    val t2 = System.currentTimeMillis
    println(t2 - t1 + "ms")
  }

}