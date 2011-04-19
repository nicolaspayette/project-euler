package ca.uqam.euler.nicolas.scalax

object Problem048 {

  def lastNDigitsForMTerms(n: Int, m: Int) = {
  	def s(n: Int): Stream[BigInt] = BigInt(n).pow(n) #:: s(n + 1)
  	s(1).take(m).sum.toString.takeRight(n)
  }

  def main(args: Array[String]): Unit = {
    util.timed {
    	println(lastNDigitsForMTerms(10, 1000))
    }
  }

}