package ca.uqam.euler.nicolas.scala

/** What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20? */
object Problem5 {

  def smallestEvDiv(n: Int) = {
	  Iterator.from(1).find { i =>
	 	  (1 to n).forall(i % _ == 0)
	  }.get
  }
  
  def main(args: Array[String]): Unit = {
    println(smallestEvDiv(20))
  }

}