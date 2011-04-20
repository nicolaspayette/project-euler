package ca.uqam.euler.nicolas.scala

object Problem067 {

  def main(args: Array[String]): Unit = tools.timed {
    val lines = io.Source.fromFile("src/ca/uqam/euler/nicolas/scala/triangle.txt").getLines()
    println(Problem018.triangleSum(lines))
  }

}