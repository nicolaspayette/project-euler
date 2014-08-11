package ca.uqam.euler.nicolas

object Problem067 {
  def main(args: Array[String]) = Answer {
    val lines = io.Source.fromFile("src/ca/uqam/euler/nicolas/scala/triangle.txt").getLines()
    Problem018.triangleSum(lines)
  }
}