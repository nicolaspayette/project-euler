package ca.uqam.euler.nicolas

import scala.collection.immutable.SortedSet
/**
 * Using names.txt  (right click and 'Save Link/Target As...'),
 * a 46K text file containing over five-thousand first names,
 * begin by sorting it into alphabetical order. Then working
 * out the alphabetical value for each name, multiply this
 * value by its alphabetical position in the list to obtain a name score.
 *
 * For example, when the list is sorted into alphabetical order, COLIN,
 * which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the
 * list. So, COLIN would obtain a score of 938 × 53 = 49714.
 *
 * What is the total of all the name scores in the file?
 *
 */

object Problem022 {

  def readNameFile(fn: String) =
    io.Source.fromFile(fn)
      .getLines.next.split(",")
      .map(s => s.substring(1, s.length - 1))

  def nameScore(name: String, pos: Int) =
    name.toCharArray.map(_.toLong - 64).sum * (pos)

  def nameScoreSum(names: Array[String]) =
    names.indices.map { i =>
      nameScore(names(i), i + 1)
    }.sum

  def main(args: Array[String]) {
    val names = readNameFile("/home/nicolas/Desktop/names.txt")
    util.Sorting.quickSort(names)
    println(nameScoreSum(names))
  }

}