package ca.uqam.euler.nicolas

object Problem036 {

  def isPalindrome(s: String) =
    s == s.reverse
  def main(args: Array[String]): Unit = Answer {
    (1 to 1000000)
      .filter(n ⇒
        isPalindrome(n.toString) &&
          isPalindrome(Integer.toBinaryString(n)))
      .sum
  }

}