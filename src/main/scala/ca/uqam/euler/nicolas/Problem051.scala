package ca.uqam.euler.nicolas

/**
 * By replacing the 1st digit of *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
 *
 * By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.
 *
 * Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.
 */
object Problem051 {
  import Seq.fill
  import Util._
  def main(args: Array[String]): Unit = Answer {
    Iterator.from(5)
      .flatMap { n ⇒
        (fill(n)('0' to '9').flatten ++ fill(n - 1)('*'))
          .combinations(n)
          .filter(_.exists(_ == '*'))
          .flatMap(_.permutations)
          .map { p ⇒
            ('0' to '9').map { d ⇒
              p
                .map(x ⇒ if (x == '*') d else x)
                .map(_.asDigit)
            }
              .filterNot(_.head == 0)
              .map(fromDigits)
              .filter(isPrime)
          }
      }
      .find(_.size == 8).get.head
  }

}