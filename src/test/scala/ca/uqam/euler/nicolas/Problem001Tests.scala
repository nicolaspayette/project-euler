package ca.uqam.euler.nicolas

import org.scalatest.FunSuite

class Problem001Tests extends FunSuite {
  test("sum of multiples of 3 or 5 below 10 is 23") {
    assert(Problem001.sumOfMultiples(10, 3, 5) == 23)
  }
}
