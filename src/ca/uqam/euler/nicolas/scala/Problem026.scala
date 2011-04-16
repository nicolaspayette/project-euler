/** 
 * A unit fraction contains 1 in the numerator. The decimal representation 
 * of the unit fractions with denominators 2 to 10 are given:    
 * 
 * ^(1)/_(2)	= 	0.5    
 * ^(1)/_(3)	= 	0.(3)    
 * ^(1)/_(4)	= 	0.25    
 * ^(1)/_(5)	= 	0.2    
 * ^(1)/_(6)	= 	0.1(6)    
 * ^(1)/_(7)	= 	0.(142857)    
 * ^(1)/_(8)	= 	0.125    
 * ^(1)/_(9)	= 	0.(1)    
 * ^(1)/_(10)	= 	0.1
 * 
 * Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. 
 * It can be seen that ^(1)/_(7) has a 6-digit recurring cycle.
 * Find the value of d < 1000 for which ^(1)/_(d) contains the longest 
 * recurring cycle in its decimal fraction part.
 */
object Problem026 {

  def isInfinite(bd: BigDecimal) =
    try {
      BigDecimal(1) / bd
      false
    } catch {
      case _ => true
    }

  def digits(d: BigDecimal, nbDigits: Int) = {
    val mc = new java.math.MathContext(nbDigits + 1)
    (BigDecimal(1)(mc) / d).toString.split('.')(1).init
  }

  def main(args: Array[String]): Unit = {

    def isRepeating(gs: Seq[String]): Boolean =
      if (gs.size < 2)
        false
      else if (gs.head == gs.tail.head)
        true
      else
        isRepeating(gs.tail)

    def hasSeqOf(d: BigDecimal, len: Int) = {
      def f(ds: String): Boolean = 
        if (ds.isEmpty)
          false
        else if (isRepeating(ds.grouped(len).toSeq))
          true
        else
          f(ds.tail)
      f(digits(d, len * 3))
    }

    val xs = (1 to 10).map(BigDecimal(_)).filter(isInfinite)
    
    println(xs.map(digits(_, 18)))
    println(xs.map(hasSeqOf(_, 6)))
    
  }

}