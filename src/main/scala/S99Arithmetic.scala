import java.lang.Math._

/**
 * Arithmetic and algorithms using FP in Scala.
 *
 * @author Daniel Fang <danfang@uw.edu>
 */
object S99Arithmetic {

  class S99Int(val start: Int) {

    // Basic primality test.
    def isPrime: Boolean = {
      start > 1 &&
        Stream.range(2, sqrt(start).toInt + 1).forall(n => start % n != 0)
    }

    override def toString = start.toString
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  }

}
