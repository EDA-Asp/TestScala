package playgraund.RationNums

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.*

import scala.language.postfixOps

class RationTest extends AnyFunSuiteLike with Matchers {

  test("add two Rational") {
    val r = Ration(1, 2) + Ration(1, 2)
    r.numerator should be(1) // TODO: why Unused expression without side effects ?
    r.denumerator should be(1)
  }

  test("multiply two Rational") {
    val r = Ration(1, 2) * Ration(1, 2)
    r.numerator should be(1)
    r.denumerator should be(4)
  }

}
