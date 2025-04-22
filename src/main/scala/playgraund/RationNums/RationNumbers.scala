package playgraund.RationNums

import scala.annotation.tailrec

object RationNumbers extends App {

  val n1 = Ration(1, 2)
  val n2 = Ration(1, 2)
  val r = n1 + n2
  println(r)

  println(Ration(1,2) == Ration(1,2))

}

class Ration(n: Int, d: Int):
  require(d != 0)

  private val g = gcd(n.abs, d.abs)
  val numerator: Int = n / g
  val denumerator: Int = d / g

  override def toString: String = s"$numerator/$denumerator"

  def -(that: Ration): Ration =
    this + (that * -1)

  def +(that: Ration): Ration =
    Ration(
      numerator * that.denumerator + denumerator * that.numerator,
      denumerator * that.denumerator
    )

  def *(n: Int): Ration = Ration(numerator * n, denumerator)

  def -(n: Int): Ration =
    this + (n * -1)

  def +(n: Int): Ration = this + Ration(n)

  def this(n: Int) = this(n, 1)

  def /(that: Ration): Ration = that * Ration(that.denumerator, that.numerator)

  def /(that: Int): Ration = this * Ration(1, n)

  def *(that: Ration): Ration =
    Ration(numerator * that.numerator, denumerator * that.denumerator)

  @tailrec
  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a
    else gcd(b, a % b)
