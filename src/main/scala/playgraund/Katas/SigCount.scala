package playgraund.Katas

import scala.annotation.tailrec

object SigCount extends App {

  val a = recCountSig(List(1, -2, 3, -4, 5, 5, 6, 7), 0, 1)
  println(a)

  val b = List(-1, 2, -3, -4).foldLeft(SigCounter(0))((x: SigCounter, y: Int) => x.add(y))

  // val b = List(-1, 2, -3, -4).fold(SigCounter(0))((x: SigCounter, y: Int) => x.add(y))

  println(b)

}

@tailrec
def recCountSig(seq: List[Int], cnt: Int, prev: Int): Int =
  seq match
    case Nil => cnt
    // case h :: Nil => cnt
    case h :: t => if h * prev < 0 then recCountSig(t, cnt + 1, h) else recCountSig(t, cnt, h)

class SigCounter(sign: Int, cnt: Int = 0):
  def add(x: Int): SigCounter =
    if sign != 0 then
      x.sign match
        case s if s == this.sign => SigCounter(s, cnt)
        case s if s != this.sign => SigCounter(s, cnt + 1)
    else SigCounter(x.sign)

  override def toString: String = s"$cnt"

object Rextester extends App {
  import scala.io.StdIn.readLine

  val input = readLine().split(' ').flatMap(_.toIntOption).toSeq

  println(input)

  def sum(in: Seq[Int]): Int = {
    val nonZero = in.filter(_ != 0)

    if (nonZero.nonEmpty)
      (nonZero zip nonZero.tail).collect {
        case (i1, i2) if i2.sign * i2.sign < 0 => 1
      }.sum
    else 0
  }

  println(sum(input))
}
