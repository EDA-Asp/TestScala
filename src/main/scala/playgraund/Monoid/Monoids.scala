package playgraund.Monoid

import cats.Monoid

implicit val intAdditionMonoid: Monoid[Int] = new Monoid[Int] {
  def empty: Int = 0

  def combine(x: Int, y: Int): Int = x + y
}

given strAdditionMonoid: Monoid[String] with
  val empty: String = ""

  def combine(x: String, y: String): String = x + " " + y

  override def combineAll(as: IterableOnce[String]): String =
    if (as.iterator.nonEmpty) as.iterator.reduce(combine) else ""

given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
  def combine(f: A => B, g: A => B): A => B = a => mb.combine(f(a), g(a))

  val empty: A => B = _ => mb.empty

@main
def main(): Unit =

  val a = Monoid[String](using strAdditionMonoid).combineAll(List("a", "b", "c"))

  println(a)

  val b = Monoid[String].combineAll(List("a"))
  println(b)

  val f1 = (x: Int) => List(x)
  val f2 = (x: Int) => List(x + 1)
  val f3 = Monoid[Int => List[Int]].combine(f1, f2)

  println(f3(5))
