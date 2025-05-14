package playgraund.MyStructures.MyList

import scala.annotation.tailrec

enum List[+A]:
  case Nill
  case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nill
    else Cons(as.head, apply(as.tail*))

  def sum[A](ints: List[A])(using num: Numeric[A]): A = {
    import num.*
    ints match {
      case Nill => num.fromInt(0)
      case Cons(h, t) => h + sum(t)
    }
  }

  @tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match {
      case Nill => acc
      case Cons(h, t) => foldLeft(t, f(h, acc), f)
    }

  def foldRight[A, B](as: List[A], z: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nill => z
      case Cons(x, xs) => f(x, foldRight(xs, z, f))

@main
def main(): Unit = {

  val r = List[Float]()
  val rez = List.sum(r)
  println(rez)
}
