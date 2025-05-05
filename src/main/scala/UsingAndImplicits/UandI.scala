package UsingAndImplicits

import scala.math.Ordering.Implicits.infixOrderingOps

//This sort
//method can sort lists of any type T so long as T is a subtype of Ordered[T].
//This is called subtyping polymorphism, and as illustrated in Section 18.7, the
//upper bound of Ordered[T] means that you can’t use orderedMergeSort
//on lists of Ints or Strings. By contrast, you can sort lists of Ints and
//Strings with the msort shown in Listing 21.5, because the Ordering[T]
//it requires forms a separate hierarchy distinct from T.

object UandI extends App { // not ok
  // import JillsPrefs.given ok
  import JillsPrefs.prompt // ok

  Greeter.greet("aaa")
  Greeter.greet("aaa")(using prompt)

  println(User("a", 1) > User("a", 2))

  println(User2("a", 1) > User2("a", 2))

  val l1 = List(User("a", 2), User("b", 1)).sorted
  l1.foreach(println(_))

  val l2 = List(User2("a", 2), User2("b", 1)).sorted
  l2.foreach(println(_))

  val f: Int ?=> Int = summon[Int] + 1

  println(f(using 10))

  given Int = 2
  println(f)

  def cmb[T](x: T, y: T)(using Monoid[T]): T =
    x combine y

  println(cmb(1, 2))

  def combineAll[T: Monoid as m](xs: List[T]): T =
    xs.foldLeft(m.unit)(_.combine(_))

  println(1.combine(10))
}

//
class PreferredPrompt(val preference: String)

//#############
object Greeter:
  def greet(name: String)(using prompt: PreferredPrompt): Unit =
    println(s"Welcome, $name. The system is ready.")
    println(prompt.preference)

object JillsPrefs:
  given prompt: PreferredPrompt = PreferredPrompt("Your wish> ")

trait Ord[T]:
  def compare(x: T, y: T): Int
  def lteq(x: T, y: T): Boolean = compare(x, y) < 1

def isort[T <: Ordered[T]](xs: List[T]): List[T] =
  def insert[T <: Ordered[T]](x: T, xs: List[T]): List[T] =
    if xs.isEmpty || x <= xs.head then x :: xs
    else xs.head :: insert(x, xs.tail)
  if xs.isEmpty then Nil
  else insert(xs.head, isort(xs.tail))

case class User(name: String, age: Int)

object User:
  given useOrd: Ordering[User] = Ordering.by((x: User) => x.age)

case class User2(name: String, age: Int) extends Ordered[User2] {
  override def compare(that: User2): Int = this.age - that.age
}

trait SemiGroup[T]:
  extension (x: T) def combine(y: T): T

trait Monoid[T] extends SemiGroup[T]:
  def unit: T

given Monoid[String]:
  extension (x: String) def combine(y: String): String = x.concat(y)
  def unit: String = ""

given Monoid[Int]:
  extension (x: Int) def combine(y: Int): Int = x + y + 10
  def unit: Int = 0
