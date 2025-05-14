package playgraund.MyStructures.PropertyBaseSet

object A extends App {

  val s1 = PBSet[Int](_ == 1)
  val s12 = s1 + 2
  val s123 = s12 + 3
  val s1234 = s123 + 4

  println(s1.contains(1))
  println(s12.contains(1))
  println(s12.contains(3))
  println(s1234.contains(4))
}
// {x in N | x % 2 == 0}

class PBSet[A](property: A => Boolean) extends (A => Boolean):
  infix def +(elem: A): PBSet[A] = PBSet(x => x == elem || property(x))
  infix def ++(anotherSet: PBSet[A]): PBSet[A] = PBSet(x => property(x) || anotherSet(x))
  def apply(elem: A): Boolean = contains(elem)
  def contains(elem: A): Boolean = property(elem)
  infix def -(elem: A): PBSet[A] = PBSet(x => x != elem)
  infix def --(anotherSet: PBSet[A]): PBSet[A] = filter(!anotherSet)
  def unary_! : PBSet[A] = PBSet(!contains(_))
  def map[B](f: A => B): PBSet[B] = ???
  def flatMap[B](f: A => PBSet[B]): PBSet[B] = ???
  def foreach(f: A => Unit): Unit = ???
  infix def &(anotherSet: PBSet[A]): PBSet[A] = PBSet(filter(anotherSet))
  def filter(p: A => Boolean): PBSet[A] = PBSet(x => property(x) && p(x))
  override def toString(): String = s"Cons2(${this.property})"

case class EmptyPBSet[T]() extends PBSet[T](x => false):
  override def map[B](f: T => B): PBSet[B] = EmptyPBSet()
  override def flatMap[B](f: T => PBSet[B]): PBSet[B] = EmptyPBSet()
  override def foreach(f: T => Unit): Unit = ()
  override def toString(): String = s"EmptyPBSet()"
