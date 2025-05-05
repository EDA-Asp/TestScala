package playgraund.MyStructures

import scala.annotation.tailrec

object MyStructures extends App {

  val s1 = Cons(1, Cons(2, Cons(3, Empty())))
  val s2 = Cons(4, Empty())

  val a = s2.toString

//  println(s1.toString())
//
  println(s1 ++ s2)
  println(s1 + 2)

  println(s1.filter(x => x >= 2))

  s1.foreach(x => println(x))

  println(FSet(1, 2, 3, 4, 5, 6))

  println(FSet(1, 2, 3, 4, 5, 6) - 1)

  println("!!!!!!!!!!!!!!!!!!!!!!!")
  println(Cons(1, Empty()) -- Cons(1, Empty()))
  println("#######################")

  val s3 = FSet(1, 2)
  println(s1)
  println(s3)

  println(s1 -- s3)

  println(s1 & s3)

  println("#######################")

  val ss1 = Cons2(1, Cons2(2, Cons2(3, Empty2())))
  val ss2 = Cons2(2, Cons2(3, Empty2()))
  println(ss1)
  val rez = ss1 -- ss2

  println(rez(3))

  // rez.toString()

}

sealed abstract class FSet[A] extends (A => Boolean):
  def contains(elem: A): Boolean
  override def apply(elem: A): Boolean = contains(elem)
  infix def +(elem: A): FSet[A]
  infix def ++(anotherSet: FSet[A]): FSet[A]

  def map[B](f: A => B): FSet[B]
  def flatMap[B](f: A => FSet[B]): FSet[B]
  def filter(p: A => Boolean): FSet[A]

  def foreach(f: A => Unit): Unit

  infix def -(elem: A): FSet[A]
  infix def --(anotherSet: FSet[A]): FSet[A]
  infix def &(anotherSet: FSet[A]): FSet[A]

  def unary_! : FSet[A] = PBSet(!contains(_))

object FSet:
  def apply[T](values: T*): FSet[T] =
    @tailrec
    def buildFset(seq: Seq[T], acc: FSet[T] = Empty()): FSet[T] =
      if seq.isEmpty then acc
      else buildFset(seq.tail, acc + seq.head)
    buildFset(values)

case class Empty[T]() extends FSet[T] {

  override def contains(elem: T): Boolean = false

  override infix def +(elem: T): FSet[T] = Cons(elem, this)

  override infix def ++(anotherSet: FSet[T]): FSet[T] = anotherSet

  override def map[B](f: T => B): FSet[B] = Empty()
  override def flatMap[B](f: T => FSet[B]): FSet[B] = Empty()
  override def filter(p: T => Boolean): FSet[T] = this

  override def foreach(f: T => Unit): Unit = ()

  override def toString(): String = s"Empty()"

  override infix def -(elem: T): FSet[T] = this

  override infix def --(anotherSet: FSet[T]): FSet[T] = this

  override infix def &(anotherSet: FSet[T]): FSet[T] = this

}

// {x in N | x % 2 == 0}
class PBSet[A](property: A => Boolean) extends FSet[A]:
  override def contains(elem: A): Boolean = property(elem)

  override infix def +(elem: A): FSet[A] = PBSet(x => x == elem || property(elem))

  override infix def ++(anotherSet: FSet[A]): FSet[A] = PBSet(x => property(x) || anotherSet(x))

  override infix def -(elem: A): FSet[A] = PBSet(x => x != elem)

  override infix def --(anotherSet: FSet[A]): FSet[A] = filter(!anotherSet)

  override def map[B](f: A => B): FSet[B] = ???

  override def flatMap[B](f: A => FSet[B]): FSet[B] = ???

  override def foreach(f: A => Unit): Unit = ???

  override infix def &(anotherSet: FSet[A]): FSet[A] = PBSet(filter(anotherSet))

  override def filter(p: A => Boolean): FSet[A] = PBSet(x => property(x) && p(x))

  override def toString(): String = s"Cons2(${this.property})"

case class Empty2[T]() extends PBSet[T](x => false):
  override def map[B](f: T => B): FSet[B] = Empty()
  override def flatMap[B](f: T => FSet[B]): FSet[B] = Empty()
  override def foreach(f: T => Unit): Unit = ()
  override def toString(): String = s"Empty()"

//case class UniversalSet[T]() extends FSet[T] {
//  override def contains(elem: T): Boolean = true
//
//  override infix def +(elem: T): FSet[T] = this
//
//  override infix def ++(anotherSet: FSet[T]): FSet[T] = this
//
//  override def map[B](f: T => B): FSet[B] = ???
//
//  override def flatMap[B](f: T => FSet[B]): FSet[B] = ???
//
//  override def filter(p: T => Boolean): FSet[T] = ???
//
//  override def foreach(f: T => Unit): Unit = ???
//
//  override infix def -(elem: T): FSet[T] = ???
//
//  override infix def --(anotherSet: FSet[T]): FSet[T] = ???
//
//  override infix def &(anotherSet: FSet[T]): FSet[T] = ???
//
//}

case class Cons[A](head: A, tail: FSet[A]) extends FSet[A] {
  override infix def +(elem: A): FSet[A] =
    if contains(elem) then this
    else Cons(elem, this)

  override def contains(elem: A): Boolean = elem == head || tail.contains(elem)

  override infix def ++(anotherSet: FSet[A]): FSet[A] = tail ++ anotherSet + head

  override def map[B](f: A => B): FSet[B] = tail.map(f) + f(head)

  override def flatMap[B](f: A => FSet[B]): FSet[B] = tail.flatMap(f) ++ f(head)

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def toString(): String = s"Cons(${this.head}, ${this.tail.toString()})"

  override infix def -(elem: A): FSet[A] =
    if head == elem then tail
    else tail - elem + head

  override infix def --(anotherSet: FSet[A]): FSet[A] = filter(!anotherSet)

  override def filter(p: A => Boolean): FSet[A] = {
    if p(head) then tail.filter(p) + head
    else tail.filter(p)
  }

//    if anotherSet.contains(head) then tail -- anotherSet
//    else tail -- anotherSet + head

  override infix def &(anotherSet: FSet[A]): FSet[A] = filter(anotherSet)
//    if anotherSet.contains(head) then
//      (tail & anotherSet) + head
//    else
//      tail & anotherSet

}

case class Cons2[A](head: A, tail: PBSet[A]) extends PBSet[A](x => x == head || tail(x)) {

  override def map[B](f: A => B): FSet[B] = tail.map(f) + f(head)

  override def flatMap[B](f: A => FSet[B]): FSet[B] = tail.flatMap(f) ++ f(head)

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }
  override def toString(): String = s"Cons2(${this.head}, ${this.tail.toString()})"
}
