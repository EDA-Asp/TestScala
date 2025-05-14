package playgraund.MyStructures.FunctionalSet

import scala.annotation.tailrec

object A extends App {

  val s1 = Cons(1, Cons(2, Cons(3, Empty())))
  val s2 = Cons(4, Empty())
  println(s1 ++ s2)
  println(s1 + 2)

  println(s1.filter(x => x >= 2))

  s1.foreach(x => println(x))

  println(FSet(1, 2, 3, 4, 5, 6))

  println(FSet(1, 2, 3, 4, 5, 6) - 1)

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

  override infix def --(anotherSet: FSet[A]): FSet[A] = filter(!anotherSet(_))

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
