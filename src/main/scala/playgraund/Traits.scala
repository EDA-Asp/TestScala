package playgraund

import scala.collection.mutable.ArrayBuffer

object Traits extends App {

  val frog = Frog()
  val frog_phil: Philosophical = frog

  println(frog.philosophize)
  println(frog_phil.philosophize)
  println(Baz().get)

  val qb = BasicQueue()

  val qbd = new BasicQueue with Doubling
  val qbdi: BasicQueue = new BasicQueue with Doubling with Incrementing

  qbdi.put(10)
  println(qbdi.get())

  val qbid = new BasicQueue with Incrementing with Doubling

  qbid.put(10)
  println(qbid.get())

  val q: BasicQueue = new BasicQueue with Incrementing with Filtering

//  println(x)
//  pr(x.head)

}

class Animal

trait Philosophical:
  def philosophize = "I consume memory, there fore i am"

class Frog extends Animal, Philosophical:
  override def toString: String = "green"
  def say() = "Kwa"
  override def philosophize = s"It ain't easy being $this!"

trait Foo1:
  def get = "Foo"

trait Bar1:
  def get = "Bar"

class Baz extends Foo1, Bar1:
  override def get = "Baz"

abstract class IntQueue:
  def get(): Int
  def put(x: Int): Unit

class BasicQueue extends IntQueue:
  private val buf = ArrayBuffer.empty[Int]

  override def get(): Int = buf.remove(0)

  override def put(x: Int): Unit = buf += x

transparent trait Doubling extends IntQueue:
  abstract override def put(x: Int): Unit = super.put(x * 2)

transparent trait Incrementing extends IntQueue:
  abstract override def put(x: Int): Unit = super.put(x + 1)

transparent trait Filtering extends IntQueue:
  abstract override def put(x: Int): Unit = if x > 0 then super.put(x)

//val condition = false
//transparent trait S
//trait Kind
//object Var extends Kind, S
//object Val extends Kind, S
//val x = Set(if condition then Val else Var)
//
//def pr(x: Kind): Unit = println(x)
