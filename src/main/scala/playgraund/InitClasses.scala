package playgraund

object InitClasses extends App {
//
  val b = Bar(List(1, 2, 3))
  println(b.len)

//  val b = B()
//  val c = C()
//  val z = Z()

  RemoteFile("asd")

}

// https://stackoverflow.com/questions/12184997/scala-and-forward-references
abstract class Foo:
  println("Foo")

  def l: List[Int] // lazy &

  val len: Int = l.length

object Foo:

  def fromList(l: List[Int]): Foo = Bar(l)
//
//class Bar(lp: List[Int]) extends Foo: // Important because class Bar(var l: List[Int]) extends Foo works
//  val l: List[Int] = lp

class Bar(var l: List[Int]) extends Foo:
  println("Bar") // Important because class Bar(var l: List[Int]) extends Foo works

abstract class AbstractFile:
  def extension: String = name.substring(4)

  def name: String

class RemoteFile(url: String) extends AbstractFile:
  val localFile: String = s"${url.##}.tmp" // error: usage of `localFile` before it's initialized
  def name: String = localFile

abstract class A {
  val x1: String
  val x2: String = "mom"

  println(s"A: $x1, $x2")
}
class B extends A {
  val x1: String = "hello"

  println(s"B: $x1, $x2")
}
class C extends B {
  override val x2: String = "dad"

  println(s"C: $x1, $x2")
}

class Z extends A:
  val x1: String = "hi"
  println(s"Z: $x1, $x2")


val z = Z()

//
//When a val is overridden, it’s more precise to say that its accessor method (the “getter”) is overridden.
//So the access to x2 in class A invokes the overridden getter in class C. That getter reads the underlying field C.x2.
//  This field is not yet initialized during the construction of A.
