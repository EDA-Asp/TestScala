package playgraund

object FunMethods extends App {

  def foo[A](xs: List[A]): List[A] = xs.reverse

  val bar: [A] => List[A] => List[A] = [A] => (xs: List[A]) => foo[A](xs)
  val mkl = [A] => (a: A) => List(a)
  def mmkl[A](a: A): List[A] = List(a)

  val mkl2 = [A] => (a: A) => mmkl(a)

  println(List(1, 2, 3).map(mkl[Int]))

  println(List(1, 2, 3).map(mkl2[Int]))

  println(List(1, 2, 3).map(mmkl))

  println(foo(List(1, 2, 3)))
  println(bar(List(1, 2, 3)))

  def add1(n: Int): Int = n + 1

  //
//  println(makeList(1))
  println(mkl(1))
//
//  val makeList = [a] => (a: a) => List(a)
  val ff = add1(1)
  val aa = (y: Int) => y
  val a = (x: Int, y: Int) => x + y

  println(add1.getClass)
  val b = new B(1)
  val bf1: (Int, Int) => Int = b.Foo
  println(a(1, 2))
  val bf2: (Int, Int) => Int = b.Foo
  val l = List.tabulate(10)(_ + 1)

  println(bf1(1, 2))

//  b.x += 100

  println(bf1(1, 2))
  val ll = List.tabulate(10)(_ + 1)
  println(bf2(1, 2))

  println(bf1 == bf2)
  val lll = l lazyZip ll
  val c = b.Foo.tupled andThen (_ * 10)

  // A polymorphic method:

  println(lll.map(bf1))
  println(lll.map(b.Foo))
  println(lll.map(a))

  def Foo(x: Int, y: Int): Int = x + y

  def Bar = (x: Int, y: Int) => x + y

  class B(var x: Int):
    def Foo(x: Int, y: Int): Int = x + y + this.x

}
