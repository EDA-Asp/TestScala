package playgraund

object Currying extends App {

  var inc = 1

  println("@@@@@@@@@@@@@!!!!")
  println(even() == even())

  val ev1 = even()
  val ev2 = even()

  println(ev1 == ev2)

  def even(): Int => Boolean = _ % 2 == 0

  def foo(x: Int): Int =
    x + inc

  println(foo(1))

  inc = 2

  println(foo(1))

  bar13(3)(4)

  println(bar12(1, 10))

  println("@@@@@@")
  println(bar14(1)(10))

  println(Abra().foo(1)(2))
  val pfff = Abra().foo(1)

  def concatenator1(a: String, b: String, c: String) = a + b + c

  val insertName = concatenator1("Hello", _, "Yep")

  val insertPrefPos = concatenator1(_, "Ema", _) // (String,String)=>String

  def concatenator2(a: String)(b: String)(c: String) = a + b + c

  val insertName2 = concatenator2("Hello")(_: String)("Yep")
  val insertPrefPos2 = concatenator2(_: String)("Emma")(_: String) // (String,String)=>String
  val crd = insertPrefPos2.curried // String => String => String

}

def produceLambda = (x: Int) => x + 2

val declareLambda: Int => Boolean = _ % 2 == 0

def addWithoutSyntaxSugar(x: Int): Function1[Int, Int] = {
  new Function1[Int, Int]() {
    def apply(y: Int): Int = x + y
  }
}
def fiveAdder: Function1[Int, Int] = addWithoutSyntaxSugar(5)

def foo(x: Int, y: Int, z: Int): Int = (x + y) * z

val bar12 = foo(_, 5, _)

def bar(x: Int)(y: Int)(z: Int): Int = (x + y) * z

val bar13 = bar(1)

val bar14 = bar(_: Int)(2)

val baz: Int => Int => Int => Int = x => y => z => (x + y) * z

val baz2 = (x: Int) => (y: Int) => (z: Int) => x + y + z

var k = baz(2)

class Abra:
  def foo(x: Int)(y: Int): Int =
    x + y
