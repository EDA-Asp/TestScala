package playgraund

object Currying extends App {

  var inc = 1

  println(even() equals even())

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
