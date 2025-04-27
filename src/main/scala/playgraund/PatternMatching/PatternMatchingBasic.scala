package playgraund.PatternMatching

import scala.collection.mutable.ListBuffer

object PatternMatchingBasic extends App {

  println(Bar(Foo()) == Bar(Foo())) // false

  val foo_val = Foo()
  println(Bar(foo_val) == Bar(foo_val)) // true

  var foo_var = Foo()
  val b1 = Bar(foo_var)
  foo_var = Foo()
  val b2 = Bar(foo_var)
  println(b1 == b2) // false

  val lb = ListBuffer(1,2)
  var baz1 = Baz(lb)
  lb +=3
  var baz2 = Baz(lb)
  println(baz1 == baz2) //true

  println(baz1) //(1,2,3)
  println(baz2) //(1,2,3)

}

sealed trait Expr
case class Var(name: String) extends Expr
case class Num(number: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr,right: Expr) extends Expr


class Foo

case class Bar(a: Foo)

case class Baz(a: ListBuffer[Int])

