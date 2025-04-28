package playgraund.PatternMatching

import scala.collection.mutable.ListBuffer

object PatternMatchingBasic extends App {


}

sealed trait Expr
case class Var(name: String) extends Expr
case class Num(number: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr,right: Expr) extends Expr


def describe(e: Expr): String =
  e match
    case _: Num => "a number"
    case _: Var => "a var"
    case _ => "something else"

def simplify(expr: Expr): Expr =
  expr match
    case UnOp("-", UnOp("-", x)) => simplify(x) // - -(x)
    case BinOp("+", Num(0), x) => simplify(x)
    case BinOp("*", Num(1), x) => simplify(x)
    case UnOp(op,e) => UnOp(op,simplify(e))
    case BinOp(operator, left, right) => BinOp(operator,simplify(left),simplify(right))
    case _ => expr