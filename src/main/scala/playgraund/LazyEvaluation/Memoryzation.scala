package playgraund.LazyEvaluation

object Memoryzation extends App {

//  lazy val x: Int = {
//    println("Hellow")
//    42
//  }
//
//  println(x)
//  println(x)

  // Hellow
  // 42
  // 42

  // ______________________________________

  // call by need

  // byNameMethods(retrieveInt())

  // Waiting ...
  // Waiting ...
  // Waiting ...

  // byNeedMethod(retrieveInt())

  // Waiting ...

}

def byNameMethods(x: => Int): Int = x + x + x // retrieveInt() + retrieveInt() + retrieveInt()

def byNeedMethod(x: => Int): Int = {
  val xx = x // retrieveInt()
  xx + xx + xx
}

def retrieveInt(): Int =
  println("Waiting ...")
  Thread.sleep(1000)
  42

import scala.collection.mutable

def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
  override def apply(key: I): O = getOrElseUpdate(key, f(key))
}
