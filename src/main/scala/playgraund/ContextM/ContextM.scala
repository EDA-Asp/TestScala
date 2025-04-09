package playgraund.ContextM

import java.io.PrintStream

object ContextM extends App {

  val captured = captureFreeVar(freeVar)

  println(captureVariable(10)) // 20

  freeVar += 1
  println(captureVariable(10)) // 21

  freeVar = 10
  val triple = someConstruct(3) { x => x + 1 }
  val triple2 = someConstruct2({ x =>
    x + 1
  })(3)

  println(captured(1))

  freeVar = 11

  println(captured(1))
  var freeVar = 10

  def captureVariable = (bindedVar: Int) => bindedVar + freeVar
//
//  withPrintWriter() { printStream =>
//    printStream.println(new java.util.Date)
//  }

  def captureFreeVar(x: Int) = (y: Int) => x + y

  def withPrintWriter(ps: PrintStream = System.out)(op: PrintStream => Unit) =
    try op(ps)
    finally ps.close()

  println("AAA")

  println(triple(1))

  def someConstruct(n: Int)(f: Int => Int): Int => Int =
    (0 until n).foldLeft(identity[Int])((f1, _) => f1 andThen f)

  def someConstruct2(f: Int => Int)(n: Int): Int => Int =
    (0 until n).foldLeft(identity[Int])((f1, _) => f1 andThen f)

  println("AAA")

  println(triple2(1))


  def biz(a:Int, b: Int) = (a,b)

  def bizz(a:Int, b: String) = (_:Int,_:Int)


  object ListMatcher:
    private val innerList = List("one", "two", "three", "Four")

    def stringsEndings(query: String): List[String] =
      listMatching(_.endsWith(query))

    private def listMatching(matcher: String => Boolean): List[String] =
      for s <- innerList if matcher(s) yield s

}
