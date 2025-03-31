import scala.annotation.tailrec
//import playgraund.Katas.booleanToStringToString
//
//import scala.collection.mutable.ListBuffer
//
//@main
//def main(): Unit =
//
////  val r = booleanToStringToString(false)
////
////  println(s"booleanToString = $r")
////
////  val r2 = booleanToStringToString(true)
////
////  println(s"booleanToString = $r2")
//
//  type Pos = (Int, Int)
//
//  type Path = List[Pos]
//
//  val mv_1: Pos => Pos = p => (p._1 + 1, p._2)
//
//  val mv_2: Pos => Pos = p => (p._1, p._2 + 1)
//
//  val mvs = List(mv_1, mv_2)
//
////  def backtracking(from: Pos, s: Int): List[Any] =
////    val l: List[Any] = List(from)
////    if s == 0
////      then List(from)
////    else
////      for f <- mvs do
////        l :: backtracking(f(from), s - 1)
////    l
//
//
//
////  println(backtracking((0,0),3))
//
////def flatFilter(src: Seq[Any], pred: (Int => Boolean)): Seq[Int] = {
////  if (src.isEmpty)
////    Seq.empty
////  else src.head match {
////    case car: Seq[Any] =>
////      flatFilter(car, pred) ++ flatFilter(src.tail, pred)
////    case car: Int =>
////      if (pred(car))
////        car +: flatFilter(src.tail, pred)
////      else
////        flatFilter(src.tail, pred)
////    case _ => flatFilter(src.tail, pred)
////  }
////}
//
//  val lintsf = List((x: Int) => x + 1, (x: Int)=> x + 2)
//
//  //var buffer = ListBuffer[Any](1)
////
////  def backtracking(from: Int, s: Int): List[Any] =
////    val buffer = ListBuffer[Any](from)
////    s match
////      case 0 =>
////        val a = List(from)
////      case _ =>
////        for i <- lintsf do
////          buffer.addOne(backtracking(i(from),s-1))
////    buffer.toList
////
////  println(backtracking(1, 2))
//
////
////  def allPath(): List[List[Int]] =
////    val from = 0
////    val step = 5
////    val rez: List[List[Int]] = List()
////    lintsf.map(mkStep => path(from, step, List(), mkStep))
////
//
//
//  def allPath(): List[List[Int]] =
//    val from = 0
//    val step = 5
//    val rez: List[List[Int]] = List()
//    lintsf.map(mkStep => path(from, step, List(), mkStep))
//
//
//
//  def path(from: Int, step: Int, acc: List[Int], mkStep: (Int=>Int)): List[Int] =
//
//    if step == 0
//    then
//      acc
//    else
//      path(from + 1, step -1, acc :+ mkStep(from), mkStep)
//
//
//
//  println(path(1,5,List(0),(x:Int)=> x+10))
//
//  println(allPath())
//
//
//  def path2(from: Int, step: Int, acc: List[Int], mkStep: (Int => Int)): List[Int] =
//
//    if step == 0
//    then
//      acc
//    else
//      path(from + 1, step - 1, acc :+ mkStep(from), mkStep)
//
//
//  def backtracking(from: Int, s: Int): List[Any] =
//    val buffer = ListBuffer[Any](from)
//    s match
//      case 0 =>
//      case _ => lintsf.foreach(i => buffer.addOne(backtracking(i(from), s - 1)))
//    buffer.toList
//
//  println(backtracking(1, 2))

@main
def main(): Unit =

  // Print all subsets of a given Set or Array
//
//
//  def subSetsOfArrayProblem(n: Int): Set[Int] =
//    val s = Set.range(1,n+1)
//
//
//  def solver()
//

  def subsetsS(start: Int, end: Int, count: Int): Seq[Seq[Int]] = (
    if (count == 0)
      List(Nil)
    else
      for (
        head <- start to end;
        tail <- subsetsS(head + 1, end, count - 1)
      )
        yield head +: tail
  )

  println(subsetsS(1, 3, 2))

  def subsets2(start: Int, end: Int, count: Int): Seq[Seq[Int]] =
    if (count == 0)
      List(Nil)
    else
      for
        head <- start to end
        tt <- subsets2(head + 1, end, count - 1)
      yield head +: tt

  println(subsets2(1, 3, 2))

  val lintsf = List((x: Int) => x + 1, (x: Int) => x + 2)

//  @tailrec
//  def backtracking2(from: Int, s: Int, acc: List[List[Int]]): List[List[Int]] =
//    s match
//      case 0 => acc
//      case _ =>
//        for
//          f <- List((x: Int) => x + 1, (x: Int) => x + 2)
//        yield
//          backtracking2(f(from), s - 1, List(from) +: acc).flatten
//
//
//
//  println("AAAAAAA")
//  println(backtracking2(1,1,List(List()))

  def backtracking222(from: Int, s: Int): List[List[Int]] =
    s match
      case 0 => List(List(from))
      case _ =>
        List((x: Int) => x + 1, (x: Int) => x + 2)
          .flatMap(f =>
            backtracking222(f(from), s - 1)
              .map(k => from +: k)
          )

  println(backtracking222(1, 1))

  def backtracking333(from: Int, s: Int): Seq[Seq[Int]] =
    s match
      case 0 => List(List(from))
      case _ =>
        for
          f <- List((x: Int) => x + 1, (x: Int) => x + 2)
          k <- backtracking333(f(from), s - 1)
        yield from +: k

  println(backtracking333(1, 1))

  def backtracking444(from: Int, s: Int, acc: List[Int] = List()): List[List[Int]] =

    if s == 0 then List(acc :+ from)
    else lintsf.flatMap(i => backtracking444(i(from), s - 1, acc :+ from))

  println("ASD")
  println(backtracking444(1, 2))

//
//  def backtracking555(from: Int, s: Int, acc: List[Int] = List()): List[Int] =
//
//    if s == 0 then List(acc :+ from)
//    else
//      lintsf.map(i => backtracking555(i(from), s - 1, acc :+ from))
//
//
//  println("ASddD")
//  println(backtracking555(1, 2))
//

  def backtracking3(from: Int, s: Int): List[Any] =
    s match
      case 0 => List(List(from))
      case _ =>
        for i <- lintsf
        yield from :: backtracking3(i(from), s - 1)

  println(backtracking3(1, 2))
