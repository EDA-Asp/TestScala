package playgraund.Recursion

import scala.annotation.tailrec

object SumInts extends App {

  println(sumInts1(1, 10))
  println(sumIntsList(List(1), 10))
  println(sumIntsList(List(), 10))

}

@tailrec
def sumInts1(start: Int, step: Int): Int =
  step match
    case 0 => start
    case _ => sumInts1(start + 1, step - 1)

@tailrec
def sumIntsList(start: List[Int], step: Int): List[Int] =
  step match
    case 0 => start
    case _ =>
      start match
        case h :: t => sumIntsList(h + 1 :: start, step - 1)
        case Nil => Nil
