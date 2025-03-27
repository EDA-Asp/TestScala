package playgraund.Recursion

object Subsets extends App {
  println(subsetsList(List(1, 2, 3)))

  println(subsetsSet(Set(1,2,3)))



}

def subsetsList(l: List[Int]): List[List[Int]] =
  l match
    case Nil => List(List())
    case h :: t =>
      for
        c <- List(true, false)
        s <- subsetsList(t)
      yield
        if c then h +: s
        else s


def subsetsSet(s: Set[Int]): Set[Set[Int]] =
  s match
    case e if e.isEmpty => Set(Set())
    case _ =>
      val h = s.head
      val t = s.tail
      for
        c <- Set(true, false)
        s <- subsetsSet(t)
      yield
        if c then Set(h) ++ s
        else s

//
//def subsets(l: List[Int]): List[List[Int]] =
//  l match
//    case Nil => List(List())
//    case h :: t =>
//      for
//        c <- List(true, false)
//      yield
//        if c then
//          h +: subsets(t).flatten
//        else
//          subsets(t).flatten