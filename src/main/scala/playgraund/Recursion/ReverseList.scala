package playgraund.Recursion

object ReverseList extends App {

  println(reverseList(List(1, 2, 3, 4, 5)))
  println(reverseList(List[Int]()))

  println(reverseList2(List(1, 2, 3, 4, 5)))

}

// O(n^2)
def reverseList[T](l: List[T]): List[T] =
  l match
    case Nil => Nil
    case h :: t => reverseList(t) ::: List(h)

// O(n)
def reverseList2[T](l: List[T]): List[T] =
  l.foldLeft(List[T]())((a, b) => b :: a)
