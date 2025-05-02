package playgraund.Recursion

object FlattenListOfInt extends App {
  println(flatten(List(1, 2, List(3, 4, List(5, 6)))))
}

def flatten(l: List[Any]): List[Any] =
  l match
    case Nil => Nil
    case h :: t =>
      h match
        case x: Int => h :: flatten(t)
        case x: List[Any] => flatten(x) ::: flatten(t)
