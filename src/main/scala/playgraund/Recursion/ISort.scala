package playgraund.Recursion

object ISort extends App {

  val a = isort(List(4, 2))

  println(a)

}

def isort(l: List[Int]): List[Int] =

  def insert(i: Int, l: List[Int]): List[Int] =
    l match
      case Nil => i :: Nil
      case h :: t => if i <= h then i :: l else h :: insert(i, t)

  l match
    case Nil => Nil
    case h :: t => insert(h, isort(t))
