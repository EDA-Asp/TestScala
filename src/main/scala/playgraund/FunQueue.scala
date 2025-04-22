package playgraund

object FunQueue extends App {

  val a = Queue(1,2,3,4)
  a.head
  a.tail
  val b = a.enqueue(1).enqueue(2)
  println(b)


}

class Queue[A] private (private val leading: List[A], private val trailing: List[A]):

  private def mirror: Queue[A] =
    if leading.isEmpty then new Queue[A](trailing.reverse, Nil)
    else this

  def enqueue(x: A): Queue[A] = new Queue[A](leading, x :: trailing)

  def head: A = mirror.leading.head

  def tail: Queue[A] =
    val q = mirror
    new Queue[A](q.leading.tail,q.trailing)

  override def toString: String =
    val l = leading ::: trailing.reverse
    l.toString()

object Queue:
// constructs a queue with initial elements ‘xs'
  def apply[T](xs: T*) = new Queue[T](xs.toList, Nil)