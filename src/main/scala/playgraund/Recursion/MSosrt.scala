package playgraund.Recursion

object MSosrt extends App {

  msort(List(1, 2, 3))(_ < _)

}

def msort[A](l: List[A])(less: (A, A) => Boolean): List[A] =

  def merge(left: List[A], right: List[A]): List[A] =
    (left, right) match
      case (Nil, _) => right
      case (_, Nil) => left
      case (l :: ll, r :: rr) => if less(l, r) then l :: merge(ll, right) else r :: merge(left, rr)

  val m = l.length / 2
  if m == 0 then l
  else
    val (left, right) = l.splitAt(m)
    merge(msort(left)(less), msort(right)(less))
