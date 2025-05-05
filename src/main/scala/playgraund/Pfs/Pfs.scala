package playgraund.Pfs

object Pfs extends App {

  val l = List(1, 2, 3)
  // val rez = l.map(pf) // Match Error

  val rez2 = l.map { x =>
    x match {
      case 1 => 2
      case _ => 2
    }

  }
}

val pf: PartialFunction[Int, Int] =
  case 1 => 2
  case 2 => 3
