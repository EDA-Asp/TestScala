type Coordinates = (Int, Int)


final case class BoardPosition private(x: Int, y: Int)

object BoardPosition {
  def apply(c: Coordinates): Option[BoardPosition] =
    c match
      case (x, y) if 0 <= x && x <= 8 && 0 <= y && x <= 8 => Some(new BoardPosition(x, y))
      case _ => None
}


val a = BoardPosition.apply(100,100)
