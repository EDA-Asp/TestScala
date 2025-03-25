package playgraund.Katas

import scala.annotation.tailrec

object KnightKata extends App {

  val from = BoardPosition((5, 5))
  val to = BoardPosition((6, 6))

  val rez = for
    f <- from
    to <- to
  yield solve(f, to, 5)

  println(rez)

}

type Coordinates = (Int, Int)

final case class BoardPosition private (x: Int, y: Int)

object BoardPosition {
  def apply(c: Coordinates): Option[BoardPosition] =
    c match
      case (x, y) if 0 <= x && x <= 8 && 0 <= y && x <= 8 => Some(new BoardPosition(x, y))
      case _ => None

}

def knightMoves(): List[BoardPosition => Option[BoardPosition]] =
  val cc =
    (for
      i <- List(1, -1)
      j <- List(2, -2)
    yield List(
      (from: BoardPosition) => BoardPosition.apply(from.x + i, from.y + j),
      (from: BoardPosition) => BoardPosition.apply(from.x + j, from.y + i)
    )).flatten
  cc

def figureMove(moves: () => List[BoardPosition => Option[BoardPosition]])(
    from: Option[BoardPosition]
): List[BoardPosition] =
  val r: List[BoardPosition] =
    for
      i <- moves()
      f <- from
      j <- i(f)
    yield j
  r

def knightMove = figureMove(knightMoves)

val solve: (BoardPosition, BoardPosition, Int) => Boolean = (from, to, n) =>

  @tailrec
  def solver(from: List[BoardPosition], to: BoardPosition, n: Int): Boolean =
    if (n < 0) false
    else if (from.contains(to)) true
    else
      val c = from.flatMap(p => knightMove(Some(p)))
      solver(c, to, n - 1)

  solver(List(from), to, n)
