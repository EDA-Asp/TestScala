package playgraund.Recursion

import scala.annotation.tailrec

object PathsF extends App {
  println(backtracking(1, 2))

  println(solve())

  println(solve2())
}

def backtracking(start: Int, steps: Int): List[List[Int]] =
  steps match
    case 0 => List(List(start))
    case _ =>
      for
        f <- List((x: Int) => x + 1, (x: Int) => x + 2)
        k <- backtracking(f(start), steps - 1)
      yield start +: k

def movef(fs: List[Int => Int])(p: List[Int]): List[List[Int]] =
  fs.map(f => p :+ f(p.last))

val fsl: List[Int => Int] = List((x: Int) => x + 1, (x: Int) => x + 2)

def move = movef(fsl)

@tailrec
def solve(from: List[List[Int]] = List(List(1)), steps: Int = 2): List[List[Int]] =
  if steps == 0 then from
  else
    val rez = from.flatMap(x => move(x))
    solve(rez, steps - 1)

def solve2(from: List[List[Int]] = List(List(1)), steps: Int = 2): List[List[Int]] =

  steps match
    case 0 => from
    case _ =>
      for
        fr <- from
        n <- solve2(move(fr), steps - 1)
      yield n

def solve3(from: List[List[Int]] = List(List(1)), steps: Int = 2): List[List[Int]] =

  steps match
    case 0 => from
    case _ =>
      from.flatMap(fr => solve3(move(fr), steps - 1).map(n => n))
