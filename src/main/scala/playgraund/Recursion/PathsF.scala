package playgraund.Recursion

object PathsF extends App {
  println(backtracking(1,2))
}


def backtracking(start: Int, steps: Int): List[List[Int]] =
  steps match
    case 0 => List(List(start))
    case _ =>
      for
        f <- List((x: Int) => x + 1, (x: Int) => x + 2)
        k <- backtracking(f(start), steps - 1)
      yield start +: k
