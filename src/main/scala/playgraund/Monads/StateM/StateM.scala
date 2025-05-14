package playgraund.Monads.StateM

case class Shift(distance: Int)

case class State[S, A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State[S, B](s => {
      val (a, s1) = this.run(s)
      f(a).run(s1)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))
}

object State {
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def runS[S, A](state: State[S, A], s: S): S =
    state.run(s)._2

  def runA[S, A](state: State[S, A], s: S): A =
    state.run(s)._1
}

@main
def main(): Unit =
//  val program: State[Int, Int] = for {
//    x <- State.get[Int]
//    _ <- State.set[Int](x + 1)
//    y <- State.get[Int]
//  } yield x + y
//
//  val (result, finalState) = program.run(10)
//  println(result) // Output: 1
//  println(finalState) //

  val xz: State[Shift, Int] = State.unit(10)

  val (r, s) = xz.run(Shift(100))

  println(r)
  println(s)

//  val moove: State[Shift,Int] =
//    for
//      x <- State.get[Shift]
//      _ <- State.set[Shift](Shift(x.distance + 5))
//      y <- State.get[Shift]
//    yield x.distance + y.distance

//
//  val (result, finalState) = moove.run(Shift(10))
//
//  println(result)
//  println(finalState)

//  val dash: State[Shift,Int] = State(s=>(5,Shift(5)))
//
//  val r = dash.flatMap{x=>
//    State(s=>(5,Shift(5)))
//  }
//
//  val (rrr,sss) = r.run(Shift(10))
//
//  println(rrr)
//

//  val s1: State[Int,Int] = State(x=>(x+1,x))
