package playgraund.Monads.MyIOMonad

import playgraund.Monads.MyIOMonad.MyIO.pure

class MyIO[A](val unsafeRun: () => A):

  def map2[B](f: A => B): MyIO[B] = MyIO(() => f(unsafeRun()))

  def map[B](f: A => B): MyIO[B] = this.flatMap(x => pure(f(x)))

  def flatMap[B](f: A => MyIO[B]): MyIO[B] = pure(f(unsafeRun()).unsafeRun())

object MyIO:
  def pure[A](x: => A): MyIO[A] = MyIO(() => x)

@main
def main(): Unit =

  val io1 = MyIO(() => { println("io1"); 1 + 2 })
  val f1: Int => MyIO[String] = (x: Int) => MyIO({ println("Io2"); (x * 10).toString })

  val io2 = io1.flatMap(f1)

  println("@@@@@@@@@@@@@@@@@@@@")

  println(io2.unsafeRun())
  println(io2.unsafeRun())

  println("@@@@@@@@@@@@@@@@@@@@")

  val z = io1.map(x => x * 15).map(x => x * 3)

  println("#######################")

  println(z.unsafeRun())
  println(z.unsafeRun())

  println("////////////////")
  val zz = for
    a <- io1
    b <- f1(a)
  yield b

  println("////////////////")
  println(zz.unsafeRun())
