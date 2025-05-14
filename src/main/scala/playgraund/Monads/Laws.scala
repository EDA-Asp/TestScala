package playgraund.Monads

import scala.annotation.targetName
import scala.io.StdIn.readLine

val f = (x: Int) => List(x, x + 1)
val g = (x: Int) => List(x, x * 2)
val pure = (x: Int) => List(x)

@main
def main(): Unit = {
  // 1 law // left id
  // pure is a left-identity for bind:
  assert(pure(5).flatMap(g) == g(5))
  // 2 law // right id
  // pure is also a right-identity for bind:
  assert(List(1, 2, 3).flatMap(pure) == List(1, 2, 3))
  // associative
  // In mathematics, the associative property[1] is a property of some binary operations
  // that rearranging the parentheses in an expression will not change the result

  // http://lambda-the-ultimate.org/node/2448
  // http://lambda-the-ultimate.org/node/2448#comment-36775
  assert(List(1, 2, 3).flatMap(f).flatMap(g) == List(1, 2, 3).flatMap(x => f(x).flatMap(g)))

  val z: () => Int = () => 42
  PossibleMonad[Int](z).map(x => x * 2)

  pms()

  val mp1 = PossibleMonad { () =>
    println("first possible monad")
    42
  }

  val mp2 = PossibleMonad { () =>
    println("second possible monad")
    1
  }

  val f1 = (x: Int) =>
    PossibleMonad {
      println("pm in f1 x + 1")
      x + 1
    }

  val f2 = (x: Int) =>
    PossibleMonad {
      println("pm in f2 x * 10")
      x * 10
    }

  println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  val zdfda = mp1.flatMap(f1(_).flatMap(f2)).unsafeRun()
  println(zdfda)

  val zdfda2: PossibleMonad[Int] =
    for
      a <- mp1
      b <- f1(a)
      c <- f2(b)
    yield c

  println(zdfda2.unsafeRun())

  println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

  println("AAAAAAAAAAAAAAAAA!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  val asdas =
    for
      a <- mp1
      b <- mp2
      c <- PossibleMonad { println(s"$a and $b"); 1 }
    yield c

  val a = asdas.unsafeRun()
  println(a)

  val foc1 = mp1.flatMap(num1 => mp2.map(num2 => num1 + num2))

  val xaxa = mp1.flatMap(_ => mp2)

  val xaxa3 = PossibleMonad(() => { println("first"); "ABC" })
    .flatMap(s =>
      PossibleMonad { () =>
        println("second")
        s.length
      }
    )
    .flatMap(x =>
      PossibleMonad({ () =>
        println("third")
        x * 5
      })
    )

  println(foc1.unsafeRun())
  println(xaxa.unsafeRun())

  println(xaxa3.unsafeRun())

  val zz =
    PossibleMonad(println("Please enter your name:")).flatMap(x =>
      PossibleMonad(readLine()).flatMap(s => PossibleMonad(s"Hi $s"))
    )

  val zzz =
    for
      _ <- PossibleMonad(println("Name, sister, NAME!"))
      b <- PossibleMonad(readLine())
      c <- PossibleMonad(println(s"She said $b"))
    yield c

  zzz.unsafeRun()

  def asker(): PossibleMonad[String] = PossibleMonad {
    println("I Ask")
    readLine()
  }
  def printer(name: String): PossibleMonad[Unit] = PossibleMonad {
    println(s"she told $name")
  }

  val zzzz = asker().flatMap(printer)

}

case class PossibleMonad[A](unsafeRun: () => A):

  def map[B](f: A => B): PossibleMonad[B] = PossibleMonad(() => f(unsafeRun()))

  def flatMap[B](f: A => PossibleMonad[B]): PossibleMonad[B] =
    PossibleMonad(() => f(unsafeRun()).unsafeRun())
//f(unsafeRun())

object PossibleMonad:
  @targetName("pure")
  def apply[A](value: => A): PossibleMonad[A] = new PossibleMonad(() => value)

def pms(): Unit =
  val pure = (x: Int) => PossibleMonad(() => x)
  val f = (x: Int) => PossibleMonad(() => x + 1)
  val g = (x: Int) => PossibleMonad(() => x * 2)

  println(PossibleMonad(() => 42).flatMap(f).flatMap(g).unsafeRun())

//https://stackoverflow.com/questions/10291176/how-to-use-scala-trait-with-self-reference
//https://stackoverflow.com/questions/10291176/how-to-use-scala-trait-with-self-reference
//trait IO:
//  self: IO =>
//  def unsafeRun: Unit
//
//  def ++(io: IO): IO = new:
//    def unsafeRun: Unit =
//      self.unsafeRun
//      io.unsafeRun
//
