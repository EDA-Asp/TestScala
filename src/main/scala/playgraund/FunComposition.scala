package playgraund

object FunComposition extends App {

  val f1 = (x: Int) => x + 1

  println(composeFunctions(f1, f1, f1)(1))

  val f2 = (x: Int, y: Int) => (x + 1, y)

  println(composeFunctions2Args(f2, f2)(1, 1))

  val lf = List((x: Int) => x + 1, (x: Int) => x + 2)

  val r3 = lf.fold((x: Int) => x + 1)((f1, f2) => f1 andThen f2)

  val r4 = lf.fold((x: Int) => x + 1)(_ andThen _)

  println(r3(1))

  println(r4(1))

}

def composeFunctions[A, B, C, D](f: A => B, g: B => C, h: C => D): A => D =
  // f andThen g andThen h
  h compose g compose f

def composeFunctions2Args[A, B, C](f: (A, A) => (B, B), g: (B, B) => (C, C)): ((A, A)) => (C, C) =
  f.tupled andThen g.tupled
