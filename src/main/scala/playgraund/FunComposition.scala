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

  val ff1: (Int => Option[String]) = (x) => if x > 5 then Some("gt 5") else None
  val ff22: (String => Option[Int]) = s => Some(s.length)

  val ff2: (String => Int) = s => s.length

  val ff3: Int => Int = x => x * 10

  val ff1Andff2: (Int => Option[Int]) = x => ff1(x).map(ff2 andThen ff3)

  val ff1Andff22: (Int => Option[Int]) = x =>
    for
      a <- ff1(x)
      b <- ff22(a)
    yield b

  def func1(): Option[Int] = Some(1)

  def func2(x: Int): Option[String] = Some((x + 10).toString)

  def func3(x: String): Option[Int] = Some(x.length)

  val func11: () => Option[Int] = () => Some(1)

  val func22: Int => Option[String] = x => Some((x + 10).toString)

  val func33: String => Option[Int] = x => Some(x.length)

  def composedFunction(): Option[Int] =
    func1().flatMap(func2).flatMap(func3)

  def composedFunction2(): Option[Int] =
    func1().flatMap(a => func2(a).flatMap(b => func3(b)))

  val q3 = x => func33(x)

  val q2 = (x: Int) => func22(x).flatMap(q3)

  val func22then3 = func22(_: Int).flatMap(q3)

  val z = () => func11().flatMap(func22then3)

  val rere = composedFunction2()
}

def composeFunctions[A, B, C, D](f: A => B, g: B => C, h: C => D): A => D =
  // f andThen g andThen h
  h compose g compose f

def composeFunctions2Args[A, B, C](f: (A, A) => (B, B), g: (B, B) => (C, C)): ((A, A)) => (C, C) =
  f.tupled andThen g.tupled

def getIntOption(a: Int) = if a > 5 then Some(1) else Some(a * 10)

def addOne(i: Int) = Some(i + 1)

def toStr(i: Int) = if i > 15 then Some("gt 15") else None
