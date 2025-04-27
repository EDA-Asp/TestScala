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


  val ff1: (Int => Option[String]) = (x)=> if x > 5 then Some("gt 5") else None
  val ff22: (String => Option[Int]) = s => Some(s.length)

  val ff2: (String => Int) = s => s.length
  
  val ff3: Int => Int = x=> x *10
  

  val ff1Andff2: (Int => Option[Int]) = x => ff1(x).map(ff2 andThen ff3)
  
  val ff1Andff22: (Int => Option[Int]) = x => 
    for 
      a <- ff1(x)
      b <- ff22(a)
    yield b
      
      
    
  




}

def composeFunctions[A, B, C, D](f: A => B, g: B => C, h: C => D): A => D =
  // f andThen g andThen h
  h compose g compose f

def composeFunctions2Args[A, B, C](f: (A, A) => (B, B), g: (B, B) => (C, C)): ((A, A)) => (C, C) =
  f.tupled andThen g.tupled


def getIntOption(a: Int) = if a > 5 then Some(1) else Some(a *10)

def addOne(i: Int) = Some(i +1)

def toStr(i: Int) = if i > 15 then Some("gt 15") else None