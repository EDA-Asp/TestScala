package playgraund.Variants

object SomeVariants extends App {

  val africanSwallows = Nil.prepend(AfricanSwallow())
  val beards: MyList[Beard] = africanSwallows

  val moreBeard = beards.prepend(EuropeanSwallow())

  val afAndEuSw = africanSwallows.prepend(EuropeanSwallow())

  val c = beards.prepend(EuropeanSwallow())

  val strange: MyList[Int] = Nil.prepend(1)

  val b = c.h

  b match
    case _: AfricanSwallow => println("a")
    case _: EuropeanSwallow => println("E")

  val firstChar: String => Option[Char] = _.headOption

  val q: String => Option[Char] = _.headOption

  val accesserA = List(1, 2, 3).lift(10)

  val sasd = Some(1)

  Seq("foo", "bar", "baz") match {
    case firstChar.unlift.elementWise(c0, c1, c2) =>
      println(s"\\$c0, \\$c1, \\$c2") // Output: f, b, b
  }
  val ua = Function.unlift(f)
  val rez = ua(4)

  def f(x: Int) = if x > 10 then Some(x) else None

  val foo = Foo[Int](x = 10)
  val fooX: Any = foo.x

}

trait MyList[+T]:
  def prepend[U >: T](elem: U): NonEmptyList[U] = NonEmptyList(elem, this)

case class NonEmptyList[T](h: T, t: MyList[T]) extends MyList[T]

object Nil extends MyList[Nothing]

trait Beard

case class AfricanSwallow() extends Beard
case class EuropeanSwallow() extends Beard

class Foo[+T](val x: T)
