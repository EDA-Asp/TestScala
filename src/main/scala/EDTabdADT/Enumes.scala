package EDTabdADT

import EDTabdADT.Vie.*

object Enumes extends App {

  val v1 = Refl((x: Animal) => new Animal {})
  val v2 = Refl((x: Dog) => new Dog {})
  val v3: Vie[Dog] = v1

  val z = v3 match {
    case x: Refl[d] => x.f(new Dog {}) // new Cats {} do not compile
  }

  println(z)
//
//  val a = CustumerT.OpenC(1)
//  val b = CustumerT.CloseC("asd")
//
//  val a = summon[Productable[CustumerT]]

}

enum Vie[-T]:
  case Refl[R](f: R => R) extends Vie[R]

trait Animal
trait Dog extends Animal
trait Cat extends Animal

enum CustumerT:
  case OpenC(a: Int)
  case CloseC(a: String)

trait Productable[A]:
  def produce: A

given Productable[Int] with
  override def produce: Int = 100

given Productable[String] with
  override def produce: String = "AAA"
