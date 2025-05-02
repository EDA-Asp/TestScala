package playgraund.Variants

object AnimalsVariance extends App {

  val superVet = Vet[Animal]

  val c = superVet.rescue(new Cat {})
  val d = superVet.rescue(new Dog {})

  val catVet = Vet[Cat]

  val another_c = catVet.rescue(new Cat {})
  // val another_d = catVet.rescue(new Dog {})

  val aaaa = Iterable.apply("1")
}

sealed trait Animal
trait Cat extends Animal
trait Dog extends Animal

class Vet[-T <: Animal]:
  def rescue[S <: T](animal: S): S = animal
