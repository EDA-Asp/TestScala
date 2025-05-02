package playgraund.AbsMembers

object AbstractMembers extends App {

  val someAnimal = new Animal:
    override type SuitableFood = Food
    override def eat(food: Food): Unit = println(food)

  someAnimal.eat(Fish())

  Cow().eat(Grass())
  // Cow().eat(Fish())
  Bear().eat(Fish())

  val someDog = new Dog

  val f = new someDog.SuitableFood

  Bear().eat(new someDog.SuitableFood)


  //Refinement types

  var animals: List[Animal { type SuitableFood <: Grass }] = List(
    new Animal {
      override type SuitableFood = Grass
      override def eat(food: SuitableFood): Unit = {}
    },
    new Animal {
      override type SuitableFood = Grass & Fish
      override def eat(food: SuitableFood): Unit = {}
    }

  )

  var a = animals.head

  type MeatEaters = Animal {type SuitableFood <: Meat}

  var animals2: List[MeatEaters] = List(
    new Animal:
      override type SuitableFood = Meat

      override def eat(food: SuitableFood): Unit = ()
  )

}

trait Fresh

trait Food
case class Grass() extends Food
case class Meat() extends Food
case class Fish() extends Food

abstract class Animal:
  // can pass subtypes of Food
  type SuitableFood <: Food
  def eat(food: SuitableFood): Unit

class Cow extends Animal:
  // method eat has a different signature than the overridden declaration
  // override def eat(food: Grass): Unit = {}

  override type SuitableFood = Grass

  override def eat(food: SuitableFood): Unit = {}

class Bear extends Animal:

  override type SuitableFood = Meat | Fish

  override def eat(food: SuitableFood): Unit = {}

class Dog extends Animal:
  override type SuitableFood = Meat

  override def eat(food: SuitableFood): Unit = {}
