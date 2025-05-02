package playgraund.StructuralTypes

import scala.language.reflectiveCalls
import scala.reflect.Selectable.reflectiveSelectable

object StructuralTypes extends App {

  val listOfFlyers: List[Flyer] = List(Duck(), Eagle())

  listOfFlyers.foreach(_.fly())

  val p1 = Record("age" -> 10, "name" -> "A").asInstanceOf[Person]

  println(s"person is ${p1.name} and ${p1.age}")

}

class Duck:
  def fly(): Unit = print("Ducks fly together")

class Eagle:
  def fly(): Unit = print("Eagles fly better than MJ")

class Walrus:
  def swim(): Unit = print("I am faster on the water than on the land")

type Flyer = { def fly(): Unit }

def callFly(thing: Flyer): Unit = thing.fly()

class Record(elems: (String, Any)*) extends Selectable:
  private val fields = elems.toMap
  def selectDynamic(name: String): Any = fields(name)

type Person = Record { val name: String; val age: Int }
