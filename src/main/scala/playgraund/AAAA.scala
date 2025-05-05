package playgraund

object AAAA extends App {

  s.subscribe(o)
  s.publish()
  s.value = 10
  val t = Time()
  t.hour = 11
  println(t.hour)
  import Myo.*

  val resort = AdultOnlyResort()
  // resort.makeReservation(persons) // Compilation Error!!
  val adults = getAdults(persons)
  println(adults)
  resort.makeReservation(adults)

}

trait Observer:
  def notify(sub: Subject): Unit

trait Subject:
  private var observers: List[Observer] = List()
  def subscribe(obs: Observer): Unit =
    observers = obs :: observers
  def publish(): Unit =
    for obs <- observers do obs.notify(this)

class MySubject(val label: String) extends Subject:
  private var v = 1
  def value: Int = v
  def value_=(v: Int): Unit = {
    this.v = v
    publish()
  }

class MyObserver extends Observer {
  override def notify(sub: Subject): Unit =
    val xaxa = sub.asInstanceOf[MySubject]
    println(s"${xaxa.label} has value ${xaxa.value}")

}

val s = MySubject("A1")

val o = MyObserver()

class Time:
  private var h = 12
  private var m = 0
  def hour: Int = h
  def hour_=(x: Int): Unit =
    require(0 <= x && x < 24)
    h = x
  def minute = m
  def minute_=(x: Int): Unit =
    require(0 <= x && x < 60)
    m = x

//https://www.turingtaco.com/quick-review-of-scala-3s-existential-and-refinement-types/
object Myo {
  type True = true
  type Adult = Person { val isAdult: True } // require(age > 18)

  case class Person(name: String, age: Int):
    val isAdult: Boolean = age >= 18

    def toAdult: Option[Adult] =
      if isAdult then
        Some(new Person(name, age) {
          override val isAdult: True = true
        })
      else None

  val persons = List(
    Person("Alain", 42),
    Person("Rita", 35),
    Person("Lolita", 16)
  )

  def getAdults(people: Seq[Person]): Seq[Adult] =
    for {
      p <- people
      if p.isAdult
    } yield p.toAdult.get

  class AdultOnlyResort():
    def makeReservation(guests: Seq[Adult]) =
      for (guest <- guests) do println(s"Made made reservation for ${guest}")

}
