package playgraund.Variants

//                    O  - universal donor
//                 /  |  \
//                A   |   B
//                 \  |  /
//                   A B

object BloodGroups extends App {

  val donorO_1: Donor[Blood.O] = new User("u1") with Donor[Blood.O] {
    val give = new Blood with Blood.O
  }

  val recipientA_1 = new User("u2") with Recipient[Blood.A] {
    def receive(blood: Blood.A): Unit = ()
  }

  val recipientB_1 = new User("u2") with Recipient[Blood.B] {
    def receive(blood: Blood.B): Unit = ()
  }

  val recipientAB_1 = new User("2") with Recipient[Blood.AB] {
    def receive(blood: Blood.AB): Unit = ()
  }

  val DonorOorA = () => {
    val rand = new scala.util.Random
    rand.nextInt(2) match
      case 0 =>
        new User("u1") with Donor[Blood.O] {
          val give = new Blood with Blood.O
        }
      case 1 =>
        new User("u2") with Donor[Blood.A] {
          val give = new Blood with Blood.A
        }
  }

  recipientAB_1.receive(DonorOorA().give)

  recipientA_1.receive(donorO_1.give)

  println(donorO_1)

  recipientA_1.receive(DonorOorA().give)

  val Adonor: Donor[Blood.A] = new User("I") with Donor[Blood.O] {
    val give = new Blood with Blood.O
  }

  val listOfADonors = new User("I") with Donor[Blood.AB] {
    val give = new Blood with Blood.AB
  } :: List(donorO_1, DonorOorA())

  val rr = DonorOorA()

  val p1: List[Recipient[Blood.B]] = List(RecipientAB(), RecipientB())

  val a = p1.head

  val rez = a match
    case _: RecipientB => "B"
    case _: RecipientAB => "AB"

  println(rez)

}

case class RecipientB() extends Recipient[Blood.B]:
  override def receive(blood: Blood.B): Unit = ()

case class RecipientAB() extends Recipient[Blood.AB]:
  override def receive(blood: Blood.AB): Unit = ()

abstract class Blood

object Blood:
  trait AB

  trait A extends AB

  trait B extends AB

  trait O extends A with B

end Blood

trait Donor[+T]:
  def give: T

trait Recipient[-T]:
  def receive(blood: T): Unit

transparent abstract case class User(name: String):
  override def toString: String = s"Users`s name is $name"
