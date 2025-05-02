package As

object As extends App{

  import Box.*

  val da1 = new Donor[A] {
    override def give: A = new A {}
  }

  val db1 = new Donor[B] {
    override def give: B = new B {}
  }

  val ba = Box.BoxDonorA(da1)


  val bb = Box.BoxDonorB(db1)


  val lb = List(ba,bb)

  lb.foreach(x=>println(x.money))

  def salary[T<:Donor[AB]](b: Box[T]): Int =
    b match {
      case x:BoxDonorA => x.money
      case BoxDonorB(_) => 20
      case BoxDonorAB(_) => 30
    }

  import DonorE.*
  val da = DonorA()
  val db = DonorB()

  val lde = List(da,db)

  def salary2[T<:AB](d: DonorE[T]): Int =
    d match {
      case DonorA() => 10
      case DonorB() => 10
      case DonorAB() => 20
      case DonorO() => 30
    }

  println("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
  lde.map(x=>salary2(x)).foreach(x=>println(x))


}


sealed trait AB:
  override def toString: String = "AB"
trait A extends AB:
  override def toString: String = "B"
trait B extends AB:
  override def toString: String = "A"
trait O extends A with B:
  override def toString: String = "0"







abstract class Donor[+T<:AB]:
  def give: T


enum Box[+T<:Donor[AB]](contents: Donor[?],val money: Int):
  case BoxDonorA(n: Donor[A]) extends Box[Donor[A]](n,10)
  case BoxDonorB(n: Donor[B]) extends Box[Donor[B]](n,20)
  case BoxDonorAB(n: Donor[AB]) extends Box[Donor[AB]](n,30)



enum DonorE[+T <: AB]:

  def give: T =
    this match {
      case DonorA()=> new A {}
      case DonorB() => new B {}
      case DonorAB() => new AB {}
      case DonorO() => new O {}
    }

  case DonorA() extends DonorE[A]
  case DonorB() extends DonorE[B]
  case DonorAB() extends DonorE[AB]
  case DonorO() extends DonorE[O]


