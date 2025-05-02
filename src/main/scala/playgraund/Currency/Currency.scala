package playgraund.Currency

object Currency extends App{

  
}

abstract class Currency1:
  val amount: Long
  def designation: String
  override def toString = s"$amount $designation"
  def + (that: Currency1): Currency1
  def * (x: Double): Currency1

