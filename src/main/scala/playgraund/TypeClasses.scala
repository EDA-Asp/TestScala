package playgraund

object TypeClasses extends App {

  useTrf("ab",strTr1)

  increaseOne((i: Int) => i + 7)
}


trait Tr[A]:
  def useTr(x: A): Int
  def secondM(a:Int): Int

val strTr1 = new Tr[String]:
  def useTr(x: String): Int = x.length
  override def secondM(a: Int): Int = 10

val strTr2 = new Tr[String]:
  override def useTr(x: String): Int = x.length * 2

  override def secondM(a: Int): Int = 15

def useTrf[T](x: T,y:Tr[T]) = y.useTr(x)



trait Increaser:
  def increase(i: Int): Int

def increaseOne(increaser: Increaser): Int =
  increaser.increase(1)