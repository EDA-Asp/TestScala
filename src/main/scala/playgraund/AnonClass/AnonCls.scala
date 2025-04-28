package playgraund.AnonClass

object AnonCls extends App {

  val point = new { val asdcd3 = 10; val y = 10 }

  // println(point.asdcd3)

  println(Foo().x)

}

class Foo:
  val x = 10
  val y = 10
