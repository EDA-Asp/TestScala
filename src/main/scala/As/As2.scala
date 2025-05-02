package As



object As2 extends App{

}


enum Eee:
  case Foo1()
  case Foo2()
  case Foo3()

final class Cnt[+T](val x: T)


import Eee.*

val cf1 = new Cnt(new Foo1())
val cf2 = new Cnt(new Foo2())
val cf3 = new Cnt(new Foo3())

val l = List(cf1,cf2,cf3)

