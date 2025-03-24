package playgraund.Katas

object BoolToStrKata extends App {

  println(booleanToStringToString(true))

}

def booleanToStringToString(b: Boolean): String =
  b.toString

def booleanToStringInterpolated(b: Boolean): String =
  s"$b"
