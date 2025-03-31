package playgraund.Recursion

import scala.annotation.tailrec

object GCD extends App {

  println(gcd(48, 18))

}

@tailrec
def gcd(a: Int, b: Int): Int =
  if b == 0 then a
  else gcd(b, a % b)
