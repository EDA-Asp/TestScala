package playgraund.Recursion

import scala.annotation.tailrec

object Primes extends App {
  println(isPrime(13))

  println(NPrimes(7))
}

def isPrime(x: Int): Boolean =
  val m = x / 2 + 1

  @tailrec
  def isPrimeAux(x: Int, d: Int): Boolean =
    if d > m then true
    else if x % d == 0 then false
    else isPrimeAux(x, d + 1)

  x match
    case 1 => true
    case 2 => true
    case _ => isPrimeAux(x, 2)

def NPrimes(n: Int): List[Int] =

  def nextPrime(l: List[Int]): List[Int] =
    @tailrec
    def auxNPrime(x: Int): List[Int] =
      if l.exists(i => x % i == 0 && i != 1) then auxNPrime(x + 1) else x :: l
    auxNPrime(l.head)

  @tailrec
  def eratosphene(n: Int, l: List[Int]): List[Int] =
    if n == 0 then l
    else eratosphene(n - 1, nextPrime(l))

  n match
    case 1 => List(1)
    case 2 => List(1, 2)
    case 3 => List(1, 2, 3)
    case _ => eratosphene(n - 3, List(3, 2, 1))
