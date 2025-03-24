package playgraund.Katas

import scala.annotation.tailrec

object SmashWordsKata extends App {

  val rez1 = smash_rec(List("a", "b"))
  println(rez1)

  val rez2 = smash_rec(List("a"))
  println(rez2)

  val rez3 = smash_rec(List())
  println(rez3)

}

def smash_mkString(words: List[String]): String =
  words.mkString(" ")

def smash_reduce(words: List[String]): String =
  if (words.nonEmpty) words.reduce(_ + " " + _) else ""

def smash_reduceOption(words: List[String]): String =
  words.reduceOption(_ + " " + _).getOrElse("")

def smash_rec(words: List[String], e: String = "", delimiter: String = " "): String =

  @tailrec
  def rec(l: List[String], acc: String): String =
    l match
      case Nil => acc
      case h :: t => rec(t, acc + delimiter + h)

  words match
    case Nil => e
    case h :: Nil => h
    case h :: t => rec(t, h)
