package playgraund.Katas

@main
def main(): Unit =

  val r = smash_mkString(List("aa"))

  println(s"smash =$r")

def smash_mkString(words: List[String]): String =
  words.mkString(" ")

def smash_reduce(words: List[String]): String =
  if (words.nonEmpty) words.reduce(_ + " " + _) else ""

def smash_reduceOption(words: List[String]): String =
  words.reduceOption(_ + " " + _).getOrElse("")
