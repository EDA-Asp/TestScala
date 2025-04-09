package playgraund

object MultiTable extends App {
  println(table(10, 5))

  println(padLines("a\nasd\nasdasd", 4))

  println(Mx(10, 10))
}

def makeRow(row: Int, y: Int): String =
  val r =
    for i <- 1 to y yield
      val col = (row * i).toString
      padLine(col)
  r.mkString

def table(x: Int, y: Int): String =
  val t =
    for r <- 1 to x yield makeRow(r, y)
  t.mkString("\n")

def padLine(line: String, minWidth: Int = 4): String =
  line.length match
    case l if l < minWidth => " " * (minWidth - l) + line
    case _ => line

def padLines(text: String, minWdh: Int): String =

  val paddedLines =
    for line <- text.linesIterator yield padLine(line)

  paddedLines.mkString("\n")

def Mx(x: Int, y: Int) =
  val r = for i <- 1 to x yield
    val l = for j <- 1 to y yield padLine((i * j).toString)
    l.mkString
  r.mkString("\n")

//
//def Mx2(x: Int, y: Int) =
//  val m = for i <- 1 to x yield
//    val r = for j <- 1 to y yield
//      padLine((i * j).toString)
//    r.mkString
//  m.mkString("\n")
