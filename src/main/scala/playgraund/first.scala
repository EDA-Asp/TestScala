package playgraund

@main
def main(): Unit =

  (1 to 2).foreach(println)

  for (i <- 1 to 2) do

    println(s"i = $i")