package playgraund.ProducerConsmer

import java.util.concurrent.Executors

//threads wrap runnables - java interface with run method

object JVMConcurency {

  def basicThreads(): Unit =
    val runnable = new Runnable {
      override def run(): Unit = {
        println("running on some thread")
      }
    }

    // define new thread
    val aThread = Thread(runnable)
    // run on another thread (JVM thread ~ OS thread)
    aThread.start()

  // order not guaranteed
  def orderOfExecutions(): Unit =
    val threadHello = Thread(() => 1 to 100 foreach (_ => println("Hello")))
    val threadGoodbye = Thread(() => 1 to 100 foreach (_ => println("Goodbye")))

    threadHello.start()
    threadGoodbye.start()

  def executorsExample(): Unit =
    // thread pool
    val threadPool = Executors.newFixedThreadPool(4)
    // threadPool.execute(()=> println("something in thread pool"))

    // need shutdown threadPool

    // parallel computation
    threadPool.execute { () =>
      println("1 w 1 s")
      Thread.sleep(1000)
      println("1 done")
    }

    threadPool.execute { () =>
      println("2 w 1 s")
      Thread.sleep(1000)
      println("2 w + 1 s")
      Thread.sleep(2000)
      println("2 done")
    }

    threadPool.shutdown()

  def concurencyProblem(): Int =
    var x = 0

    val t1 = Thread(() => x = 1)
    val t2 = Thread(() => x = 2)

    t1.start()
    t2.start()
    t1.join()
    t2.join()
    // println(x)
    x

  @main
  def main(): Unit = {
    // it is not parallel because in the same thread
    // runnable.run()

    // different results
    // orderOfExecutions()

    // main thread continues because thread pool alive
    // executorsExample()
    concurencyProblem()
//    val a = for
//      _ <- 1 to 1000
//      a <- concurencyProblem()
//    yield a
//    concurencyProblem()
//    concurencyProblem()
//    concurencyProblem()

    val r = (1 to 100000).toList.flatMap(i => {
      val a = concurencyProblem()
      if a != 2 then Some(a)
      else None
    })

//    val rr = r.filter(x=>x!=2)
    println(r)

  }

}
