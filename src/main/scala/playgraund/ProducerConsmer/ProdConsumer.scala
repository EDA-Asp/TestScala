package playgraund.ProducerConsmer

import scala.collection.mutable
import scala.util.Random

object ProdConsumer extends App {

  val nt = Thread(() => {
    println("start nt")
    Thread.sleep(1000)
    println("end nt")
  })

  nt.start()
  println("a")

//  val a = ProdConsumerV3.start()
//  println(a)

}

def consumerLog(s: String): Unit =
  println(s"[consumer] $s")

def producerLog(s: String): Unit =
  println(s"[producer] $s")

object ProdConsumerV3:

  def start(): Int =

    val queue = mutable.Queue[Int]()

    val consumer = new Thread(() => {
      val random = Random(System.nanoTime())
      while (true)
        queue.synchronized {
          if queue.isEmpty then
            consumerLog("Queue is empty")
            queue.wait()
          val v = queue.dequeue()
          queue.notify()
          consumerLog(s"get value $v")
        }
        Thread.sleep(random.nextInt(500))

    })

    val producer = new Thread(() => {
      val random = Random(System.nanoTime())
      var cnt = 0

      while (true)
        queue.synchronized {
          if queue.size > 5 then
            producerLog("Queue is full")
            queue.wait()
          queue.enqueue(cnt)
          queue.notify()
          producerLog(s"put value $cnt")
          cnt += 1
        }
        Thread.sleep(random.nextInt(500))
    })

    producer.start()
    consumer.start()

    10
