package playgraund.Futures

import playgraund.Futures.Futures.SotialNetwork.{getFriends3, tryrun1, tryrun2}

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

object Futures:

  object SotialNetwork:

    case class Profile(name: String, id: Int)

    val names = Map(
      1->"Dima",
      2->"Nastya",
      3->"Chybaka"

    )

    val friends = Map(
      1 -> List(2,3),
      2 -> List(1),
      3 -> List(1)
    )


    def getProfile3(id: Int): Future[Option[Profile]] =
      Future{
        names.get(id).map(n=>Profile(n,id))
      }


    def getProfiles3(ids: List[Int]): Future[List[Profile]] =

      Future.sequence(ids.map(i => getProfile3(i))).map(l => l.flatten)


    def getFriends3(id: Int) =

      val futureProfile = getProfile3(id)
      val z = futureProfile.map(x => x.toList.flatMap(y => friends.getOrElse(y.id, List()))).flatMap(getProfiles3)
      z


    def fetpchProfile2(id: Int): Future[Profile] = Future {
      Thread.sleep(100)
      Profile(names(id), id)
    }

    def fetchBestFriends2(profile: Profile): Future[List[Profile]] = Future {
      Thread.sleep(100)
      friends(profile.id).map(i=>Profile(names(i),i))
    }
//
//    def fetchBestFriends3(profile: Profile) =
//      val p = fetpchProfile(profile.id).
//        transform(s=>s.flatMap(p=>Some(fetchBestFriends(p))),t=>t)
//    
      
      
//    
//    val l1 = List(Future(1),Future(2), Future(throw Exception("failed future")))
//    val seq = Future.sequence(l1.map(f=>f.transform(x=>x+1,t=>t))
//    

//
//    def fetchBestFriends4(profile: Profile) = Future {
//
//      Thread.sleep(100)
//      
////      val z = friends.get(profile.id).flatMap(li => Some(li.map(fetpchProfile)))
////      z
//      
//      for
//        a1 <- friends.get(profile.id)
//        a2 <- a1
//      yield fetpchProfile(a2)
//        
//    }






    //      Future {
//      Thread.sleep(100)
//      val a = friends.getOrElse(profile.id,List())
//      a.map(fetpchProfile)
//      
//    }
    
    
    


    def tryrun2(id: Int) =
      for
        p <- fetpchProfile2(id)
        f <- fetchBestFriends2(p)
      yield
        println(f)


    def fetpchProfile(id: Int): Future[Option[Profile]] = Future {
      Thread.sleep(100)
      names.get(id) match
        case Some(n) => Some(Profile(n,id))
        case None => None
    }

    val r = List(fetpchProfile(1),fetpchProfile(2))

    def fetchBestFriends(profile: Profile): Future[List[Future[Option[Profile]]]] = Future{
      Thread.sleep(100)
      val r = friends.get(profile.id) match
        case None => List()
        case Some(x) => x.map(x => fetpchProfile(x))
      r
    }


    def tryrun1(id: Int) =
      val z = fetpchProfile(id).flatMap {
      case None => Future(None)
      case Some(p) => fetchBestFriends(p)
    }

      for
        a1 <- z
        a2 <- a1
        a3 <- a2
        a4 <- a3
      do
        println(a4)
        
    
//    def getListOfFriends(id:Int): Future[List[Profile]] =
      






  def someCalculation(): Int = {
    println("start calculation")
    Thread.sleep(1000)
    println("end calculation")
    42
    //throw Exception("error")
  }
  //
  val jvmPool: ExecutorService = Executors.newFixedThreadPool(4)
  given executionContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(jvmPool)


  def ftx2(x: Int): Future[Int] = Future {
    Thread.sleep(1000)
    x*2
  }

  def ftadd100(x: Int): Future[Int] = Future {
    Thread.sleep(1000)
    x+100
  }

  object BlockingFutures:

    case class User(name: String)

    case class Transaction12(user: User, merchant: String, amount: Double, status: String = "Success")

    def fetchUser(name: String): Future[User] = Future{
      Thread.sleep(1000)
      User(name)
      throw Exception("My ex")
    }

    def createTransaction (user: User, mecrhant: String, amount: Double): Future[Transaction12] = Future{
      Thread.sleep(1000)
      Transaction12(user,mecrhant,amount)
    }

    def purchase(username: String, merchant: String, price: Double): Try[String] =
      //1. Fetch user
      //2. create transaction
      // Wait

      val transcationStatus: Future[String] =
        for
          user <- fetchUser(username)
          tr <- createTransaction(user, merchant, price)
        yield
          tr.status

    // blocking call


      Try {
        Await.result(transcationStatus, 5.seconds) // it can throw exceptions
      }


  @main
  def main(): Unit =


    tryrun1(1)

    tryrun2(1)


    getFriends3(1).onComplete {
      case Failure(exception) => println(exception)
      case Success(value) => value.foreach(p=>println(p))
    }
//
//    val rr = BlockingFutures.purchase("s","a",100)
//
//    rr match {
//      case Success(v) => println(v)
//      case Failure(e) => println(s"some e as $e")
//    }
//
//    val z = for
//      x<-ftx2(10).recover{
//        case e: Throwable => 0
//      }
//      y<-ftadd100(x+10)
//    yield {
//      println(y)
//      y
//    }
//
//
//    println("Futures")
//
//    val ft1 = Future(someCalculation()) //Starts immediately
//
//    ft1.onComplete{
//      case Success(value) => println(s"callback receive $value"); 10
//      case Failure(ex) => println(s"callback receive $ex"); 100
//    }
//
//
//    val r = ft1.value // Option[Try[Int]]
//    println(r)
//
//    //val i = Await.result(ft1,2.seconds)
//    //println(i)
//    println("Done")




    //jvmPool.shutdown()


