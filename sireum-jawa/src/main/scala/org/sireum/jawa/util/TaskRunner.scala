package org.sireum.jawa.util

import scala.util._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

object TaskRunner {
  private final val TITLE = "TaskRunner"
  def execute[T](task : Task[T], timeoutOpt : Option[Int] = None) : Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val (f, cancel) = FutureUtil.interruptableFuture[T] { () =>
      task.run
    }
    f.onComplete {
      case Success(sth) =>
        println(TITLE, sth.toString())
      case Failure(ex) =>
        System.err.println(TITLE, ex.getMessage)
    }
    try{
      val d = timeoutOpt match {
        case Some(t) => Duration(t, "s")
        case None => Duration("Inf")
      }
      Await.result(f, d)
    } catch {
      case te : Throwable => 
        cancel()
    }
    
  }
}

trait Task[T] {
  def run : T
}