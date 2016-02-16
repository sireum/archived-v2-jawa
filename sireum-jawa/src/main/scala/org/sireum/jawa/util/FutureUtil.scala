package org.sireum.jawa.util

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.atomic.AtomicReference
import scala.util.Try
import scala.util.Failure

object FutureUtil {
  def composeWaitingFuture[T](fut: Future[T], 
                                    timeout: Int, default: T): Future[T] =
  future { Await.result(fut, timeout seconds) } recover {
    case e: Exception => default
  }
  
  def cancellableFuture[T](fun: Future[T] => T)(implicit ex: ExecutionContext): (Future[T], () => Boolean) = {
    val p = Promise[T]()
    val f = p.future
    val funFuture = Future(fun(f))
    p tryCompleteWith funFuture                              // Scala 2.10
    
    (f, () => p.tryFailure(new CancellationException))       // Scala 2.10
  }
  
  def interruptableFuture[T](fun: () => T)(implicit ex: ExecutionContext): (Future[T], () => Boolean) = {
    val p = Promise[T]()
    val f = p.future
    val aref = new AtomicReference[Thread](null)
    val funFuture = Future {
      val thread = Thread.currentThread
      aref.synchronized { aref.set(thread) }
      try fun() finally {
        val wasInterrupted = (aref.synchronized { aref getAndSet null }) ne thread
        // Deal with interrupted flag of this thread in desired
      }
    }
    funFuture.onComplete(p tryComplete(_))                    // Akka 2.0
//    p tryCompleteWith funFuture                             // Scala 2.10
 
    (f, () => {
      aref.synchronized { Option(aref getAndSet null) foreach { _.interrupt() } }
      p.tryComplete(Failure(new CancellationException))          // Akka 2.0
//      p.tryFailure(new CancellationException("Future canceled!"))               // Scala 2.10
    })
  }
}