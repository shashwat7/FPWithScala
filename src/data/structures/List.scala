package data.structures

/**
  * Created by srastogi on 22-Mar-17.
  */

//sealed trait List[+A]
//case object Nil extends List[Nothing]
//case class Cons[+A](data: A, next: List[A]) extends List[A]

/*
* start => elements are inserted at start position
* end => elements are extracted from end position
* count => number of elements present in the queue at all times
* */
case class Queue[A](start: List[A], end: List[A], count: Int) {

//  // Read first element
//  def read(n: Int): Option[A] = this.end match{
//    case Nil => None
//    case Cons(x,xs) => Some(x)
//  }
//
//  def toList: scala.collection.immutable.List[A] = this.end match{
//    case Nil => scala.collection.immutable.List.empty[A]
//    case Cons(x,xs) => x :: Queue.dequeue(this)._1.toList
//  }

}

//object Queue{
//
//  // Type alias to carry forward Queue state after each operation
//  type QueueState[A] = Queue[A] => (Queue[A], Int)
//
//  // Insert element into the queue
//  def enqueue[A](elem: A): QueueState[A] = q => q.start match {
//    case Nil =>
//      val newNode = Cons(elem, Nil)
//      (Queue(newNode, newNode, 1), 1)
//    case _ => val newNode = Cons(elem, q.start)
//      (Queue(newNode, q.end, q.count + 1), q.count + 1)
//  }
//
//  // Take element from the queue
//  def dequeue[A]: QueueState[A] = q => q.end match {
//    case Nil => (q, q.count)
//    case Cons(x,xs) =>
//      if(q.end == q.start) (Queue(Nil, Nil, 0), 0)
//      else (Queue(q.start, xs, q.count - 1), q.count - 1)
//  }
//
//  def apply[A](elem: A*): Queue[A] = {
//    val q = new Queue[A](Nil, Nil, 0)
//    (for{
//      e <- elem
//      (q,count) = enqueue(e)(q)
//    } yield q).last
//  }
//}

