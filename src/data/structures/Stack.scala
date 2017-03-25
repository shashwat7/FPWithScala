package data.structures

/**
  * Created by srastogi on 22-Mar-17.
  * To track number of elements in Stack
  */

sealed trait Stack[+A]{

  def getTopElement: Option[A] = {
    this match{
      case Nil => None
      case Cons(top, remaining) => Some(top)
    }
  }

  def push[B>:A](elem: B): Stack[B] = Cons(elem, this)

  def pop: (Option[A], Stack[A]) = {
    this match{
      case Cons(top, remaining) => (Some(top), remaining)
      case Nil => (None, Nil)
    }
  }

  def toList: List[A] = {
    this match{
      case Nil => List.empty[A]
      case Cons(top, remaining) => top :: remaining.toList
    }
  }
}

case object Nil extends Stack[Nothing]

case class Cons[+A](top: A, nextElem: Stack[A]) extends Stack[A]

object Stack{

  def apply[A](as: A*): Stack[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}

