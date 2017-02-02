package chapter3

/**
  * Created by shashwat on 2/2/17.
  */

sealed trait List[+A] // + indicates the the type parameter A is co-variant

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{                              // Called companion object
  def sum(ints: List[Int]): Int = {
    ints match{
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def product(ds: List[Double]): Double = {
    ds match {
      case Nil => 0.0
      case Cons(0.0, _) => 0.0
      case Cons(d, ds) => d * product(ds)
    }
  }

  // Returns all elements of the List except the first
  def tail[A](as: List[A]): List[A] = {
    as match{
      case Cons(b, bs) => bs
      case Nil => Nil // or throw some error
    }
  }

  // Sets a new head to the existing List
  def setHead[A](lst: List[A], newHead: A): List[A] = Cons(newHead, lst)

  // Removes first n elements from the list
  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go(n: Int, remainingList: List[A]): List[A] = {
      if(n == 1) tail(remainingList)
      else go(n-1, tail(remainingList))
    }
    go(n, l)
  }

  // Removes element from List prefix until the condition is met
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match{
      case Nil => Nil
      case Cons(x, xs) => if(f(x)) dropWhile(xs, f) else Cons(x, xs)
    }
  }

  // Appends two lists
  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match{
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }


  /*
  * A* denotes a Seq of A, function having variable arguments are called Variadic function
  * */
  def apply[A](as: A*): List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}
