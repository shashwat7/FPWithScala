package chapter3

import scala.annotation.tailrec

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

  def appendAnElement[A](a1: List[A], a2: A): List[A] = {
    a1 match{
      case Nil => Cons(a2, Nil)
      case Cons(h, t) => Cons(h, appendAnElement(t, a2))
    }
  }

  //TODO Returns all but the last element of the list
  //  def init[A](l: List[A]): List[A] = {}

  def foldRight[A,B](l: List[A], z: B)(f:(A,B) => B): B = {
    l match{
      case Nil => z
      case Cons(x,xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def length[A](l: List[A]): Int = foldRight(l, 0){ case (elem, acc) => acc + 1}

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = {
    as match{
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def sum3(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
  def product3(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)
  def length2[A](as: List[A]): Int = foldLeft(as, 0){ case (l, a) => l + 1 }

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A]){
    case (rev, x) => setHead(rev, x)
  }

  // 3.13
  def foldLeftAsFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as, z){case (a, res) => f(res, a)}
  def foldRightAsFoldLeft[A, B](as: List[A], z: B)(f: (A,B) => B): B = foldLeft(as, z){case (res, a) => f(a, res)}

  // 3.14
  def appendUsingFold[A](l1: List[A], l2: List[A]): List[A] = foldLeft(l2, l1){
    case (res, elem1) => appendAnElement(res, elem1)
  }

  // 3.15
  def flatten[A](lists: List[List[A]]): List[A] = {
    foldLeft(lists, Nil: List[A]){case (res, list) => append(res, list) }
  }

  // 3.16
  def addOne(l: List[Int]): List[Int] = {
    l match{
      case Nil => Nil
      case Cons(x, xs) => Cons(x+1, addOne(xs))
    }
  }

  // 3.17
  def convertDoubleListToString(l: List[Double]): List[String] = {
    l match{
      case Nil => Nil: List[String]
      case Cons(x, xs) => Cons(x.toString, convertDoubleListToString(xs))
    }
  }

  // 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = {
    as match{
      case Nil => Nil: List[B]
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }
  }

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    @tailrec
    def go(as: List[A], acc: List[A]): List[A] = {
      as match{
        case Nil => acc
        case Cons(x, xs) => if(f(x)) go(xs, appendAnElement(acc, x)) else go(xs, acc)
      }
    }
    go(as, Nil)
  }

  // 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  // 3.21
  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as){ elem => if(f(elem)) List(elem) else Nil }

  // 3.22
  def addAllElements(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1,l2) match{
      case (Nil, Nil) => Nil
      case (Nil, Cons(y,ys)) => Cons(y, addAllElements(Nil, ys))
      case (Cons(x,xs), Nil) => Cons(x, addAllElements(xs, Nil))
      case (Cons(x,xs), Cons(y,ys)) => Cons(x + y, addAllElements(xs, ys))
    }
  }

  // 3.23
  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (Option[A],Option[B]) => C): List[C] = {
    (l1,l2) match{
      case (Nil, Nil) => Nil
      case (Nil, Cons(y,ys)) => Cons(f(None,Some(y)), zipWith(Nil, ys)(f))
      case (Cons(x,xs), Nil) => Cons(f(Some(x),None), zipWith(xs, Nil)(f))
      case (Cons(x,xs), Cons(y,ys)) => Cons(f(Some(x),Some(y)), zipWith(xs, ys)(f))
    }
  }

  // 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    def go(sup: List[A], sub: List[A]): Boolean = {
      if(findSub(sup, sub)) true
      else sup match{
        case Nil => false
        case Cons(x,xs) => findSub(xs, sub)
      }
    }

    // Iterate only for that many times till we have
    @tailrec
    def findSub(sup: List[A], sub: List[A]): Boolean = {
      sub match{
        case Cons(y, ys) => sup match{
          case Nil => false
          case Cons(x,xs) => x == y && findSub(xs, ys)
        }
        case Nil => true
      }
    }
    go(sup, sub)
  }

  /*
  * A* denotes a Seq of A, function having variable arguments are called Variadic function
  * */
  def apply[A](as: A*): List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}
