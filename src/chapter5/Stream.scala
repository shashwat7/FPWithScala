package chapter5

import scala.annotation.tailrec

/**
  * Created by srastogi on 18-Mar-17.
  */
sealed trait Stream[+A]{
  def headOption(): Option[A] = {
    this match {
      case Empty => None
      case Cons(h, t) => Some(h()) // h() forces execution of h. t is not evaluated at all.
    }
  }

  def toList: List[A] = { // Returns a list by evaluating the entire stream.
    this match{
      case Empty => Nil
      case Cons(h,t) => h() :: t().toList
    }
  }

  def take(n: Int): Stream[A] = {
    if(n == 1){
      this match{
        case Empty => Empty
        case Cons(h,t) => Cons(h, () => Stream.empty[A])
      }
    }
    else this match{
      case Empty => Empty
      case Cons(h,t) => Cons( h, () => t().take(n-1) )
    }
  }

  def drop(n : Int): Stream[A] = {
    if(n > 0) this match {
      case Empty => Empty
      case Cons(h,t) => t().drop(n-1)
    } else this
  }

  def takeWhile(f: A => Boolean): Stream[A] = {
    this match{
      case Empty => Empty
      case Cons(h,t) => if(f(h())) Cons(h, () => t().takeWhile(f)) else Stream.empty[A]
    }
  }

  def exists(f: A => Boolean): Boolean = {
    this match{
      case Cons(h, t) => f(h()) || t().exists(f)
      case _ => false
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {     // second argument of f will not be evaluated until called far
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def exists_2(f: A => Boolean): Boolean = {
    this.foldRight(false){(a,b) => f(a) || b}
  }

  // 5.4
  def forAll(f: A => Boolean): Boolean = {
    this.foldRight(true){(a,b) => f(a) && b}
  }

  // 5.5
  def takeWhile_2(f: A => Boolean): Stream[A] = {
    this.foldRight[Stream[A]](Stream.empty[A]){(a,b) => if(f(a)) Stream.cons(a, b) else b }
  }

  // 5.6
  def headOption_2(): Option[A] = {
    this.foldRight[Option[A]](None: Option[A]){(a,b) => Some(a)}
  }

  // 5.7
  def map[B](f: A => B): Stream[B] = {
    this.foldRight[Stream[B]](Stream.empty[B]){(a,b) => Stream.cons(f(a), b)}
  }
  def filter(f: A => Boolean): Stream[A] = {
    this.foldRight[Stream[A]](Stream.empty[A]){(a,b) => if(f(a)) Stream.cons(a,b) else b}
  }
  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    this.foldRight[Stream[B]](Stream.empty[B]){(a,b) => Stream.appendAnotherStream(f(a),b)}
  }

  //5.13
  def map_2[B](f: A => B): Stream[B] = {
    Stream.unfold(this){
      case Empty => None
      case Cons(h,t) => Some(f(h()), t())
    }
  }
  def take_2(n: Int): Stream[A] = {
    Stream.unfold(this, n){case (stream, n) => stream match{
      case Empty => None
      case Cons(h,t) => if(n==0) None else Some(h(), (t(),n-1))
    }}
  }
  def takeWhile_3(f: A => Boolean): Stream[A] = {
    Stream.unfold[A,Stream[A]](this){
      case Empty => None
      case Cons(h,t) => if(f(h())) Some(h(), t()) else None
    }
  }
  def zipWith[B,C](s2: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] = {
    Stream.unfold[C, (Stream[A], Stream[B])]((this,s2)){ case (s1,s2) =>
      (s1,s2) match{
        case (Empty, Empty) => None
        case (Cons(h1,t1), Empty) => Some( f(Some(h1()), None), (t1(),Stream.empty[B]) )
        case (Empty, Cons(h2,t2)) => Some( f(None, Some(h2())), (Stream.empty[A],t2()) )
        case (Cons(h1,t1), Cons(h2,t2)) => Some( f(Some(h1()), Some(h2())), (t1(),t2()) )
      }
    }
  }
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    Stream.unfold[(Option[A], Option[B]), (Stream[A], Stream[B])]((this, s2)){ case (s1,s2) =>
      (s1,s2) match{
        case (Empty, Empty) => None
        case (Cons(h1,t1), Empty) => Some( (Some(h1()), None), (t1(),Stream.empty[B]) )
        case (Empty, Cons(h2,t2)) => Some( (None, Some(h2())), (Stream.empty[A],t2()) )
        case (Cons(h1,t1), Cons(h2,t2)) => Some( (Some(h1()), Some(h2())), (t1(),t2()) )
      }
    }
  }

  // 5.15
  def tails: Stream[Stream[A]] = {
    Stream.append(
      Stream.unfold[Stream[A], Stream[A]](this){
        case Cons(h,t) => Some(Cons(h,t),t())
        case Empty => None
      }, Stream.empty[A])
  }

  // 5.16
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = ???


}

case class Cons[+A](h: () => A, t: ()=> Stream[A]) extends Stream[A]
case object Empty extends Stream[Nothing]

object Stream{
  // Following is a smart constructor
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h // Lazy will cache the value of head and tail. Memoizing.
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty // Annotates Empty as Stream[A]. This is better for type inference.

  // 5.7
  def append[A](s: Stream[A], elem: A) = {
    s.foldRight[Stream[A]](Stream(elem)){(a,b) => Stream.cons(a, b)}
  }
  def appendAnotherStream[A](as: Stream[A],xs: Stream[A]): Stream[A] = {
    xs match{
      case Empty => as
      case Cons(h,t) => Stream.appendAnotherStream(Stream.append(as, h()), t())
    }
  }

  // 5.8
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  // 5.9
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))
  // 5.10
  def fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      Stream.cons(f0, go(f1, f0+f1))
    go(0, 1)
  }
  //5.11
  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = {
    f(z) match {
      case None => Stream.empty[A]
      case Some((a,s)) => Stream.cons(a, unfold(s)(f))
    }
  }
  // 5.12
  def constant_2[A](a: A): Stream[A] = Stream.unfold[A, A](a){a => Some(a,a)}
  def from_2(n: Int): Stream[Int] = Stream.unfold[Int, Int](n){n => Some(n, n+1)}
  def fibs_2: Stream[Int] = Stream.unfold((0,1)){case (a,b) =>
    Some((a, (b, a+b)))
  }

  //5.14
  def startsWith[A](s1: Stream[A], s2: Stream[A]): Boolean = {
    s1.zipAll(s2).takeWhile{case (h1,h2) => h2.isDefined}
      .forAll{case (h1,h2) => h1 == h2}
  }

  def apply[A](as: A*): Stream[A] = {
    if(as.isEmpty) empty[A]
    else cons(as.head, apply(as.tail: _*))
  }
}
