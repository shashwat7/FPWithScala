package chapter4

/**
  * Created by srastogi on 18-Mar-17.
  */
trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match{
      case Right(a) => Right(f(a))
      case _ => _
    }
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match{
      case Right(a) => f(a)
      case _ => _
    }
  }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match{
      case Left(e) => b
      case _ => _
    }
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this match {
      case Right(a) => b.map{bv => f(a,bv)}
      case _ => _
    }
  }
}

case class Right[A](value: A) extends Either[Nothing, A]
case class Left[E](value: E) extends Either[E, Nothing]

object Either{
  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch{ case e: Exception => Left(Exception) }
  }

  // 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es match{
      case Nil => Right(Nil)
      case h :: t => h.flatMap{ hv => sequence(t).map{tv => hv :: tv}}
    }
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
//    sequence(as.map(f))
    as.foldLeft[Either[E, List[B]]](Right(Nil)){case (agg, curr) => f(curr).flatMap{ c => agg.map{ a=> a :+ c }}}
  }

}