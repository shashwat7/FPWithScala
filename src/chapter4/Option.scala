package chapter4

/**
  * Created by srastogi on 14-Mar-17.
  */
sealed trait Option[+A]{
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B // B>:A means that B must be supertype of A
  def orElse[B >: A](ob: => Option[B]): Option[B] // : => is some concept of non-strictness
  def filter(f: A => Boolean): Option[A]
}
case class Some[+A](get: A) extends Option[A]{
  override def map[B](f: (A) => B): Option[B] = Some(f(get))

  override def filter(f: (A) => Boolean): Option[A] = if(f(get)) this else None

  override def flatMap[B](f: (A) => Option[B]): Option[B] = f(get)

  override def getOrElse[B >: A](default: => B): B = get

  override def orElse[B >: A](ob: => Option[B]): Option[B] = this
}

case object None extends Option[Nothing]{

  override def filter(f: (Nothing) => Boolean): Option[Nothing] = None

  override def flatMap[B](f: (Nothing) => Option[B]): Option[B] = None

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def map[B](f: (Nothing) => B): Option[B] = None

  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob

}

object Option{
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  // 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(av => b.map{bv => f(av,bv)})
  }

  def Try[A](a: => A) = {
    try Some(a)
    catch{
      case e: Exception => None
    }
  }

  // 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match{
      case Nil => Some(Nil)
      case h :: t => h.flatMap{hv => sequence(t).map{tv => hv :: tv}}
    }
  }

  def sequence_2[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldLeft[Option[List[A]]](Some(Nil)){ case (a,b) => b.flatMap{bv => a.map{av => av :+ bv}}}
  }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil)){ case (a,b) => a.flatMap{av => b.map{bv => av :: bv}}}
  }

  // 4.5
//  def traverse[A, B](a: List[A])(fS: A => Option[B]): Option[List[B]] = {
//    a match{
//      case Nil => Some(Nil)
//      case h :: t => f(h).flatMap{v => traverse(t)(f).map{r => v :: r}}
//    }
//  }
//  def sequence_3[A](a: List[Option[A]]): Option[List[A]] = {
////    traverse(a){a => a}
//  }




}
