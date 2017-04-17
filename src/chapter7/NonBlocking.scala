package chapter7

/**
  * Created by shashwat on 26/3/17.
  */
trait Future[A] {
  private [chapter7] def apply(k: A => Unit)
}
