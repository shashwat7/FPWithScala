package chapter7

/**
  * Created by srastogi on 24-Mar-17.
  */
trait Par[A] {

  def get: A

}

// Should computation be eager or lazy => i.e. should it be responsibility of fork or of get?
object Par{

  // Creates a computation that immediately results in value a.
  def unit[A](a: A): Par[A] = ???

  // This unit doesn't start executing it immediately. Wraps the expression a for concurrent evaluation by run.
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // This is where the parallel computation takes place.
  def run[A](a: Par[A]): A = ???

  // Combines result of two parallel computation.
  def map2[A,B,C](p1: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] = {
    val firstExecution = p1.get
    val secondExecution = p2.get
    unit(f(firstExecution, secondExecution))
  }

  // Marks a computation for concurrent evaluation by run.
  def fork[A](a: => Par[A]): Par[A] = ???
}