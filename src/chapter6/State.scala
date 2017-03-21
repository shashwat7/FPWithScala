package chapter6

/**
  * Created by srastogi on 20-Mar-17.
  */
case class State[S,+A](run: S => (A,S)){

  def get: State[S, S] = State(s => (s, s))
  def set(s: S): State[S, Unit] = State(_ => ((), s))
  def modify(f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def map[B](f: A => B): State[S,B] = State{s: S =>
    val (a,state2) = run(s)
    (f(a), state2)
  }
  def map2[B,C](sb: State[S,B])(f: (A,B) => C): State[S,C] = State{
    s: S => {
      val (a1,s1) = run(s)
      val (b1,s2) = sb.run(s1)
      (f(a1,b1), s2)
    }
  }
  def flatMap[B](g: A => State[S,B]): State[S,B] = {
    val (a,_) = run(_)._1
    g(a)
  }
}

object State{
  def unit[S,A](a: A): State[S,A] = State{s: S => (a: A, s: S)}

  def map2[S,A,B,C](sa: State[S,A], sb: State[S,B])(f: (A,B) => C): State[S,C] = State{
    s: S => {
      val (a1,s1) = sa.run(s)
      val (b1,s2) = sb.run(s1)
      (f(a1,b1), s2)
    }
  }

  def sequence[S,A](list: List[State[S,A]]): State[S, List[A]] = {
    list.foldLeft[State[S, List[A]]](unit(List.empty[A])){case (acc,s) => map2(s,acc)(_ :: _)}
  }

}
