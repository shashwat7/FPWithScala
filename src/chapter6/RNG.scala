package chapter6

/**
  * Created by srastogi on 19-Mar-17.
  */

trait RNG{
  def nextInt: (Int, RNG)
  def nonNegativeInt: (Int, RNG)
}

case class SimpleRng(seed: Long) extends RNG {

  def nextInt: (Int, SimpleRng) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRng = SimpleRng(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRng)
  }

  def nonNegativeInt: (Int, SimpleRng) = {
    val (n, nextRng) = nextInt
    (Math.abs(n),nextRng)
  }

}

object SimpleRng{
  type Rand[+A] = RNG => (A, RNG)
//  type Rand[+A] = State[RNG, A]

  def int: Rand[Int] = {rng => rng.nextInt}
  def nonNegativeInt: Rand[Int] = {rng => rng.nonNegativeInt}
  def unit[A](a: A): Rand[A] = { rng => (a, rng) }
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a,rng2) = s(rng)
    (f(a), rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRng) = int(rng)
    (n.toDouble / Integer.MAX_VALUE.toDouble, nextRng)
  }

  // 6.3
  def intDouble(rng: SimpleRng): ((Int,Double), RNG) = {
    val (i,rng2) = int(rng)
    val (d,rng3) = double(rng2)
    ((i,d),rng3)
  }
  def doubleInt(rng: SimpleRng): ((Double,Int), RNG) = {
    val (d,rng2) = double(rng)
    val (i, rng3) = int(rng2)
    ((d,i),rng3)
  }
  def double3(rng: SimpleRng): ((Double,Double,Double), RNG) = {
    val (d1,rng2) = double(rng)
    val (d2,rng3) = double(rng2)
    val (d3,rng4) = double(rng3)
    ((d1,d2,d3), rng4)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count == 0) (List.empty[Int],rng)
    else{
      val (intermediateV, intermediateR) = rng.nextInt
      val (listV, endR) = ints(count-1)(intermediateR)
      (intermediateV :: listV, endR)
    }
  }

  // 6.5
  def double_2(rng: RNG): Rand[Double] = map(int)(i => i.toDouble / Integer.MAX_VALUE)

  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a,b), rng3)
  }
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra,rb)((_,_))
  def randIntDouble: Rand[(Int, Double)] = both(int, double)
  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  //6.7
  def sequence[A](list: List[Rand[A]]): Rand[List[A]] = {
    list match{
      case Nil => rnd => (List.empty[A], rnd)
      case (h :: t) => map2(h, sequence(t))(_ :: _)
    }
    //    list.foldLeft[Rand[List[A]]](unit(List.empty[A])){case (acc,l) => map2(l,acc)(_ :: _)}
  }
  def ints_2(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  // 6.8 - flatMap allows us to generate a random A with Rand[A],and
  // then take that A and choose a Rand[B] based on its value.
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val(a, rng2) = f(rng)
      g(a)(rng2)
    }
  }
  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap[Int, Int](rng => rng.nonNegativeInt){a =>
    val mod = a % n
    if(a + (n+1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  }

  // 6.9
  def map_2[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s){a => unit(f(a))}
  def map2_2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra){a => flatMap(rb){b => unit(f(a,b))}}

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}
