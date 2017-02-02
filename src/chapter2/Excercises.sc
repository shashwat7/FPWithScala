// Recursive fibonacci but not tail recursion
def fibR(n: Int): Int = {
  def go(n: Int): Int = {
    if(n == 1) 0
    else if(n == 2) 1
    else go(n-1) + go(n-2)
  }
  go(n)
}

// Iterative
def fibI(n : Int): Int = {
  var a = 0
  var b = 1
  var c = 0
  for(i <- 3 to n)
  {
    c = a + b
    a = b
    b = c
  }
  c
}

def fibTR(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, prev: Int, cur: Int): Int = {
    if(n == 1) prev
    else go(n-1, cur, prev+cur)
  }
  go(n, 0, 1)
}

fibR(7)
fibI(7)
fibTR(7)


// To check whether Array is sorted
def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def go(n: Int, res: Boolean): Boolean = {
    if(n >= as.length-1) res
    else go(n+1, ordered(as(n), as(n+1)))
  }
  go(0, true)
}


// Excercise 2.3
def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
  a: A => b: B => f(a, b)
}

// Excercise 2.4
def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
  (a: A,b: B) => f(a)(b)
}

// Excercise 2.5
def compose[A,B,C](f: B => C, g: A => B): A => C = {
  a: A => f(g(a)) // f compose g
55}



