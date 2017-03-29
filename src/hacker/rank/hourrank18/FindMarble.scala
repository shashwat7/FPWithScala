package hacker.rank.hourrank18

/**
  * Created by shashwat on 28/3/17.
  * https://www.hackerrank.com/contests/hourrank-18/challenges/wheres-the-marble
  */
object FindMarble {
  def main(args: Array[String]) {
    val sc = new java.util.Scanner (System.in)
    var m = sc.nextInt()
    var n = sc.nextInt()
    var a0: Int = 0
    while(a0 < n){
      var a = sc.nextInt()
      var b = sc.nextInt()
      // your code goes here
      if(a == m) m = b
      else if(b==m) m = a
      a0+=1
    }
    println(m)
  }
}
