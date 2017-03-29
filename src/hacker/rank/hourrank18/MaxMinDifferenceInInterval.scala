package hacker.rank.hourrank18

/**
  * Created by shashwat on 28/3/17.
  */
object MaxMinDifferenceInInterval {
  def main(args: Array[String]) {
    val sc = new java.util.Scanner (System.in);
    var n = sc.nextInt();
    var q = sc.nextInt();
    var arr = new Array[Int](n);
    for(a_i <- 0 to n-1) {
      arr(a_i) = sc.nextInt();
    }

    val maxMinusMin = for{
      a <- 0 to arr.length-1
      b <- a to arr.length - 1
      (max, min) = findMaxMinArray(arr,a,b)
    } yield (max - min)

    var a0 = 0;
    while(a0 < q){
      var low = sc.nextInt();
      var high = sc.nextInt();
      // your code goes here
      println(maxMinusMin.foldLeft(0){case (sum, v) => if(low <= v && high >= v) sum+1 else sum})
      a0+=1;
    }
  }

  def findMaxMinArray(arr: Array[Int], startPos: Int, endPos: Int): (Int, Int) = {
    var max = arr(startPos)
    var min = arr(startPos)
    for(i <- startPos to endPos){
      if(arr(i) > max) max = arr(i)
      if(arr(i) < min) min = arr(i)
    }
    (max,min)
  }

}
