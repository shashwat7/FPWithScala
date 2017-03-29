package hacker.rank.hourrank18

import scala.collection.mutable

/**
  * Created by shashwat on 28/3/17.
  * https://www.hackerrank.com/contests/hourrank-18/challenges/super-six-substrings
  */
object SuperSubstring {

  def main(args: Array[String]): Unit = {
    val sc = new java.util.Scanner (System.in)
    var s = sc.next()
    def isSuper(s: String): Boolean = s == "0" || (!s.startsWith("0") && isDivisibleBy6(s))
    def isDivisibleBy6(str: String) = isDivisibleBy2(str) && isDivisibleBy3(str)
    def isDivisibleBy2(str: String) = str.last.toInt % 2 == 0
    def isDivisibleBy3(str: String) = str.map(_.toInt).sum % 3 == 0
    val savedResult = scala.collection.mutable.Map.empty[String, Int]
    def slideCount(s: String): Int = {
      if(savedResult.contains(s)) savedResult(s)
      else{
        if(s.length == 0) 0
        else if(s.length == 1)
          if(s == "0" || s == "6") {savedResult.put(s,1); 1}
          else {savedResult.put(s,0); 0}
        else{
          val left = s.substring(0, s.length -1)
          val right = s.substring(1,s.length)
          val mid = s.substring(1, s.length-1 )
          val lCount = slideCount(left)
          savedResult.put(left,lCount)
          val rCount = slideCount(right)
          savedResult.put(right,rCount)
          val mCount = slideCount(mid)
          savedResult.put(mid,mCount)
          val res = lCount + rCount - mCount + (if(isSuper(s)) 1 else 0)
          savedResult.put(s,res)
          res
        }
      }
    }
    val answer = slideCount(s)
    println(answer)
  }

  def editorialSolution(s: String): Int = {

    def numberOfEvenSubstringsStartingAtIAndMod3M(i: Int, m: Int): Int = {
      if(i == s.length) 0
      else {
        val c = s(i)
        numberOfEvenSubstringsStartingAtIAndMod3M(i+1, m+c%3)
      }
    }
  }



}
