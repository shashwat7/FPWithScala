package Questions

import scala.collection.mutable

/**
  * Created by srastogi on 27-Mar-17.
  */
object FindLargestPalindromicSubstring {

  val isPalidromeMap: mutable.Map[String, Boolean] = mutable.Map.empty[String, Boolean]

  def main(args: Array[String]): Unit = {
    val str = args(0)
    println("String Received: " + str)
    // Brute Force
    println("Brute Force: " + bruteForceWithSave(str))


  }

  def bruteForceWithSave(str: String): String = {
    if(str.length <= 1) {
      isPalidromeMap.put(str, true)
      str
    }
    else if(isPalindrome(str)) {
      isPalidromeMap.put(str, true)
      str
    } else {
      isPalidromeMap.put(str, false)
      val len = str.length
      val leftSubstring = str.substring(0, len-1)
      val rightSubstring = str.substring(1, len)
      val left = isPalidromeMap.get(leftSubstring) match{
        case Some(isPalindrome) if isPalindrome == true => leftSubstring
        case _ => bruteForceWithSave(leftSubstring)
      }
      val right = isPalidromeMap.get(rightSubstring) match{
        case Some(isPalindrome) if isPalindrome == true => rightSubstring
        case _ => bruteForceWithSave(rightSubstring)
      }
      if(left.length > right.length) left else right
    }
  }

  def isPalindrome(str: String): Boolean = str.reverse.equals(str)

}
