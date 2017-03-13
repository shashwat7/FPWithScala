package Questions

/**
  * Created by shashwat on 12/3/17.
  * 1 -> A, 2 -> B, ... , 26 -> Z
  * 123 -> can be any of the three (ABC ; LC ; AW)
  * Find all such combinations.
  */
object NumberToString {
  def main(args: Array[String]): Unit = {
    printStringFromInt(12.toString, "")
//    printStringFromInt(2345.toString, "")
  }

  private def convertToChar(n: Int): String = {
    if(n < 1 || n > 26) ""
    else (n + 64).toChar.toString
  }

  def printStringFromInt(n: String, acc: String): Unit = {
    if(n.length < 1) print(acc)
    else if(n.length == 1) print(acc + convertToChar(n.toInt))
    else {
      val firstPart_v1 = n.substring(0,2).toInt
      val stringPart_v1 = convertToChar(firstPart_v1)
      if(stringPart_v1 != "") printStringFromInt(n.substring(2, n.length), acc + stringPart_v1)
      println()
      val firstPart_v2 = n.substring(0,1).toInt
      printStringFromInt(n.substring(1, n.length), acc + convertToChar(firstPart_v2))
      println()
    }
  }
}
