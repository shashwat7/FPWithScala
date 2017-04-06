def getMinimumDifference(a: Array[String], b: Array[String]): Array[Int] = {

  def minimumModificationRequired(str1: String, str2: String): Int = {
    if(str1.length != str2.length) -1
    else {
      val mapOfStr1Char = scala.collection.mutable.Map.empty[Char,Int]
      str1.foreach{c => mapOfStr1Char.put(c, mapOfStr1Char.getOrElse(c, 0)+1)}
      str2.foldLeft(0){case (sum, c) => mapOfStr1Char.get(c) match{
        case None => sum + 1
        case Some(i) => if(i > 0) {
          mapOfStr1Char.put(c, mapOfStr1Char.getOrElse(c, 0)-1)
          sum
        } else sum + 1
      }}
    }
  }

  a.zip(b).map{case (str1,str2) => minimumModificationRequired(str1,str2)}

}
