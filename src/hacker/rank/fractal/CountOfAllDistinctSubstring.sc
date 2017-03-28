

def substringCalculator(st: String): Long = {
  val setOfUniqueStrings = scala.collection.mutable.Set.empty[String]
  def allSubstring(str: String): Unit = {
    if(setOfUniqueStrings.contains(str)) return
    else{
      val length = str.length
      if(length <= 0) return
      else{
        val leftSubstr = str.substring(0, length-1)
        val rightSubstr = str.substring(1, length)
        allSubstring(leftSubstr)
        setOfUniqueStrings.add(leftSubstr)
        allSubstring(rightSubstr)
        setOfUniqueStrings.add(rightSubstr)
      }
    }
  }
  allSubstring(st)
  setOfUniqueStrings.size
}

substringCalculator("shashwat")
