val dictionary = List("a", "an","and", "at","bat", "bath", "be","bed","beyond","ed","hand","on","than","yo")

def isWord(word: String): Boolean = {
  if(word.length < 1) true
  else{
//    println("Check: " + word + ", length: " + word.length)

    val left = 0
    val right = word.length
    var mid = 1
    var found: Boolean = false

    while(mid <= right && !found) {
//      println("Substring Check: " + word.substring(left, mid))
      if(
        dictionary.contains(word.substring(left, mid)) &&
          isWord(word.substring(mid, right))
      ) {
        found = true
      }
      else mid = mid + 1
    }

    found
  }
}
isWord("bedbathandbeyond")