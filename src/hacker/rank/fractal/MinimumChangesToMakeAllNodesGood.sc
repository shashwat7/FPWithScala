def goodNodes(points_to: Array[Int]): Int = {

  val isGoodArray: Array[Boolean] = (
    for{
      i<- 0 to points_to.length-1
      isGood = isGoodNode(i, points_to)
    } yield isGood
    ).toArray

  // Number of steps required to make Node n good
  def makeGood(n: Int, points_to: Array[Int]): Int = {
    if(isGoodArray(n-1)) 0
    else {
      isGoodArray(n-1) = true
      1
    }
  }

  // n is a good node?
  def isGoodNode(n: Int, points_to: Array[Int]): Boolean = {
    if(n == 0 || points_to(n) == 1 ) true
    else if(points_to(n)-1 == n) false
    else {
      isGoodNode(points_to(n)-1, points_to)
    }
  }

  points_to.foldLeft(0){case (sum, n) => sum + makeGood(n-1, points_to)}

}

goodNodes(Array(1,2,3,4,5))