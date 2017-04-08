
def widestGap(n: Int, start: Array[Int], end: Array[Int]): Int = {
  val freeSpots = Array.fill[Boolean](n)(true)
  start.zip(end).foreach{
    case (s,e) => for{
      i <- s-1 to e-1
    } yield freeSpots(i) = false
  }
  var max = 0
  freeSpots.foldLeft(0){case (gapSoFar, curr) =>
    if(curr) {
      if(max < gapSoFar + 1) max = gapSoFar + 1
      gapSoFar + 1
    } else 0
  }
  max
}
