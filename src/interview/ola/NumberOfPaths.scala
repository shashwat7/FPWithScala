package interview.ola

/**
  * Created by srastogi on 01-Apr-17.
  */
object NumberOfPaths {


  def isAccessible(M: Array[Array[Int]], i: Int, j: Int): Boolean = {
    if(i >=0 && i < M.length){
      if(j >=0 && j < M(0).length){
        M(i)(j) == 0
      } else false
    } else false
  }

  def numberOfWays(M: Array[Array[Int]], i: Int, j: Int, endPos: (Int,Int)): Int = {
    if(!isAccessible(M,i,j)) 0
    else if(i>endPos._1 || j>endPos._2) 0
    else if(i == endPos._1 && j == endPos._2) 1
    else {
      numberOfWays(M,i+1,j,endPos) +
        numberOfWays(M,i,j+1,endPos) +
        numberOfWays(M,i+1, j+1,endPos)
    }
  }

  def main(args: Array[String]): Unit = {
    val M1: Array[Array[Int]] = Array(
      Array(0,0,0,0,0),
      Array(0,0,0,0,0),
      Array(0,0,1,0,0),
      Array(0,0,0,0,0)
    )

    val M2: Array[Array[Int]] = Array(
      Array(0,0),
      Array(0,0)
    )

    println(numberOfWays(M1,0,0,(3,4)))
    println(numberOfWays(M2,0,0,(1,1)))
    println(numberOfWays(M2,0,1,(1,1)))
    println(numberOfWays(M2,1,0,(1,1)))
    println(numberOfWays(M2,1,0,(0,0)))
  }

}
