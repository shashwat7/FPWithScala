package hacker.rank.hourrank17

/**
  * Created by srastogi on 31-Mar-17.
  */
object OrganizingContainerOfBalls {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner (System.in);
    var q = sc.nextInt()
    var a0: Int = 0
    while(a0 < q){
      var n = sc.nextInt()
      var M = Array.ofDim[Int](n,n)
      for(c <- 0 to n-1) {
        for(t <- 0 to n-1){
          M(c)(t) = sc.nextInt();
        }
      }
      // your code goes here
      if(checkPossible(M)) println("Possible") else println("Impossible")
      a0+=1;
    }
  }

  def checkPossible(M: Array[Array[Int]]): Boolean = {
    val numberOfContainers = M.length
    // Initialize state of the containers
    val state: Array[Boolean] = Array.fill[Boolean](numberOfContainers)(false) //save state of each container
    (0 until numberOfContainers).foreach{c => if(isContainerSatisfied(M(c))) state(c) = true }
    if(M.length == 1 && isContainerSatisfied(M(0))) true
    else{
      val totalBalls = totalBallsOfEachType(M)
      (0 until numberOfContainers).foldLeft(true){case (ans, cont) => ans && canContainerBeSatisfied(M(cont),totalBalls)}
    }
  }

  // true if the container have all balls of same type
  def isContainerSatisfied(c: Array[Int]): Boolean = {
    c.filter(_ != 0).size > 1
  }

  // number of swaps to remove all balls of same type
  def swapsToRemove(c: Array[Int], t: Int): Int = c(t)

  // total balls of each type
  def totalBallsOfEachType(M: Array[Array[Int]]): Array[Int] = {
    val count: Array[Int] = Array.fill[Int](M.length)(0)
    (0 until M.length).foreach{c =>
      val container = M(c)
      (0 until container.length).foreach{ t =>
        val noOfBallsOfTypeT = container(t)
        count(t) = count(t) + noOfBallsOfTypeT
      }
    }
    count
  }

  def canContainerBeSatisfied(c: Array[Int], totalBallsOfEachType: Array[Int]): Boolean = {
    (0 until totalBallsOfEachType.length).foldLeft(false){case (ans, t) =>
      val tTypeBallInMarket = totalBallsOfEachType(t)
      val tTypeBallInContainer = c(t)
      val swapsRequiredToBringAllBall = tTypeBallInMarket-tTypeBallInMarket
      val totalBallOfOtherTypeInContainer = c.foldLeft(0){_ + _} - tTypeBallInContainer
      ans || totalBallOfOtherTypeInContainer == swapsRequiredToBringAllBall
    }
  }

}

