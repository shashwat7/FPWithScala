package interview.ola

import scala.util.Random

/**
  * Created by srastogi on 01-Apr-17.
  */
object BowlingAlley {

  val usage =
    """The application can be run by passing your name as the argument.
      | java BowlingAlley [YourName]
    """.stripMargin
  val seperator = "-------------------------------------------------------"


  def main(args: Array[String]): Unit = {
    //    if(args.length > 1) {
    //      println(usage)
    //      System.exit(1)
    //    }
    //
    //    // Initialize the player
    //    val name = if(args.length == 1) {
    //      println("Hello " + args(0) + ". Welcome to Ola_Bowling_Alley!")
    //      args(0)
    //    } else {
    //      println("Enter your name: ")
    //      readLine()
    //    }
    println("Enter the number of players:")
    val numOfPlayers = readLine().toInt
    val players = (1 to numOfPlayers).map{ i =>
      println("Enter the name of player " + i)
      Player(readLine())
    }

    println("Let the game begin!")
    val sets: List[(Player, Set)] = (1 to 10).foldLeft(List.empty[(Player, Set)]){case (allRounds,round) =>
      println(seperator)
      val playerVsThisSet: List[(Player,Set)] = players.map{
        player =>
          val thisSet = if(round != 10) playRound(round, player)
          else playLastRound(round, player)
          println(thisSet.message)
          player.incrementScore(thisSet.getScore)
          println("Player: "+player.getName +". Your score so far: " + player.getScore)
          (player, thisSet)
      }.toList
      allRounds ::: playerVsThisSet
    }

    println(seperator)
    players.foreach{player =>
      println("Player " + player.getName + ". Score is: " + player.getScore)
    }
    val winner = players.foldLeft(Player("")){case (winner, player) => if(player.getScore > winner.getScore) player else winner}
    println("The winner is : " + winner.getName)
    println("Game Over! We hope you enjoyed! :)")

  }


  /**
    * Returns the number of pins knocked down in a set
    * @param maxPins is the number of available pins
    * @return pins knocked down
    * */
  def rollBall(maxPins: Int): Int = {
    val r = Random.nextInt(maxPins+1)
    println(s"You knocked down $r balls!")
    r
  }

  def playRound(round: Int, player: Player): Set = {
    assert(round >= 1 && round <= 9)
    println(seperator)
    println("Player: " + player.getName +". Round: " + round + " begins!")
    var availablePins = 10
    println("1st attempt!")
    val firstAttempt: Int = rollBall(availablePins)
    availablePins -= firstAttempt
    firstAttempt match{
      case i if i == 10 => new StrikeSet
      case i: Int =>
        println("2nd attempt!")
        rollBall(availablePins) match{
          case second if second == availablePins => SpareSet(i,second)
          case second: Int => OrdinarySet(i, second)
        }
    }
  }

  def playLastRound(round: Int = 10, player: Player): Set = {
    assert(round == 10)
    println(seperator)
    println("Player: " + player.getName +". Round: " + round + " begins!")
    var availablePins = 10
    val attempt = Array.fill[Int](3)(0)
    var maxAllowedAttempt = 2
    var i = 0
    while(availablePins > 0 && i<maxAllowedAttempt){
      val r = rollBall(availablePins)
      attempt(i) = r
      availablePins -= r
      if(availablePins == 0 && i <=1) {
        availablePins = 10
        maxAllowedAttempt = 3
      }
      i = i + 1
    }
    FinalSet(attempt(0),attempt(1),attempt(2))
  }
}
