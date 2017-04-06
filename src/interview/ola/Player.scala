package interview.ola

/**
  * Created by srastogi on 01-Apr-17.
  */
case class Player(private val name: String,
                  private var score: Int = 0,
                  private var sets: List[Set] = List.empty[Set]
                 ){

  def getScore: Int = score
  def getName: String = name
  def getSets: List[Set] = sets

  def setScore(s: Int) = score = s
  def incrementScore(s: Int) = score = score + s
  def addSet(s: Set) = sets = sets :+ s
}
