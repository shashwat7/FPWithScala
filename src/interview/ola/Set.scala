package interview.ola

/**
  * Created by srastogi on 01-Apr-17.
  */
sealed trait Set{
  val strikeBonus = 10
  val spareBonus = 5

  def getScore: Int
  def message: String
}

case class OrdinarySet(first: Int, second: Int) extends Set{
  assert(first+second < 10)
  def getScore: Int = first + second
  def message = "You can always try harder in the next round!"
}

// first = 10, second = 0
case class StrikeSet(first: Int = 10, second: Int = 0) extends Set{
  assert(first == 10 && second == 0)
  def getScore: Int = 10 + strikeBonus
  def message = "Congratulations! You aim like an eagle!"
}

// first + second = 10
case class SpareSet(first: Int, second: Int) extends Set{
  assert(first + second == 10)
  def getScore: Int = 10 + spareBonus
  def message = "Yeeeaaahh! You did it!"
}

case class FinalSet(first: Int, second: Int, third: Int) extends Set{

  def getScore: Int = {
    if(first == 10) 10 + strikeBonus + second + third
    else if(first + second == 10) 10 + spareBonus + third
    else if(first + second < 10) first + second
    else first + second + third
  }

  override def message: String = "Yohooo! You survived till the last!"
}

