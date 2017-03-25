// main method in "Solution" will be run as your answer
import scala.collection.mutable
object Solution {

  def score(words: List[String], hotelReview: Map[String, Int]) = {
    words.foldLeft[Int](0){case (sc, w) => sc + hotelReview.getOrElse(w, 0) }
  }

  def main(args: Array[String]): Unit = {
    val words = readLine().split(" ").toList
    println("CheckInputWords: " + words.mkString(" "))
    val numberofReviews = readInt()
    println("CheckNumberOfReviews: " + numberofReviews)
    val hotels = scala.collection.mutable.Map.empty[String, scala.collection.mutable.Map[String, Int]]
    for(i <- 0 until 2 * numberofReviews) {
      val hotelId = readLine()
      println("CheckInputHotelid: " + hotelId)
      val review = readLine()
      println("CheckInputRebiew: " + review)
      val previousReviews = hotels.getOrElse(hotelId, mutable.Map.empty[String, Int])
      hotels.put(hotelId, insertIntoWordMap(review,previousReviews))
    }
    val result = hotels.map{case (hotelId, reviews) => (hotelId, score(words, reviews.toMap))}.toList
    println("Scores: " + result)
    val sortedResult = result.sortWith{(h1,h2) =>
      if(h1._2 > h2._2) true
      else if(h1._2 < h2._2) false
      else h1._1.toInt < h2._1.toInt
    }
    sortedResult.foreach(e => println("Answer: " + e))
    println(sortedResult.map(_._1).mkString(" "))
  }

  def insertIntoWordMap(str: String, wordMap: scala.collection.mutable.Map[String, Int]) = {
    str.split(" ").foreach{
      word =>
        val count = wordMap.getOrElse(word, 0)
        wordMap.put(word, count)
    }
    wordMap
  }

}