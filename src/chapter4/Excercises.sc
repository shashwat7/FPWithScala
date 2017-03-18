import chapter4._

def mean(xs: Seq[Double]): Option[Double] = {
  if(xs.isEmpty) None
  else Some(xs.sum / xs.length)
}

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap{m => mean(xs.map{x => math.pow(x-m, 2)})}

val s = Seq[Double](1,2,3,4,5,6,7,8,9,10)
mean(s)
variance(s)

val l = List(Some(1), Some(2), Some(3))
Option.sequence(l)
Option.sequence_1(l)
Option.sequence_2(l)
Option.sequence_3(l)

//val hotels: Map[Long, Map[String, Int]] =
//  Map(1L -> Map("aa" -> 2),
//    2L -> Map("aa" -> 1, "bb" -> 1)
//  )
//
//hotels.map{case (hotelId, wordMap) => (hotelId, score(List("aa"), wordMap))}
//    .toList
//  .sortWith{case (h1, h2) =>
//    if(h2._2 > h1._2) true
//    else if(h2._2 < h1._2) false
//    else h2._1 < h1._1
//  }
//
//def score(words: List[String], hotelReview: Map[String, Int]) = {
//  words.foldLeft[Int](0){case (sc, w) => sc + hotelReview.getOrElse(w, 0) }
//}