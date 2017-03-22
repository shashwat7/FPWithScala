import scala.collection.mutable

val number = "0000890,987,654,321"
// expected output:
// eight hundred ninety billion
// nine hundred eighty seven million
// six hundred fifty four thousand
// three hundred and twenty one

val singleNumber = Map(
  "0" -> "zero",
  "1" -> "one",
  "2" -> "two",
  "3" -> "three",
  "4" -> "four",
  "5" -> "five",
  "6" -> "six",
  "7" -> "seven",
  "8" -> "eight",
  "9" -> "nine"
)
val tens = Map(
  "10" -> "ten",
  "11" -> "eleven",
  "12" -> "twelve",
  "13" -> "thirteen",
  "14" -> "fourteen",
  "15" -> "fifteen",
  "16" -> "sixteen",
  "17" -> "seventeen",
  "18" -> "eighteen",
  "19" -> "nineteen",
  "20" -> "twenty ",
  "30" -> "thirty ",
  "40" -> "forty ",
  "50" -> "fifty ",
  "60" -> "sixty ",
  "70" -> "seventy ",
  "80" -> "eighty ",
  "90" -> "ninty "
)

def oneDigitNumber(num: String) = {
//  println("Received: " + num)
  assert(num.length == 1)
  singleNumber(num)
}

def twoDigitNumber(n: String): String = {
//  println("Received: " + n)
  assert(n.length == 2)
  if(n.toInt % 10 == 0 || (n.toInt > 9 && n.toInt < 20)) tens(n)
  else twoDigitNumber(((n.toInt / 10) * 10).toString) + oneDigitNumber((n.toInt % 10).toString)
}

def threeDigitNumber(n: String): String = {
//  println("Received: " + n)
  assert(n.length == 3)
  if(n.head != '0') oneDigitNumber(n.head.toString) + " hundred and " + twoDigitNumber(n.substring(1))
  else twoDigitNumber(n.substring(1))
}

def getString(n: String) = {
  assert(n.length <= 3)
  n.length match{
    case 1 => oneDigitNumber(n)
    case 2 => twoDigitNumber(n)
    case 3 => threeDigitNumber(n)
  }
}

// Return iterable of string in pairs of three.
// Lower significant first
def slide(num: String): IndexedSeq[String] = {
  val seq = for{
    i <- num.length to 0 by -3
    piece = if(i-3 >= 0) num.substring(i-3, i) else num.substring(0, i)
  } yield piece
  seq.reverse.flatMap(s => if(s.trim.isEmpty) None else Some(s) )
}

def toString(num: String): String = {
  val n = num.replaceAll("[^0-9]", "").replaceFirst("^0+(?!$)", "")
  val seq = slide(n)
  val arraySuffix = Array("billion", "million", "thousand", "")

  seq.foldLeft((Math.abs(seq.length-4),new StringBuilder())){case ((suffix, res), n) =>
    (suffix + 1, res.append(getString(n) + " " + arraySuffix(suffix) + ", "))
  }._2.toString()

}

println("OUTPUT: "  + toString(number))