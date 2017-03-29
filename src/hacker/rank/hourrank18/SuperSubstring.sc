def isSuper(s: String): Boolean = s == "0" || (!s.startsWith("0") && isDivisibleBy6(s))
def isDivisibleBy6(str: String) = isDivisibleBy2(str) && isDivisibleBy3(str)
def isDivisibleBy2(str: String) = str.last.toInt % 2 == 0
def isDivisibleBy3(str: String) = str.map(_.toInt).sum % 3 == 0

var s = "0630033563860000370956238050800516680209600904693616000606306760609036423967006036668033396009509608696766940663696692090040698967204744036340933369020960036063006646986097068660900996000900909969663969068013306749990660334560966660303399936699606661363900603034664686073010629690792205984371600960623666668310608666089066890033590910160378891000764949600836090003636304600689850007861603633556090033669308696298306096406362700308090125809806383090936066633405000690566166003038646027086638097369646663616036060683000678999040906035920460336056086002064663396608608806963460350406994220008014990661007606336360996263663063466002100240149823602868900939033549406082389894665660560466814569068030090938200010006260396056316506629803009632909666636544695486217066300046063697646694610008906630430086038566330006086409683806664046090692646493409026670301033300656828399659303460942279003009034300841300050363333030663333096635606960676124292003539319848350630992023498490696000904960266302975030606940654"
val savedResult = scala.collection.mutable.Map.empty[String, Int]


val answer = (
  for{
    len <- 1 to s.length
    res = s.sliding(len,1)
      .foldLeft(0){case (sum, sub) =>
        if(savedResult.contains(sub)) savedResult(sub) + sum
        else{
          if(isSuper(sub)) sum + 1 else sum
        }
      }
  } yield res
  ).foldLeft(0)(_ + _)

def slideCount(s: String): Int = {
  if(savedResult.contains(s)) savedResult(s)
  else{
    if(s.length == 1)
      if(s == "0" || s == "6") {savedResult.put(s,1); 1}
      else {savedResult.put(s,0); 0}
    else{
      val left = s.substring(0, s.length -1)
      val lCount = slideCount(left)
      savedResult.put(left,lCount)
      lCount + (
        for{
          i <- 1 to s.length-1
          isS = isSuper(s.substring(i, s.length))
        } yield isS
        ).foldLeft(0){case (sum,t) => if(t) sum + 1 else sum}
    }
  }
}

def editorialSolution(s: String): Int = {

  def allEvenSubstringsStartingAtiWithMod3(i: Int): List[(String, Int)] = {
    if(i < 0 || i >= s.length) List.empty[(String, Int)]
    else if(i == s.length-1) {
      val c = s.charAt(i)
      if(c % 2 ==0) List((c.toString, c.toInt % 3))
      else List.empty
    } else{
      val iPlus1: List[(String, Int)] = allEvenSubstringsStartingAtiWithMod3(i+1)
      val newStr: List[(String, Int)] = iPlus1.filter{case (subStr, mod3) => subStr.startsWith(s.charAt(i+1).toString)}
        .map{case (subStr, mod3) => (s.charAt(i) + subStr,(mod3 + s.charAt(i).toInt) % 3)}
      newStr ::: iPlus1
    }
  }

  allEvenSubstringsStartingAtiWithMod3(0)
    .filter{case (sub, mod3) => mod3 == 0}
    .size
}

val answer_2 = slideCount(s)
val answer_3 = editorialSolution(s)

