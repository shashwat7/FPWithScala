import scala.collection.mutable

val coins = List(25, 10, 5)
val v = 51
val minCoinValue = coins.foldLeft(coins.head){case (minV, c) => Math.min(minV,c)}

/*
* Recursion : asymptotic complexity is exponential.
* Trick: Save the intermediate results
* */
def findMinimumNumberOfCoinsReq(v: Int): Int = {
  if(v < minCoinValue) Integer.MAX_VALUE
  else if (coins.contains(v)) 1
  else {
    coins.foldLeft(Integer.MAX_VALUE){case (minV, c) =>
      if(v-c >=  minCoinValue)
        if(findMinimumNumberOfCoinsReq(v-c) != Integer.MAX_VALUE)
          Math.min(minV, findMinimumNumberOfCoinsReq(v-c) + 1)
        else minV
      else minV
    }
  }
}

def findMinimumNumberOfCoinsReq_dp(v: Int): Int = {
  val resultMap: mutable.Map[Int, Int] = mutable.Map.empty[Int, Int]
  resultMap.put(0,0)
  for (v <- 1 to v+1){
    val noOfCoins = coins.foldLeft(Integer.MAX_VALUE){case (minV, c) =>
      if(c <= v) {
        val savedValue = resultMap.getOrElse(v-c, Integer.MAX_VALUE)
        if(savedValue != Integer.MAX_VALUE) Math.min(minV, savedValue + 1)
        else minV
      }
      else minV
    }
    if(noOfCoins != Integer.MAX_VALUE) resultMap.put(v, noOfCoins)
  }
  resultMap.getOrElse(v, Integer.MAX_VALUE)
}

findMinimumNumberOfCoinsReq(v)
findMinimumNumberOfCoinsReq_dp(v)
