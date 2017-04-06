import scala.annotation.tailrec

def factorial(n: Int): Int = {
  if(n==1) 1
  else n * factorial(n-1)
}

@tailrec
def factorial2(n: Int, res: Int): Int = {
  if(n==1) res
  else factorial2(n-1, res * n)
}

factorial2(5,1)

def maxProfit() = {
//  val arr: Array[Int] = Array(1,2,3,4,5,4,3,2,1)
   // val arr: Array[Int] = Array(5,4,3,2,1,2,3,4,5)
  //  val arr: Array[Int] = Array(1,2,3,4,5,6,7,8,9)
  //  val arr: Array[Int] = Array(9,8,7,6,5,4,3,2,1)
  //  val arr: Array[Int] = Array(5,5,5,5,5,5,5,5,5)
   val arr: Array[Int] = Array(5,40,3,2,1)
  var minPrice = arr(0)
  var maxProfit = arr(1)-arr(0)
  var soldPrice = arr(1)
  for(i<- 1 until arr.length){
    if(minPrice > arr(i)) minPrice = arr(i)
    if(maxProfit < arr(i)-minPrice) {
      soldPrice = arr(i)
      maxProfit = arr(i)-minPrice
    }
  }

//  def go(i: Int, minPrice: Int, soldPrice: Int, maxProfit: Int) = {
//    if(i >= arr.length)
//  }


  if(maxProfit > 0){
    println("buying price: " + minPrice)
    println("selling price: " + soldPrice)
    println("max profit: " + maxProfit)
  } else println("No right time to buy, don't buy!")

}

maxProfit()