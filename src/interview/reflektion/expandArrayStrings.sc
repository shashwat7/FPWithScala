import scala.collection.mutable

/*
Input:
['abc', 'd', 'ef']

Output:
a
d
e
b
f
c

target complexity:
Space: O(n)
Time: O(c)
*/

val arr = Array("abcd", "d", "efg")

def expand = {
  val map: scala.collection.mutable.Map[Int, List[Char]] =
    mutable.Map.empty[Int, List[Char]]
  var largestStringCount = -1
  arr.foreach{str => str.zipWithIndex.foreach{
    case (c,i) =>
      if(largestStringCount < i) largestStringCount = i
      map.put(i,map.getOrElse(i, List.empty[Char]) :+ c)
  }}
  for{
    i <- 0 to largestStringCount
    c <- map(i)
  } println(c)
}

expand