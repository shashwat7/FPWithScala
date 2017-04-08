import scala.collection.mutable

case class Graph(n: Int, adjList: Array[List[Int]]){
  def addEdge(a: Int, b: Int) = adjList(a) = adjList(a) :+ b
}

def connectedCities(n: Int, g: Int, originCities: Array[Int], destinationCities: Array[Int]): Array[Int] = {

  // Build graph - There is a vertex between a and b if gcd(a,b) > g
  val graph = Graph(n, Array.fill(n)(List.empty[Int]))
  for{
    i <- 0 until n
    j <- 0 until n if i != j && gcd(i+1,j+1) > g
  } yield graph.addEdge(j,i)
  //graph.adjList.foreach(println)

  originCities.zip(destinationCities).map{
    case (origin,destination) => if (pathExists(origin-1,destination-1, graph)) {
      println("Path exists between: '" + origin + "' to '" + destination + "'")
      1
    } else {
      println("Path does not exists between: '" + origin + "' to '" + destination + "'")
      0
    }
  }
}

// This is basically a DFS starting from start until end
def pathExists(start: Int, end: Int, graph: Graph): Boolean = {
  val q = mutable.Queue.empty[Int]
  val visited = mutable.HashSet.empty[Int]
  q.enqueue(start)
  var c = start
  while(q.nonEmpty && c!=end){
    c = q.dequeue()
    visited.add(c)
    graph.adjList(c).foreach(v => if(!visited.contains(v)) q.enqueue(v))
  }
  c == end
}

def gcd(a: Int, b: Int): Int = {
  if(a > b) gcd(b,a)
  else {
    if(b%a == 0) a
    else gcd(b%a, a)
  }
}

connectedCities(6,1,Array(1,2,3,4), Array(2,3,4,5))
  .foreach(println)