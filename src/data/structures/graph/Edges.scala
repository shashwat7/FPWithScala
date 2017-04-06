package data.structures.graph

/**
  * Created by srastogi on 29-Mar-17.
  */

// Edge between two vertices of type A
sealed trait Edges{
  def edgeExists(src: Int, dest: Int): Boolean
  def addEdge(src: Int, dest: Int, weight: Double = 1): Unit
  def allVerticesFrom(src: Int): Seq[Int]
  def getWeight(src: Int, dest: Int): Double
  def getAllEdges: List[(Int,Int)]
}

// Adjacency List representation of Edge
case class AdjacencyList(numOfVertices: Int) extends Edges{

  // Initially all list are empty
  val lists = Array.fill[List[Int]](numOfVertices)(List.empty)

  override def edgeExists(src: Int, dest: Int): Boolean = lists(src).contains(dest)

  override def addEdge(src: Int, dest: Int, weight: Double = 1): Unit = if(!edgeExists(src,dest)) lists(src) = dest::lists(src)

  override def allVerticesFrom(src: Int): Seq[Int] = lists(src)

  override def getWeight(src: Int, dest: Int): Double = 1

  override def getAllEdges: List[(Int, Int)] = (for{
    i <- 0 until numOfVertices
    dest <- allVerticesFrom(i)
  } yield (i,dest)).toList

}

// Adjacency Matrix representation of Edge
case class AdjacencyMatrix(numOfVertices: Int) extends Edges{

  // Initially all position in the matrix are false
  val matrix = Array.fill[Array[Boolean]](numOfVertices)(
    Array.fill[Boolean](numOfVertices)(false)
  )

  override def edgeExists(src: Int, dest: Int): Boolean = matrix(src)(dest)

  override def addEdge(src: Int, dest: Int, weight: Double = 1): Unit = if(!edgeExists(src,dest)) matrix(src)(dest) = true

  override def allVerticesFrom(src: Int): Seq[Int] = matrix(src).zip(0 until numOfVertices).flatMap{case (edge, idx) =>
    if(edge) Some(idx) else None
  }

  override def getWeight(src: Int, dest: Int): Double = 1

  override def getAllEdges: List[(Int, Int)] = (for{
    i <- 0 until numOfVertices
    dest <- allVerticesFrom(i)
  } yield (i,dest)).toList

}

case class WeightedAdjacenyList(numOfVertices: Int) extends Edges{

  // Initially all list are empty
  val lists = Array.fill[List[(Int, Double)]](numOfVertices)(List.empty)

  override def edgeExists(src: Int, dest: Int): Boolean = lists(src).map(_._1).contains(dest)

  override def addEdge(src: Int, dest: Int, weight: Double): Unit = if(!edgeExists(src,dest)) lists(src) = (dest,weight)::lists(src)

  override def allVerticesFrom(src: Int): Seq[Int] = lists(src).map(_._1)

  override def getWeight(src: Int, dest: Int): Double = lists(src).toMap.getOrElse(dest, 0)

  override def getAllEdges: List[(Int, Int)] = (for{
    i <- 0 until numOfVertices
    dest <- allVerticesFrom(i)
  } yield (i,dest)).toList
}
