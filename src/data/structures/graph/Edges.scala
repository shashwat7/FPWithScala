package data.structures.graph

/**
  * Created by srastogi on 29-Mar-17.
  */

// Edge between two vertices of type A
sealed trait Edges{
  def edgeExists(src: Int, dest: Int): Boolean
  def addEdge(src: Int, dest: Int): Unit
  def allVerticesFrom(src: Int): Seq[Int]
}

// Adjacency List representation of Edge
case class AdjacencyList(numOfVertices: Int) extends Edges{

  // Initially all list are empty
  val lists = Array.fill[List[Int]](numOfVertices)(List.empty)

  override def edgeExists(src: Int, dest: Int): Boolean = lists(src).contains(dest)

  override def addEdge(src: Int, dest: Int): Unit = if(!edgeExists(src,dest)) lists(src) = dest::lists(src)

  override def allVerticesFrom(src: Int): Seq[Int] = lists(src)
}

// Adjacency Matrix representation of Edge
case class AdjacencyMatrix(numOfVertices: Int) extends Edges{

  // Initially all position in the matrix are false
  val matrix = Array.fill[Array[Boolean]](numOfVertices)(
    Array.fill[Boolean](numOfVertices)(false)
  )

  override def edgeExists(src: Int, dest: Int): Boolean = matrix(src)(dest)

  override def addEdge(src: Int, dest: Int): Unit = if(!edgeExists(src,dest)) matrix(src)(dest) = true

  override def allVerticesFrom(src: Int): Seq[Int] = matrix(src).zip(0 until numOfVertices).flatMap{case (edge, idx) =>
    if(edge) Some(idx) else None
  }

}
