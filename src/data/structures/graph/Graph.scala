package data.structures.graph

import scala.collection.mutable

/**
  * Created by srastogi on 29-Mar-17.
  */

case class Graph(numOfVertices: Int, edges: Edges){
  def addEdge(src: Int, dest: Int): Unit = edges.addEdge(src,dest)
}

object Graph{

  def breadthFirstTraversal(g: Graph, s: Int) = {
    val visited = Array.fill[Boolean](g.numOfVertices)(false)
    val q = mutable.Queue.empty[Int]
    q.enqueue(s)
    while(q.nonEmpty){
      val curr = q.dequeue()
      println("Visited: " + curr)
      visited(curr) = true
      g.edges.allVerticesFrom(curr).foreach{c => if(!visited(c)) q.enqueue(c) }
    }
  }

  def shortestPathOfAllNodesFrom(g: Graph,s: Int) = {
    val q = mutable.Queue.empty[Int]
    val distance = mutable.Map.empty[Int, Int] // (v, d) => distance of v from s is d
    val parent = mutable.Map.empty[Int, Option[Int]] // (v1,v2) => parent of v1 is v2
    distance.put(s,0)
    parent.put(s,None)
    println("Visited: " + s + ". Distance from start: 0. Parent: -")
    q.enqueue(s)
    var dist = 1
    while(q.nonEmpty){
      val currParent = q.dequeue()
      g.edges.allVerticesFrom(currParent).foreach{c =>
        if(!distance.contains(c)){
          println("Visited: " + c + ". Distance from start: " + dist + ". Parent: " + currParent)
          distance.put(c, dist)
          parent.put(c, Some(currParent))
          q.enqueue(c)
        }
      }
      dist = dist + 1
    }
  }

  def depthFirstSearch(g: Graph) = {
    val visited = Array.fill[Boolean](g.numOfVertices)(false)
    val parent = mutable.Map.empty[Int, Option[Int]] // (v1,v2) => parent of v1 is v2
    def dfs(v: Int): Unit = {
      if(!visited(v)){
        println("Visit: " + v + ". Parent: " + parent(v))
        visited(v) = true
        g.edges.allVerticesFrom(v).foreach{c =>
          parent.put(c, Some(v))
          dfs(c)
        }
      }
    }
    (0 until g.numOfVertices).foreach{v =>
      if(!visited(v)) {parent.put(v, None); dfs(v)}
    } // For disconnected graph
  }

  // Do a DFS and return true if same node comes again.
  def hasCycle(g: Graph): Boolean = {
    val status = Array.fill[Int](g.numOfVertices)(0) // 0 -> not visited, 1 -> in process, 2 -> visited
    def findCycleByDfs(v: Int): Boolean = {
      if(status(v) == 0){
        println("Visit: " + v)
        status(v) = 1
        val isCycle = g.edges.allVerticesFrom(v).foldLeft(false){case (res, c) => res || findCycleByDfs(c)}
        status(v) = 2
        isCycle
      }
      else if(status(v) == 1) true
      else false
    }
    (0 until g.numOfVertices).foldLeft(false){case (res, v) => res || findCycleByDfs(v)}
  }

  /** Given a directed acyclic graph(DAG),
    * re-order the vertices so that all edges point from lower order to higher order.
    * Run DFS and output reverse of finishing time of vertices.
    * @param g is the DAG
    * @return print on system.out the correct topological sequence
    */
  def topologicalSort(g: Graph): Unit = {
    val stk = new mutable.Stack[Int] // For keeping vertices

    val visited = Array.fill[Boolean](g.numOfVertices)(false)
    def dfs(v: Int): Unit = {
      if(!visited(v)){
        visited(v) = true
        g.edges.allVerticesFrom(v).foreach{ c => dfs(c) }
        stk.push(v)
      }
    }
    (0 until g.numOfVertices).foreach{c => dfs(c)} //To run disconnected part of the graph

    while(stk.nonEmpty) {
      print(stk.pop + " ")
    }
  }




  def apply(numOfVertices: Int): Graph = {
    new Graph(numOfVertices, AdjacencyList(numOfVertices))
  }

}


