package data.structures.graph

import scala.collection.mutable

/**
  * Created by srastogi on 29-Mar-17.
  */

case class Graph(numOfVertices: Int, edges: Edges){
  def addEdge(src: Int, dest: Int, weight: Double = 1.0): Unit = edges.addEdge(src,dest, weight)
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

  /**
    * Find shortest path between src vertex and dest vertex in Graph g.
    * Assume g, does not have any negative.
    * */
  def shortestPath(src: Int, dest: Int, g: Graph): Double = {
    val distance: Array[Double] = Array.fill(g.numOfVertices)(Double.MinPositiveValue)
    val parent: mutable.Map[Int, Int] = mutable.Map.empty[Int, Int]
    val q: mutable.Queue[Int] = mutable.Queue.empty[Int]
    distance(src) = 0
    parent.put(src,Integer.MIN_VALUE) //dummy value
    q.enqueue(src)
    while(q.nonEmpty){
      val curr = q.dequeue()
      //println("visiting : " + curr + ". disctance: " + distance(curr))
      g.edges.allVerticesFrom(curr).foreach{c =>
        //println("next  : " + c + ", distance before correction: " + distance(c))
        if(distance(c) == Double.MinPositiveValue || distance(c) > distance(curr) + g.edges.getWeight(curr,c)){
          distance(c) = distance(curr) + g.edges.getWeight(curr,c)
          parent.put(c,curr)
          q.enqueue(c)
        }
      }
    }
    // Print
    val stk = new mutable.Stack[Int]
    var p = parent.get(dest)
    while(p.isDefined && p.get != src) {stk.push(p.get); p = parent.get(p.get)}
    print(src + "->")
    while(stk.nonEmpty){print(stk.pop() + "->")}
    print(dest)
    println("\nDistance: " + distance(dest))
    distance(dest)
  }


  def dijkstra(g: Graph, start: Int) = {
    val distance = Array.fill[Double](g.numOfVertices)(Double.MaxValue) // array used as a queue.
    val completed = mutable.HashMap.empty[Int, Double] // Stores indexes of vertices already completed
    distance(start) = 0
    completed.put(start,0)
    println(getMinimum)
    while(getMinimum._1 != -1){ // while the queue is non-empty
    val (currIdx,currDist) = getMinimum
      distance(currIdx) = Double.NaN // Remove from distance queue
      if(currDist != Double.NaN){
        println("On: " + currIdx + ". Distance: " + currDist)
        completed.put(currIdx,currDist)
        g.edges.allVerticesFrom(currIdx).foreach{ c =>
          if(distance(c) >= currDist + g.edges.getWeight(currIdx,c))
            distance(c) = currDist + g.edges.getWeight(currIdx,c)
        }
      }
    }

    def getMinimum: (Int, Double) = {
      var minDist = Double.MaxValue
      var minIdx = -1
      (0 until g.numOfVertices).zip(distance).foreach{ case (curIdx, currDist) =>
        if(currDist!= Double.NaN && currDist<=minDist){minDist = currDist; minIdx = curIdx}
      }
      (minIdx, minDist)
    }
  }


  def bellmanFord(g: Graph, start: Int) = {
    val distance: Array[Double] = Array.fill[Double](g.numOfVertices)(Double.MaxValue)
    val parent: mutable.Map[Int, Int] = mutable.Map.empty[Int, Int]
    distance(start) = 0
    // Set each edge
    for(i<- 0 until g.numOfVertices){
      for((u,v) <- g.edges.getAllEdges){
        relax(u,v)
      }
    }
    // Check for cycles
    for((u,v) <- g.edges.getAllEdges){
      if(distance(v) > distance(u) + g.edges.getWeight(u,v)){
        distance(v) = Double.NaN
        println("Negative cycle exists! It passes through vertex - " + v)
      } else
        println(s"Shortest Path from $start to vertex $v is: " + distance(v))
    }
    // Print

    def relax(u: Int, v: Int) = {
      val w = g.edges.getWeight(u,v)
      if(distance(v) > distance(u) + w){
        distance(v) = distance(u) + w
        parent.put(v,u)
      }
    }

  }



  def apply(numOfVertices: Int): Graph = {
    new Graph(numOfVertices, AdjacencyList(numOfVertices))
  }

  def createWeightedGraph(numOfVertices: Int): Graph =
    new Graph(numOfVertices, WeightedAdjacenyList(numOfVertices))

}


