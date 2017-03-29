import data.structures.graph.Graph

val g = Graph(5)
g.addEdge(0,1)
g.addEdge(0,2)
g.addEdge(1,2)
g.addEdge(2,3)
g.addEdge(2,0)
g.addEdge(3,4)

Graph.breadthFirstTraversal(g, 2)
Graph.shortestPathOfAllNodesFrom(g,2)

Graph.depthFirstSearch(g)
Graph.hasCycle(g)

val dag = Graph(6)
dag.addEdge(0,1)
dag.addEdge(1,2)
dag.addEdge(1,3)
dag.addEdge(3,2)
dag.addEdge(4,1)
dag.addEdge(5,0)
Graph.topologicalSort(dag)