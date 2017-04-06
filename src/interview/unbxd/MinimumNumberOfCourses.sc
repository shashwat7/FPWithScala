import scala.collection.mutable

// CourseName, DependentUpon

val courseDependentUpon = Map(
  "ds" -> List("programming"),
  "ai" -> List("stats","programming"),
  "matlab" -> List("programming"),
  "imp" -> List("ai","matlab"),
  "stats" -> List("calculus","la"),
  "calculus" -> List(),
  "la" -> List(),
  "programming" -> List()
)

val graph = Map(
  "programming" -> List("ds","matlab","ai"),
  "ds" -> List(),
  "matlab" -> List("imp"),
  "ai" -> List("imp"),
  "imp" -> List(),
  "calculus" -> List("stats"),
  "stats" -> List("ai"),
  "la" -> List("stats")
)

def minimumNumberOfSemester(graph: Map[String, List[String]]): Int = {
  val stack: mutable.Stack[(String, Int)] = new mutable.Stack[(String, Int)]
  val visited: mutable.Map[String, Boolean] = graph.foldLeft(mutable.Map.empty[String, Boolean]){ case (map, (k,v)) => map.put(k,false); map}

  // To add all elements in the stack
  def addAllDependencies(course: String, n: Int): Unit = {
    if(!visited(course)){
      println("At " + course,n )
      visited.put(course, true)
      graph(course).foreach{c => addAllDependencies(c,n+1)}
      stack.push((course,n))
    }
  }

  graph.foreach{case (k,v) => addAllDependencies(k,1)}
  var minNumOfSemester = 0
  while(stack.nonEmpty){
    val (course,semester) = stack.pop()
    println("Course '" + course + "' can be taken on semester: " + semester)
    if(minNumOfSemester < semester) minNumOfSemester = semester
  }
  minNumOfSemester
}

minimumNumberOfSemester(graph)