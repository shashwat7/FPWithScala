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

val cache: mutable.Map[String,Int] = mutable.Map.empty[String, Int]
def semesterCanBeTakenOn(course: String): Int = {
  if(cache.contains(course)) cache(course)
  else{
    if(courseDependentUpon(course).isEmpty) {
      cache.put(course,1)
      1
    } else{
      val numOfSemester = courseDependentUpon(course).map{c => semesterCanBeTakenOn(c)}.max + 1
      cache.put(course,numOfSemester)
      numOfSemester
    }
  }
}
val minimumNumberOfSemesterRequired: Int = courseDependentUpon.keySet.map{course =>
  val s = semesterCanBeTakenOn(course)
  println("Course '" + course + "' can be taken on semester: " + s)
  s
}.max

println("Minimum Number of semesters required: " + minimumNumberOfSemesterRequired)

//val graph = Map(
//  "programming" -> List("ds","matlab","ai"),
//  "ds" -> List(),
//  "matlab" -> List("imp"),
//  "ai" -> List("imp"),
//  "imp" -> List(),
//  "calculus" -> List("stats"),
//  "stats" -> List("ai"),
//  "la" -> List("stats")
//)

// Topological Sort is needed to determine the order in which we can take these courses,
// but it would be difficult to tell which course to take on which semester
//def minimumNumberOfSemester(graph: Map[String, List[String]]): Int = {
//  val stack: mutable.Stack[String] = new mutable.Stack[String]
//  val visited: mutable.Map[String, Boolean] = graph.foldLeft(mutable.Map.empty[String, Boolean]){ case (map, (k,v)) => map.put(k,false); map}
//
//  // To add all elements in the stack
//  def addAllDependencies(course: String): Unit = {
//    if(!visited(course)){
//      visited.put(course, true)
//      graph(course).foreach{c => addAllDependencies(c)}
//      stack.push(course)
//    }
//  }
//
//  graph.foreach{case (k,v) => addAllDependencies(k)}
//  var minNumOfSemester = 0
//  while(stack.nonEmpty){
//    val course = stack.pop()
////    println("Course '" + course + "' can be taken on semester: " + semester)
////    if(minNumOfSemester < semester) minNumOfSemester = semester
//  }
//  minNumOfSemester
//}
