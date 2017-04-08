import scala.collection.mutable

def prefixToPostfixArray(prefixes: Array[String]): Array[String] = {
  for{str <- prefixes} yield prefixToPostfix(str)
}

def prefixToPostfix(prefix: String): String = {
  val l = prefix.length
  val stack = mutable.Stack[String]()
  for(i <- l-1 to 0 by -1){
    if(isOperator(prefix(i))){
      val t1: String = stack.top; stack.pop()
      val t2: String = stack.top; stack.pop()
      stack.push(t1 + t2 + prefix(i))
    } else stack.push("" + prefix(i))
  }
  stack.pop()
}

def isOperator(c: Char): Boolean = {
  if(c == '+' || c== '-' || c == '/' || c == '*') true
  else false
}

prefixToPostfix("*34")
prefixToPostfix("+1*23")
prefixToPostfix("+1**23/14")