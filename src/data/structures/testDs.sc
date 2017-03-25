import data.structures.Stack

val stack = Stack[Int](1,2,3,4,5)
val a = stack.getTopElement
val s3 = stack.pop._2
val s4 = s3.push(10)
s4.toList

new