import chapter3._

val a = chapter3.List(1,2,3,4,5)
val b = chapter3.List(6,7,8,9)

// 3.7
chapter3.List.foldRight(a, Nil: List[Int]) (Cons(_,_))

// 3.8
chapter3.List.length(a)

// 3.11
chapter3.List.sum3(a)
chapter3.List.product3(chapter3.List(2.0, 5.0))
chapter3.List.length2(a)

// 3.12
chapter3.List.reverse(a)

// 3.14
chapter3.List.appendUsingFold(a, b)

// 3.15
chapter3.List.flatten(chapter3.List(chapter3.List(1,2), chapter3.List(3,4)))

// 3.16
List.addOne(a)

// 3.20
List.filter(a)(_%2==0)
// 3.21
List.filterUsingFlatMap(a)(_%2==0)

//3.22
List.addAllElements(a,b)

// 3.24
List.hasSubsequence(List(1,2,3,4,5), List(2,3))

val tree1 = Branch(1, Branch(2, Leaf(4), Leaf(5)), Branch(3, Leaf(6), Branch(7, Leaf(8), Leaf(9))))

Tree.map(tree1)(_ + 1)
Tree.size(tree1)
Tree.sizeUsingFold(tree1)
Tree.maximum(tree1)
Tree.maximumUsingFold(tree1)
Tree.depth(tree1)
Tree.depthUsingFold(tree1)
