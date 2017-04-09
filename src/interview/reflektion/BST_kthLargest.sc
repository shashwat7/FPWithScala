case class Node[A](value: A, left: Node[A], right: Node[A])
case class Tree[A](root: Node[A])

var c = 0
def findKthLargest[A](root: Node[A], k: Int): Unit = {
  if(root.right != null && c < k) findKthLargest(root.right, k)
  c = c + 1
  if(c == k) println(root.value)
  if(root.left != null && c < k) findKthLargest(root.left, k)
}

val bst = Tree(Node(3,Node(2,Node(1, null, null),null), Node(4, null, Node(5, null, null))))

findKthLargest(bst.root, 2)