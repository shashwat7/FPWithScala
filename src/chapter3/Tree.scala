package chapter3

/**
  * Created by shashwat on 13/3/17.
  */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{

  // 3.26 -> Count number of nodes
  def size[A](t: Tree[A]): Int = {
    t match {
      case leaf: Leaf[A] => 1
      case branch: Branch[A] => 1 + size(branch.left) + size(branch.right)
    }
  }

  // 3.27 -> Find max
  def maximum(t: Tree[Int]): Int = {
    t match{
      case leaf: Leaf[Int] => leaf.value
      case branch: Branch[Int] => branch.value max maximum(branch.left) max maximum(branch.right)
    }
  }

  // 3.28 -> Find depth
  def depth[A](t: Tree[A]): Int = {
    t match {
      case leaf: Leaf[A] => 1
      case branch: Branch[A] => (depth(branch.left) max depth(branch.right)) + 1
    }
  }

  // 3.29
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(value) => Leaf(f(value))
      case Branch(value, left, right) => Branch(f(value), map(left)(f), map(right)(f))
    }
  }

  // 3.30
  def fold[A,B](t: Tree[A], z: B)(f:(A,B) => B)(g:(B,B) => B): B = {
    t match{
      case Leaf(value) => f(value, z)
      case Branch(value, left, right) =>
        val leftFold = fold(left, z)(f)(g)
        val rightFold = fold(right, z)(f)(g)
        g(f(value, leftFold), rightFold)
    }
  }

  def sizeUsingFold[A](t: Tree[A]): Int = {
    fold(t, 0){ case (tree, res) => res + 1}{ case (leftRes, rightRes) => leftRes + rightRes}
  }

  def maximumUsingFold(t: Tree[Int]): Int = {
    fold(t, Integer.MIN_VALUE){ case (tree, res) => tree max res}{ case (leftRes, rightRes) => leftRes max rightRes}
  }

  def depthUsingFold[A](t: Tree[A]): Int = {
    fold(t, 1){ case (tree, res) => res + 1}{ case (leftRes, rightRes) => leftRes max rightRes}
  }


}

