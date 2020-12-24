package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  // define a tree consisting of only a leaf to have a depth of 0
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(handleLeaf: A => B)(combineBranches: (B, B) => B): B =
    tree match {
      case Leaf(value) => handleLeaf(value)
      case Branch(left, right) =>
        combineBranches(
          fold(left)(handleLeaf)(combineBranches),
          fold(right)(handleLeaf)(combineBranches)
        )
    }

  def sizeViaFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)(_ + _)

  def maxViaFold(tree: Tree[Int]): Int =
    fold(tree)(identity)(_ max _)

  def depthViaFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 0)((left, right) => 1 + (left max right))

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    val valueToMappedLeaf = (value: A) => Leaf(f(value)): Tree[B]
    fold(tree)(valueToMappedLeaf)(Branch(_, _))
  }
}
