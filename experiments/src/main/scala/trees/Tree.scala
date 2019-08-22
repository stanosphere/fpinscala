package trees

sealed trait RoseTree[+A] {
  // gonna put the boilerplate to have infix stuff here
  def map[B](f: A => B): RoseTree[B] = RoseTree.map(f)(this)
  def size: Int = RoseTree.size(this)
  def traverse[R](mapper: A => R)(reducer: (R, R) => R): R =
    RoseTree.traverse(mapper)(reducer)(this)
}
case class Leaf[A](value: A) extends RoseTree[A]
case class Node[A](value: A, children: List[RoseTree[A]]) extends RoseTree[A]

object RoseTree {
  def size[A](tree: RoseTree[A]): Int = tree match {
    case Leaf(_) => 1
    case Node(_, children) => 1 + children.map(size).sum
  }

  def map[A,B](f: A => B)(tree: RoseTree[A]): RoseTree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Node(v, children) => Node(f(v), children.map(map(f)))
  }

//  def maximum(tree: RoseTree[Int]): Int = tree match {
//    case Leaf(value) => value
//    case Branch(left, right) => maximum(left) max maximum(right)
//  }
//
//  // define a tree consisting of only a leaf to have a depth of 0
//  def depth[A](tree: RoseTree[A]): Int = tree match {
//    case Leaf(_) => 0
//    case Branch(left, right) => 1 + (depth(left) max depth(right))
//  }

  def traverse[A,R](mapper: A => R)( reducer: (R, R) => R)(node: RoseTree[A]): R =
    node match {
      case Leaf(v) => mapper(v)
      case Node(v, children) => {
        val z = mapper(v)
        val myTraverse = traverse(mapper)(reducer)(_)
        children.foldLeft(z)((acc, child) => reducer(acc, myTraverse(child)))
      }
    }
}

