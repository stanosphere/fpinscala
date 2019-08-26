package trees

sealed trait RoseTree[+A] {
  val value: A
  // gonna put the boilerplate to have infix stuff here
  def map[B](f: A => B): RoseTree[B] = RoseTree.map(f)(this)
  def size: Int = RoseTree.size(this)
  def depth: Int = RoseTree.depth(this)
  def reduce[R](mapper: A => R)(reducer: (R, R) => R): R =
    RoseTree.reduce(mapper)(reducer)(this)
}

case class Leaf[A](value: A) extends RoseTree[A]
case class Node[A](value: A, children: List[RoseTree[A]]) extends RoseTree[A]

object RoseTree {
  def flatMap[A,B](f: A => RoseTree[B])(tree: RoseTree[A]): RoseTree[B] =
    tree match {
      case Leaf(v) => f(v)
      case Node(v, children) => {
        val newChildren = children.flatMap(c => List(flatMap(f)(c)))
        f(v) match {
          case Leaf(v) => Node(v, newChildren)
          case Node(v, fChildren) => Node(v, fChildren ++ newChildren)
        }
      }
    }

  // this will just remove one layer of nesting like it does for lists
  def flatten[A](tree: RoseTree[A]): RoseTree[A] = ???

  def map[A,B](f: A => B)(tree: RoseTree[A]): RoseTree[B] =
    tree match {
      case Leaf(v) => Leaf(f(v))
      case Node(v, children) => Node(f(v), children.map(map(f)))
    }

  // define a tree consisting of only a leaf to have a depth of 1
  def depth[A](tree: RoseTree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Node(_, children) => 1 + children.foldLeft(0)((d, c) => d max depth(c))
    }

  // the mapper isn't strictly necessary but I'm gonna leave it in for now
  def reduce[A,R](mapper: A => R)(reducer: (R, R) => R)(node: RoseTree[A]): R =
    node match {
      case Leaf(v) => mapper(v)
      case Node(v, children) => {
        val z = mapper(v)
        val myTraverse = reduce(mapper)(reducer)(_)
        children.foldLeft(z)((acc, child) => reducer(acc, myTraverse(child)))
      }
    }

  def size[A](tree: RoseTree[A]): Int =
    reduce((_: A) => 1)(_+_)(tree)

  def maximum(tree: RoseTree[Int]): Int =
    reduce((x: Int) => x)(_ max _)(tree)
}

