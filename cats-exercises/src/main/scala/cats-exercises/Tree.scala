package myTree
import cats.Applicative

object tree {
  sealed abstract class Tree[A] extends Product with Serializable {
    def traverse[F[_]: Applicative, B](f: A => F[B]): F[Tree[B]] = {
      val af = Applicative[F]
      this match {
        case Tree.Empty() => af.pure(Tree.Empty())
        case Tree.Branch(v, l, r) => af.map3(f(v), l.traverse(f), r.traverse(f))(Tree.Branch(_, _, _))
        case Tree.Leaf(v) => af.map(f(v))(Tree.Leaf(_))
      }
    }

    def map[B](f: A => B): Tree[B] = this match {
      case Tree.Empty() => Tree.Empty()
      case Tree.Leaf(v) => Tree.Leaf(f(v))
      case Tree.Branch(v, l, r) => Tree.Branch(f(v), l.map(f), r.map(f))
    }
  }

  object Tree {
    final case class Empty[A]() extends Tree[A]
    final case class Leaf[A](value: A) extends Tree[A]
    final case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]
  }
}