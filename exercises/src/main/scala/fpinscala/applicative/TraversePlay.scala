package fpinscala.applicative

import Traverse.{listTraverse, optionTraverse, treeTraverse}
import Applicative.{idApp, listApp, optionApp}
import fpinscala.monads.Id

object TraversePlay extends App {
  // remember we'll need some implicits for the Gs (I think!!!)

  // this traversal will just wrap the list into an ID
  val boring = listTraverse.traverse(List("Hi", 1, 2))(Id(_))
  println(boring)

  // a horribly contrived way of implementing foAll
  def forAll[A](as: List[A])(p: A => Boolean): Boolean =
    listTraverse.traverse(as)(x => if (p(x)) Some(()) else None).isDefined

  println(forAll(List(1, 2, 3, 4, 5))(_ < 10))
  println(forAll(List(1, 2, 3, 4, 5))(_ < 5))

  // can I generalise forAll??
  def forAll2[C[_], A](as: C[A])(p: A => Boolean)(implicit C: Traverse[C]): Boolean = {
    C.traverse(as)(x => if (p(x)) Some(()) else None).isDefined
  }

  implicit val l: Traverse[List] = listTraverse
  implicit val t: Traverse[Tree] = treeTraverse
  implicit val o: Traverse[Option] = optionTraverse

  println(forAll2(List(1, 2, 3, 4, 5))(_ < 10))
  println(forAll2(List(1, 2, 3, 4, 5))(_ < 5))

  def leaf[A](a: A): Tree[A] = Tree(a, Nil: List[Tree[A]])

  val myTree = Tree(1, List(2, 3, 4, 5).map(leaf))

  println(forAll2(myTree)(_ < 10))
  println(forAll2(myTree)(_ < 5))

  val anotherTree = Tree(1,
    List(
      Tree(2, List(3, 4, 5).map(leaf)),
      Tree(6, List(7, 8, 9).map(leaf))
    )
  )

  println(anotherTree)
  println(treeTraverse reverse anotherTree)
  println(treeTraverse zipWithIndex anotherTree)
  println(listTraverse.foldLeft(List(1, 2, 3, 4))(0)(_ + _))
  println(treeTraverse.foldLeft(anotherTree)(0)(_ - _))
  println(treeTraverse.foldRight(anotherTree)(0)(_ - _))

  val mad = listTraverse.compose[Tree].compose[Option]

  val stuff: List[Tree[Option[Int]]] =
    List(Tree(None, Nil), Tree(Some(10), List(None, Some(5)).map(leaf)))

  val crazy = mad.traverse(stuff)(x => List(x, x))

  crazy.foreach(println)


}
