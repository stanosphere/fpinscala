package monoid

import cats.kernel.Monoid
import cats.implicits._

object OhLookSomeMonoids extends App {
  def stringyMonoids(): Unit = {
    println(Monoid[String].empty)
    println(Monoid[String].combineAll(List("a", "b", "c")))
    println(Monoid[String].combineAll(Nil))
  }

  def monoidCompositionExample(): Unit = {
    val m1 = Map("a" -> 1, "b" -> 2)
    val m2 = Map("a" -> 3)
    val combined = Monoid[Map[String, Int]].combineAll(List(m1, m2))
    println(combined)

    val empty = Monoid[Map[String, Int]].combineAll(Nil)
    println(empty)
  }

  // cats already does this for us!
//  implicit def monoidTuple[A: Monoid, B: Monoid]: Monoid[(A, B)] =
//    new Monoid[(A, B)] {
//      def combine(x: (A, B), y: (A, B)): (A, B) = {
//        val (xa, xb) = x
//        val (ya, yb) = y
//        (Monoid[A].combine(xa, ya), Monoid[B].combine(xb, yb))
//      }
//      def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)
//    }

  def demoTupleMonoid(): Unit = {
    val l = List(1, 2, 3, 4, 5)
    println(l.foldMap(i ⇒ (i, i.toString)))
  }

  def main(): Unit = {
    stringyMonoids()
    monoidCompositionExample()
    demoTupleMonoid()
  }

  main()
}

