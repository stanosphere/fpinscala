package fpinscala.monoids

import Monoid._

object PlayingWithMonoids extends App {
  def runFoldMapV(): Unit = {
    val mySeq = IndexedSeq(1,2,3,4)
    println(foldMapV(mySeq, intMultiplication)(identity))
    println(foldMapV(mySeq, intAddition)(identity))
  }

  def runOrdered(): Unit = {
    val orderedList = List(1,2,3,4,5)
    println(ordered(orderedList))
    val notOrderedList = List(1,3,2,5,4)
    println(ordered(notOrderedList))
  }

  def rightFirst[A](a: A,b: A, c: A)(m: Monoid[A]): A =
    m.op(a, m.op(b, c))

  def leftFirst[A](a: A,b: A, c: A)(m: Monoid[A]): A =
    m.op(m.op(a, b), c)

  def runWcMonoid(): Unit = {
    val a: WC = Part("q",16,"HPqur")
    val b: WC = Stub("ToLKom")
    val c: WC = Stub("lwjOpD")
    println(leftFirst(a,b,c)(wcMonoid))
    println(rightFirst(a,b,c)(wcMonoid))
  }

  runFoldMapV()
  runOrdered()
  runWcMonoid()
}
