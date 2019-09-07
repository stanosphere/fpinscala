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

  def rightFirst[A](a: A, b: A, c: A)(m: Monoid[A]): A =
    m.op(a, m.op(b, c))

  def leftFirst[A](a: A, b: A, c: A)(m: Monoid[A]): A =
    m.op(m.op(a, b), c)

  def runWcMonoid(): Unit = {
    val a: WC = Part("a",16,"b")
    val b: WC = Stub("c")
    val c: WC = Stub("d")
    println(leftFirst(a,b,c)(wcMonoid))
    println(rightFirst(a,b,c)(wcMonoid))
  }

  def countUsingWCMonoid(s: String): Int = {
    def charToWC(c: Char): WC =
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)

    def stringToCount(x: String): Int =
      if (x.length == 0) 0 else 1

    def wcToInt(wc: WC): Int = wc match {
      case Stub(x) => stringToCount(x)
      case Part(x, cnt, y) => stringToCount(x) + cnt + stringToCount(y)
    }

    val characterSequence: IndexedSeq[Char] = s.toIndexedSeq

    val finalWordCount = foldMapV(characterSequence, wcMonoid)(charToWC)
    println(finalWordCount)
    wcToInt(finalWordCount)
  }

  def checkBagWorks[A](): Unit = {
    val shaky = IndexedSeq("a", "rose", "is", "a", "rose")
    val xss = IndexedSeq(List(1,2), List(1,2), List(1,2,3))
    println(bag(shaky))
    println(bag(xss))
  }

  def calcMean(xs: List[Float]): Float = {
    val m = productMonoid(floatAddition, intAddition)
    val sumAndCount = ListFoldable.foldMap(xs)(x => (x, 1))(m)
    val res = sumAndCount._1 / sumAndCount._2
    println(res)
    res
  }

//  runFoldMapV()
//  runOrdered()
  runWcMonoid()
  countUsingWCMonoid("paul is great and amazing and we love him")
  checkBagWorks()
  calcMean(List(1,2,3))
}
