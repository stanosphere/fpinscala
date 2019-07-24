package foldable

import cats._

object ForAFewFoldablesMore extends App {

  val numbers = List(1,2,3,4)
  val letters = List("a", "b", "c", "d")
  val f: Foldable[List] = {
    import cats.implicits._
    Foldable[List]
  }

  def demoFoldLeft(): Unit = {
    println(f.foldLeft(numbers, 0)(_+_))
    println(f.foldLeft(letters, "")(_+_))
  }

  def demoFoldRight(): Unit = {
    // this is lazy and right-associative
    // it has a bit of a different signature to foldLeft
    // so as to support laziness in a stack safe way

    // Eval is a cats thing too

    val folder = (x: Int, y: Eval[Int]) => {
      println("This should not appear unless value is called")
      println(x)
      Later(x + y.value)
    }

    val lazyResult = f.foldRight(numbers, Eval.Zero)(folder)
    println(lazyResult)
  }

  def foldMapDemo(): Unit = {

    implicit def stringMonoid: Monoid[String] =
      new Monoid[String] {
        def combine(x: String, y: String): String = x ++ " <-> " ++ y
        def empty: String = "EMPTY"
      }

    val res = f.foldMap(numbers)(_.toString)
    println(res)
  }

  def main(): Unit = {
    demoFoldLeft()
    demoFoldRight()
    foldMapDemo()
  }

  main()
}

