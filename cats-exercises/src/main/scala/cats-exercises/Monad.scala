package monad

import cats.Monad
import cats.implicits._

// a monad extends an applicative
// an applicative has something called `pure`
// pure is used to wrap a thin g in an Applicative
// eg List.pure(1) -> List(1)

object AFistFullOfMonads extends App {
  def demoFlatten(): Unit = {
    val flattenedOption = Option(Option(1)).flatten
    println(flattenedOption)

    val flattenedOption2 = Option(None).flatten
    println(flattenedOption2)

    val flatList = List(List(1), List(2, 3)).flatten
    println(flatList)
  }

  // flatMap === map -> flatten
  // flatten === flatMap(_)(x => x)

  def flatMapDemo(): Unit = {
    val list = List(1, 2, 3)
    val toDouble = (x: Int) => List(x, x)

    println(Monad[List].flatMap(list)(toDouble))
  }

  //  def ifM[B](fa: F[Boolean])(ifTrue: => F[B], ifFalse: => F[B]): F[B] =
  //    flatMap(fa)(if (_) ifTrue else ifFalse)

  def ifMDemo(): Unit = {
    println(Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy")))
    println(Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)))
  }

  def main(): Unit = {
    demoFlatten()
    flatMapDemo()
    ifMDemo()
  }

  main()
}

