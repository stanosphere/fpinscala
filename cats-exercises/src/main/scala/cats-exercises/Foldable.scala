package foldable

import cats.{Foldable, Later, Eval, Monoid, MonoidK}

case class Complex(re: Double, im: Double) {
  def +(y: Complex): Complex = Complex(re + y.re, im + y.im)
  def -(y: Complex): Complex = Complex(re - y.re, im - y.im)
  def *(y: Complex): Complex = Complex(
    re * y.re - im * y.im,
    re * y.im + im * y.re
  )
  def conjugate(): Complex = Complex(re, -im)
  def mag(): Double = Math.hypot(re, im)

  implicit def complexMonoid: Monoid[Complex] =
    new Monoid[Complex] {
      def combine(x: Complex, y: Complex): Complex = x * y
      def empty: Complex = Complex(0.0, 0.0)
    }
}

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

    implicit def intMonoid: Monoid[Int] =
      new Monoid[Int] {
        def combine(x: Int, y: Int): Int = x * y
        def empty: Int = 1
      }

    val res = f.foldMap(numbers)(_.toString)
    println(res)

    val factsAreFactsAmerica = f.foldMap(numbers)(x => x)
    println(factsAreFactsAmerica)
  }

  def foldKDemo(): Unit = {
    val listOfLists: List[List[Int]] = List(
      List(1, 2),
      List(3, 4, 5)
    )

    val listOfOptions = List(None, Option("two"), Option("three"))

    implicit def m: MonoidK[List] =
      new MonoidK[List] {
        def combineK[A](x: List[A], y: List[A]): List[A] = x ++ y
        def empty[A] = Nil
      }

    implicit def o: MonoidK[Option] =
      new MonoidK[Option] {
        def combineK[A](x: Option[A], y: Option[A]): Option[A] =
          (x, y) match {
            case (Some(xx), _) => Some(xx)
            case (None, Some(yy)) => Some(yy)
            case _ => None
          }
        def empty[A] = None
      }

    println(f.foldK(listOfLists))
    println(f.foldK(listOfOptions))

  }

  def demoFoldKOption(): Unit = {
    import cats.implicits._
    val listOfOptions = List(None, Option("two"), Option("three"))
    println(f.foldK(listOfOptions))
  }

  def traverseDemo(): Unit = {
    import cats.implicits._

    def parseInt(s: String): Option[Int] =
      Either.catchOnly[NumberFormatException](s.toInt).toOption

    println(f.traverse_(List("1", "2", "3"))(parseInt))
    println(f.traverse_(List("a", "b", "c"))(parseInt))
  }

  def demoComposibility(): Unit = {
    import cats.implicits._
    val g = f.compose[Option]


  }

  def main(): Unit = {
    demoFoldLeft()
    demoFoldRight()
    foldMapDemo()
    foldKDemo()
    demoFoldKOption()
    traverseDemo()
  }

  main()
}

