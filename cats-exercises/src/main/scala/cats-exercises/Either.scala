package either

import cats.implicits._

object EitherStyle {
  private def parse(s: String): Either[NumberFormatException, Int] =
    if (s.matches("-?[0-9]+")) Either.right(s.toInt)
    else Either.left(new NumberFormatException(s"${s} is not a valid integer."))

  private def reciprocal(i: Int): Either[IllegalArgumentException, Double] =
    if (i == 0) Either.left(new IllegalArgumentException("Cannot take reciprocal of 0."))
    else Either.right(1.0 / i)

  private def stringify(d: Double): String = d.toString

  def magic(s: String): Either[Exception, String] =
    parse(s)
      .flatMap(reciprocal)
      .map(stringify)
}

object EitherStyleWithAdts {
  sealed abstract class Error
  final case class NotANumber(string: String) extends Error
  final case object NoZeroReciprocal extends Error

  private def parse(s: String): Either[Error, Int] =
    if (s.matches("-?[0-9]+")) Either.right(s.toInt)
    else Either.left(NotANumber(s))

  private def reciprocal(i: Int): Either[Error, Double] =
    if (i == 0) Either.left(NoZeroReciprocal)
    else Either.right(1.0 / i)

  private def stringify(d: Double): String = d.toString

  def magic(s: String): Either[Error, String] =
    parse(s).flatMap(reciprocal).map(stringify)
}

object EitherIsBae extends App {
  def basicBasics(): Unit = {
    val right: Either[String, Int] = Either.right(5)
    println(right.map(_ + 1))

    val left: Either[String, Int] = Either.left("Something went wrong")
    println(left.map(_ + 1))
  }

  def demoBiasedFlatMap(): Unit = {
    val ff = (x: Int) => Either.right(x + 1)

    val right: Either[String, Int] = Either.right(5)
    println(right.flatMap(ff))

    val left: Either[String, Int] = Either.left("Something went wrong")
    println(left.flatMap(ff))
  }

  def demoMagic(): Unit = {
    import EitherStyle._
    println(magic("0"))
    println(magic("1"))
    println(magic("Not a number"))
    println(magic("1.22"))
    println(magic("137"))
  }

  def demoPatternMatchingWithMagic(): Unit = {
    import EitherStyle._
    val dealWithMagic = (x: String) => magic(x) match {
      case Left(_: NumberFormatException) ⇒ "Not a number!"
      case Left(_: IllegalArgumentException) ⇒ "Can't take reciprocal of 0!"
      case Left(_) ⇒ "Unknown error"
      case Right(result) ⇒ s"Got reciprocal: ${result}"
    }

    println(dealWithMagic("0"))
    println(dealWithMagic("1"))
    println(dealWithMagic("Not a number"))
    println(dealWithMagic("1.22"))
    println(dealWithMagic("137"))
  }

  def main(): Unit = {
    basicBasics()
    demoBiasedFlatMap()
    demoMagic()
    demoPatternMatchingWithMagic()

    val left: Either[String, Int] = Left("Hello")

    println(left.leftMap(_.reverse))
  }

  main()
}
