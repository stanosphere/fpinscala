package fpinscala.parsing

import AParserType._
import language.higherKinds

object AParserType {
  type Parser[+A] = String => Either[ParseError, A]
  val aParser: Parser[Int] = (input: String) => {
    if (input.length == 0) Left(ParseError())
    else Right(1)
  }
}

object AParser extends Parsers[Parser] {
  def run[A](p: Parser[A])(input: String): Either[ParseError,A] =
    p(input)

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A] = input => {
    val res1 = s1(input)
    res1 match {
      case right: Right[_, A] => right
      case _: Left[ParseError, _] => s2(input)
    }
  }
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = ???
}

object PlayingWithParsers extends App {
  def main(): Unit = {
    println(12)
  }

  main()
}
