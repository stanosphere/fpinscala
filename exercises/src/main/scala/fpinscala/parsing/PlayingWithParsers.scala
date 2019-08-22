//package fpinscala.parsing
//
//import ParserType._
//
//import language.higherKinds
//
//object ParserType {
//  // an outrageously simple implementation lol
//  type Parser[+A] = String => Either[ParseError, A]
//
//  // this will contain the actual logic of parsing I think
////  val aParser: Parser[Int] = (input: String) =>
////    input.filter(_ == 'a') match {
////      case "" => Left(ParseError())
////      case s => Right(s.length)
////    }
//}
//
//object Parser extends Parsers[Parser] {
//  def run[A](p: Parser[A])(input: String): Either[ParseError,A] = {
//    println(input)
//    println(p)
//    p(input)
//  }
//
//  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A] = input =>
//    s1(input) match {
//      case right: Right[_, A] => right
//      case _: Left[ParseError, _] => s2(input)
//    }
//
//  def map[A,B](pa: Parser[A])(f: A => B): Parser[B] =
//    input => pa(input).map(f)
//
//  def many[A](p: Parser[A]): Parser[List[A]] =
//    listOfN(1, p)
//
//  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
//    map(p)(List.fill(n)(_))
//
//  override implicit def char(c: Char): Parser[Char] = input => {
//    Right(input.filter(_ == c))
//  }
//}
//
//object PlayingWithParsers extends App {
//  def booksParser(): Unit = {
//    import fpinscala.parsing.Parser._
//    val numA: Parser[Int] = char('a').many.map(_.size)
//    val parsed = run(numA)("paul")
//    println("Res 1",  parsed)
//
//    val numI: Parser[Int] = char('i').many.map(_.size)
//    val parsed2 = run(numI)("isshin ashina, the sword saint")
//    println("Res 2", parsed2)
//  }
//
//  def main(): Unit = {
//    booksParser()
//  }
//
//  main()
//}
