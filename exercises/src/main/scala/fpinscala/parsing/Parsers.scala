package fpinscala.parsing

import language.higherKinds
import fpinscala.testing._

import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>
  /**
    * primitives
    * for now we have 6
    * */
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def slice[A](p: Parser[A]): Parser[String]
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]

  /** derived combinators */
  def map[A,B](pa: Parser[A])(f: A => B): Parser[B] =
    pa.flatMap(f andThen succeed)

  // this versions is a mess
  // def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
  //   p1.flatMap(a => p2.map(b => f(a,b)))

  // this feels cleaner
  def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    for { a <- p1; b <- p2 } yield f(a,b)

  // this looks very very much like unit
  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  // this one is more of a convenience method than anything else
  implicit def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  // keep appending a thing to the list until the parser fails
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(Nil)

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    map2(p, p2)((_,_))

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  // looks roughly like a traverse???
  def listOfN[A]( n: Int, p: Parser[A]): Parser[List[A]] =
    map2(p, succeed(n))((pp, nn) => List.fill(nn)(pp))

  /** allow infix syntax */
  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def **[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p,p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def slice: Parser[String] = self.slice(p)
  }

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = ParserOps(f(a))

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(
  stack: List[(Location,String)] = List(),
  otherFailures: List[ParseError] = List()
) {

}