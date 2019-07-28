package traverse

import cats.data.{Validated, ValidatedNel}
import cats.implicits._

// so In Hindley-Milner world I think the signature is something like:
// traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
// but we can rewrite tjis in scala form as:
// traverse :: (Traversable t, Applicative f) => t a ~> (a -> f b) -> f (t b)

object TheGoodTraverse extends App {

  def demoBasicUsage(): Unit = {

    def parseIntEither(s: String): Either[NumberFormatException, Int] =
      Either.catchOnly[NumberFormatException](s.toInt)

    def parseIntValidated(s: String): ValidatedNel[NumberFormatException, Int] =
      Validated.catchOnly[NumberFormatException](s.toInt).toValidatedNel

    // In this example:
    // t a ~> (a -> f b) -> f (t b) becomes:
    // List[String] ~> (String -> Either[Error, Int]) -> Either[Error, List[Int]]

    val res = List("1", "2", "3").traverse(parseIntEither)
    val naughtyRes = List("1", "I am naughty", "3").traverse(parseIntEither)
    println(res)
    println(naughtyRes)
  }

  def map2[A,B,C] (xs: List[A], ys: List[B])(f: (A,B) => C): List[C]=
    xs.flatMap(x => ys.map(y => f(x, y)))

  def traverseListList[A, B](as: List[A])(f: A => List[B]): List[List[B]] = {
    val z = List(List.empty[B])
    val reducer = (a: A, acc: List[List[B]]) => map2(f(a), acc)(_ :: _)
    val red =     (a: A, acc: List[List[B]]) => f(a).flatMap(x => acc.map(y => x :: y))
    as.foldRight(z)(reducer)
  }


  def fAndTAreBothLists(): Unit = {
    // In this example:
    // t a ~> (a -> f b) -> f (t b) becomes:
    // List[A] ~> (A -> List[B]) -> List[List[B]]

    val xs: List[Int] = List(1, 2, 3)
    val f: Int => List[Int] = x => List(x, x * x)

    // we now have:
    // List[Int] ~> (Int -> List[String]) -> List[List[String]]

    val yss = xs.traverse(f)
    val myRes = traverseListList(xs)(f)
    println(yss)
    println(myRes)
  }

  def main(): Unit = {
    demoBasicUsage()
    fAndTAreBothLists()
  }

  main()
}

