package semigroup

import cats.kernel.Semigroup
import cats.implicits._

object OhLookASemiGroup extends App {
  def basicCombine(): Int = Semigroup[Int].combine(1, 2)

  def combineLists[A](xs: List[A], ys: List[A]): List[A] =
    Semigroup[List[A]].combine(xs, ys)

  // looks like it applies both functions and then combines the result with addition
  def something(f: Int => Int, g: Int => Int): Int => Int =
    Semigroup[Int => Int].combine(f, g)

  def peak[A,B](f: A => B): A => B =
    (x: A) => {
      val res = f(x)
      println(x)
      println(res)
      res
    }

  def combiningMaps(): Unit = {
    type EmptyMap = Map[String, Int]

    val x = Map("foo" -> Map("bar" -> 5))
    val y = Map("foo" -> Map("bar" -> 6), "baz" -> (Map(): EmptyMap))
    println(x combine y)
    println(x ++ y)

    val a = Map("foo" -> List(1, 2))
    val b = Map("foo" -> List(3, 4), "bar" -> List(42))
    println(a combine b)
    println(a ++ b)
  }

  def usingNiceSyntax(): Unit = {
    val one: Option[Int] = Option(1)
    val two: Option[Int] = Option(2)
    val n: Option[Int] = None

    println(one |+| two)
    println(n |+| two)
    println(n |+| n)
    println(two |+| n)
    println(one |+| one |+| one)
  }

  def main(): Unit = {
    println(basicCombine())
    println(combineLists(List(1,2,3), List(4,5,6)))
    println(something(peak(_ + 1), peak(_ * 10))(6))
    combiningMaps()
    usingNiceSyntax()
  }

  main()
}

