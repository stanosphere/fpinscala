package semigroup

import cats._
import cats.implicits._

object OhLookASemiGroup extends App {
  def basicCombine(): Int = Semigroup[Int].combine(1, 2)
  def combineLists[A](xs: List[A], ys: List[A]): List[A] =
    Semigroup[List[A]].combine(xs, ys)

  def main(): Unit = {
    println(basicCombine())
  }
}

