package either

import cats.implicits._

object EitherIsBae extends App {
  def basicBasics(): Unit = {
    val right: Either[String, Int] = Either.right(5)
    println(right.map(_ + 1))

    val left: Either[String, Int] = Either.left("Something went wrong")
    println(left.map(_ + 1))
  }

  def main(): Unit = {
    basicBasics()
  }

  main()
}
