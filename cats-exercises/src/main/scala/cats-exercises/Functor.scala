package functor

import cats.Functor
import cats.implicits._

object OhLookSomeFunctors extends App {

  def fProductExample(): Unit = {
    val source = List("Cats", "is", "awesome")
    val product = Functor[List].fproduct(source)(_.length)

    println(product)
    println(product.toMap)
    println(product.toMap.get("Cats"))
  }

  def functorCompositionExample(): Unit = {
    val listOpt = Functor[List] compose Functor[Option]

    val ints = List(Some(1), None, Some(3))
    println(listOpt.map(ints)(_ + 1))

    val strs = List(Some("Hi"), None, Some("stano"))
    println(listOpt.map(strs)(_.toUpperCase))
  }

  def main(): Unit = {
    fProductExample()
    functorCompositionExample()
  }

  main()
}

