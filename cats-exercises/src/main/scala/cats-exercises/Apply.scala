package apply

import cats.Apply
import cats.implicits._

// this seems to take a wrapped function
// and a wrapped value
// and return a new wrapped value
// where the wrapper is always the same type

object OhLookSomeApplies extends App {
  def exampleOfExtendingFunctor(): Unit = {
    println(Apply[Option].map(Some(1))(_ + 1))
  }

  def composableApplies(): Unit = {
    val listOpt = Apply[List] compose Apply[Option]
    val plus = (x: Int) => (y: Int) ⇒ x + y

    val optInts = List(Some(1), None, Some(3))
    val wrappedFunctions = List(Some(plus(1)), None, Some(plus(2)))

    val res = listOpt.ap(wrappedFunctions)(optInts)
    // so what is this gonna do?
    // it'll apply plus(1) to all the values
    // and then do nothing to all the values
    // and then plus(2) to all the values
    // kind of like a cartessian product or some shit
    println(res)
  }

  val addArity2 = (a: Int, b: Int) ⇒ a + b
  val addArity3 = (a: Int, b: Int, c: Int) ⇒ a + b + c

  def multipleArityAp(): Unit = {
    val three = Apply[Option].ap2(Some(addArity2))(Some(1), Some(2))
    val none = Apply[Option].ap2(Some(addArity2))(Some(1), None)
    println(three)
    println(none)

    val six = Apply[Option].ap3(Some(addArity3))(Some(1), Some(2), Some(3))
    println(six)
  }

  def tupleDemo(): Unit = {
    val twoPle = Apply[Option].tuple2(Some(1), Some(2))
    println(twoPle)
  }

  def variadicShit(): Unit = {
    val option2 = Option(1) |@| Option(2)
    val option3 = option2 |@| Option.empty[Int]

    println(option2 map addArity2)
    println(option3 map addArity3)

    println(option2 apWith Some(addArity2))
    println(option3 apWith Some(addArity3))

    println(option2.tupled)
    println(option3.tupled)

    val a = List(1)
    val b = List(2)

    println((a|@|b) map addArity2)
    val x = a|@|b
  }

  def main(): Unit = {
    exampleOfExtendingFunctor()
    composableApplies()
    multipleArityAp()
    tupleDemo()
    variadicShit()
  }

  main()
}

