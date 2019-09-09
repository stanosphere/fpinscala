package fpinscala.monoids

import MonoidLaws.allLaws
import fpinscala.monoids.OrderTwoMonoids.{A, E, TwoThings}
import fpinscala.testing.{Gen, Prop}

// I basically just want to figure out if there are one or two of these
// Are all order 2 monoids isomorphic to one another?
object OrderTwoMonoids {
  trait TwoThings
  case class E() extends TwoThings
  case class A() extends TwoThings

  val groupMonoid: Monoid[TwoThings] = new Monoid[TwoThings] {
    def op(x: TwoThings, y: TwoThings): TwoThings = (x,y) match {
      case (E(), a) => a
      case (a, E()) => a
      case (A(), A()) => E()
    }
    val zero: TwoThings = E()
  }

  // the real question is do the monoid laws hold for this structure?
  // on paper it looks as though they do
  // this was actually fairly simple to check as the first 2 cases
  // for `op` follow directly from the identity law
  // and the final case does not involve the identity whatsoever
  // so you just need to check all 8 cases for associativity
  // but I'm also gonna use my property based testing to check
  val nonGroupMonoid: Monoid[TwoThings] = new Monoid[TwoThings] {
    def op(x: TwoThings, y: TwoThings): TwoThings = (x,y) match {
      case (E(), a) => a
      case (a, E()) => a
      case (A(), A()) => A()
    }
    val zero: TwoThings = E()
  }
}

object VerifyMonoids extends App {
  val gen: Gen[TwoThings] = Gen.weighted(
    Gen.unit(E()) -> 50,
    Gen.unit(A()) -> 50,
  )

  def check(): Unit = {
    Prop.run(allLaws(OrderTwoMonoids.groupMonoid, gen))
    Prop.run(allLaws(OrderTwoMonoids.nonGroupMonoid, gen))
  }

  check()
}

// I wonder how many order 3 monoids there are
