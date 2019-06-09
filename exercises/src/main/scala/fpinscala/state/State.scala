package fpinscala.state

import scala.Option
import scala.Some

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Math.abs(Int.MinValue) -> Int.minValue which is -ve!!
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, gen) = rng.nextInt
    (math.abs(1 + x), gen)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (x, gen) = nonNegativeInt(rng)
    (x.toDouble / Int.MaxValue, gen)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (myInt, gen) = rng.nextInt
    val (myDouble, gen2) = double(gen)
    ((myInt, myDouble), gen2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((myInt, myDouble), gen) = intDouble(rng)
    ((myDouble, myInt), gen)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  // it's a bit naughty to define this here, should be a listy boi
  def unfoldRight[A,S](z: S)(f: S => Option[(A,S)]): List[A] =
    f(z) match {
      case Some((x, s)) => x :: unfoldRight(s)(f)
      case None => Nil
    }

  // this is bonkers and also intellij gets mad for no reason
  // but it works at least!
  def intsViaUnfoldRight(count: Int)(rng: RNG): (List[Int], RNG) =
    (unfoldRight((rng, count))({
      case (_, 0) => None
      case (_rng, cnt) => {
        val (x, rngNext) = _rng.nextInt
        Some((x, (rngNext, cnt - 1)))
      }
    }), rng)

  // let's try to be tail recursive af (remember to decrement the count)
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(i: Int, lst: List[Int], gen: RNG): (List[Int], RNG) = {
      if (i == 0) (lst, gen)
      else {
        val (x, gen2) = gen.nextInt
        go(i - 1, x :: lst, gen2)
      }
    }
    go(count, Nil, rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
