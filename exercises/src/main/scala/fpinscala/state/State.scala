package fpinscala.state

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

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
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

  val doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(x => x.toDouble / Int.MaxValue)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (myInt, gen) = rng.nextInt
    val (myDouble, gen2) = double(gen)
    ((myInt, myDouble), gen2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((myInt, myDouble), gen) = intDouble(rng)
    ((myDouble, myInt), gen)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  // it's a bit naughty to define this here, should be a listy boi
  def unfoldRight[A, S](z: S)(f: S => Option[(A, S)]): List[A] =
    f(z) match {
      case Some((x, s)) => x :: unfoldRight(s)(f)
      case None => Nil
    }

  // this is bonkers and also intellij gets mad for no reason
  // but it works at least!
  def intsViaUnfoldRight(count: Int)(rng: RNG): (List[Int], RNG) =
    (unfoldRight((rng, count))({
      case (_, 0) => None
      case (_rng, cnt) =>
        val (x, rngNext) = _rng.nextInt
        Some((x, (rngNext, cnt - 1)))
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

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng0 => {
      val (a, rng1) = ra(rng0)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  //   type Rand[+A] = RNG => (A, RNG)
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng0 => fs.foldRight((Nil: List[A], rng0))((randX, acc) => {
      val (lst, rng1) = acc
      val (h, rng2) = randX(rng1)
      (h :: lst, rng2)
    })

  // this is L33T Af
  // it's kind of more polymorphic than mine
  // I'd say the major difference is that they put the desired return type as the zero of the fold
  // whereas I just kind of artificially wrap stuff in a function lol
  def sequenceAns[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil: List[A]))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def mapViaFlatMap[A, B](f: Rand[A])(g: A => B): Rand[B] =
    flatMap(f)(x => unit(g(x)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(g: (A, B) => C): Rand[C] =
    flatMap(ra)(x => map(rb)(y => g(x, y)))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(x =>
      if (x < n) unit(x)
      else {
        val y = x.toDouble * n.toDouble / Int.MaxValue.toDouble
        unit(y.toInt)
      }
    )

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

  def rollDieNTimes(n: Int): Rand[List[Int]] =
    sequence(List.fill(n)(rollDie))
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def map3[B, C, D](sb: State[S, B], sc: State[S, C])(f: (A, B, C) => D): State[S, D] =
    flatMap(a => sb.flatMap(b => sc.map(c => f(a, b, c))))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State((s: S) => (a, s))

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] = {
    val init = unit(Nil): State[S, List[A]]
    states.foldRight(init)(
      (state, acc) => state.map2(acc)(_ :: _)
    )
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def updateCandyMachine = (machineState: Machine, input: Input) =>
    (input, machineState) match {
      case (_, Machine(_, 0, _)) => machineState // no candy so nothing happens
      case (Coin, Machine(false, _, _)) => machineState // unlocked so putting a coin in does nothing
      case (Turn, Machine(true, _, _)) => machineState // can't turn a locked machine
      case (Turn, Machine(false, candies, coins)) =>
        Machine(true, candies - 1, coins) // turning an unlocked machine releases a candy and relocks the machine
      case (Coin, Machine(true, candies, coins)) =>
        Machine(false, candies, coins + 1) // inserting a coin into an unlocked machine
    }

  // whilst this implementation is correct it might be wise to try using only the
  // functions that we have been so carefully defining thus far
  // for example feel like `sequence` would be useful here
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State((machine: Machine) => {
      val finalState = inputs.foldLeft(machine)(updateCandyMachine)
      val Machine(_, candies, coins) = finalState
      ((coins, candies), finalState)
    })
}



