package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (TestCases, RNG) => Result) {
  /* runs the current Prop and then the next one if the first one is cool */
  // no need to add labels to this one since if the first fails the second doesn't even run
  def &&(p : Prop): Prop = Prop((testCases, rng) => {
    run(testCases, rng) match {
      case Passed => p.run(testCases, rng)
      case Proved => p.run(testCases, rng)
      case x: Falsified => x
    }
  })

  def ||(p : Prop): Prop = Prop((testCases, rng) => {
    run(testCases, rng) match {
      case Falsified(failureMessageInFirstExecution,_) => p
        .markAsFailed(failureMessageInFirstExecution)
        .run(testCases, rng)
      case Passed => Passed
      case Proved => Proved
    }
  })

  def markAsFailed(newErrorMessage: String) = Prop((testCases, rng) => {
    run(testCases, rng) match {
      case Falsified(currentError, successes) =>
        Falsified(s"$newErrorMessage\n$currentError", successes)
      case Passed => Passed
      case Proved => Proved
    }
  })
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(r => Some(g.sample.run(r)))

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (n,rng) => f(n,rng) }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng)
      .zip(Stream.from(0))
      .take(n)
      .map { case (a, i) =>
        try { if (f(a)) Passed else Falsified(a.toString, i) }
        catch { case exc: Exception => Falsified(buildMsg(a, exc), i) }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"""
      |test case: $s
      |generated an exception: ${e.getMessage}
      |stack trace:
      |${e.getStackTrace.mkString("\n")}
    """.stripMargin
}

trait GenTrait[A] {
  def map[B](f: A => B): Gen[B]
  def flatMap[B](f: A => Gen[B]): Gen[B]
}

case class Gen[A](sample: State[RNG,A]) extends GenTrait[A] {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B,C](gb: Gen[B])(f: (A,B) => C) =
    Gen(sample.map2(gb.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  // this is better because we can feed it a number we don't yet know the value of
  // it means we can add an extra degree of randomness with ease
  // i.e. this lets us generate a list of random length whereas with the object method we could only make a list of fixed length
  // but this function can in fact generate a list of fixed length
  // so it is a more general version of the previous implementation
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  def toGenOption: Gen[Option[A]] =
    Gen(sample.map(Some(_)))
}

object Gen {
  def unit[A](a: => A): Gen[A] =
    Gen(State(s => (a, s)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.nonNegativeInt).map(n => if (n % 2 == 0) true else false))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def chooseTwoInts(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    choose(start, stopExclusive).map2(choose(start, stopExclusive))((x,y) => (x,y))

  private def listOfNumbersToString(xs: Gen[List[Int]]): Gen[String] =
    xs.map(_.map(_.toChar).mkString)

  // this will give you all sorts of characters!
  def chooseString(length: Int): Gen[String] =
    listOfNumbersToString(listOfN(length, choose(1,1000)))

  /* get an alphanumeric character with equal probability */
  // note that this is aweful if you want more than about 2000 characters
  def chooseAlphaNumericString(length: Int): Gen[String] = {
    // https://theasciicode.com.ar/
    val numbers = choose(48, 58)
    val lowerCase = choose(65, 91)
    val upperCase = choose(97, 123)
    val choice = generalisedWeighted(List(
      (numbers, 10),
      (lowerCase, 26),
      (upperCase, 26),
    ))

    listOfNumbersToString(listOfN(length, choice))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap({
      case true => g1
      case false => g2
    })

  private case class IntervalAccumulator(intervalList: List[(Int, Int)], runningTotal: Int)

  private def calculateIntervalList(xs: List[Int]): IntervalAccumulator =
    xs.foldLeft(IntervalAccumulator(Nil, 0))(
      (acc, x) => {
        val IntervalAccumulator(intervalList, runningTotal) = acc
        val nextTotal = runningTotal + x
        IntervalAccumulator(intervalList ++ List((runningTotal, nextTotal)), nextTotal)
      }
    )

  def generalisedWeighted[A](gs: List[(Gen[A], Int)]): Gen[A] = {
    val weights = gs.map(_._2)
    val IntervalAccumulator(intervals, total) = calculateIntervalList(weights)
    val zipped = intervals zip gs
    val choice = choose(0, total)
    val isBetween = (x: Int) => (interval: (Int, Int)) =>
      x >= interval._1 && x < interval._2

    choice flatMap (x =>
      zipped
      .find(intervalAndGen => isBetween(x)(intervalAndGen._1))
      .get._2._1
    )
  }

  def weighted[A](g1: (Gen[A], Int), g2: (Gen[A], Int)): Gen[A] = {
    val (x, y) = (g1._2, g2._2)
    val (gx, gy) = (g1._1, g2._1)
    val total = x + y
    choose(0, total + 1) flatMap (num => if (num < x) gx else gy)
  }


}

trait SGen[+A] {

}

