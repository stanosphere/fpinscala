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

trait Prop {
  def check: Boolean
  def &&(p: Prop): Prop = new Prop {
    def check: Boolean = Prop.this.check && p.check
  }
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
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
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

  // this will give you all sorts of characters!
  def chooseString(length: Int): Gen[String] =
    listOfN(length, choose(1,1000))
      .map(_.map(_.toChar).mkString)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap({
      case true => g1
      case false => g2
    })

  def weighted[A](g1: (Gen[A], Int), g2: (Gen[A], Int)): Gen[A] = {
    val (x, y) = (g1._2, g2._2)
    val (gx, gy) = (g1._1, g2._1)
    val total = x + y
    choose(0, total + 1) flatMap (num => if (num < x) gx else gy)
  }


}

trait SGen[+A] {

}

