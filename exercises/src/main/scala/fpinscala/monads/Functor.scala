package fpinscala.monads

import fpinscala.testing._
import Prop._

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def coDistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

// I think these laws have to specify what F and A are when you run them
// Otherwise how would you generate what the functor contains?
// like imagine checking a list is a functor
// it would be impossible to generate test cases unless you specify it's a list of ints
// or a list of booleans or whatever
// which I think is kind of unfortunate

// however it appears that scala check has the ability to generate arbitrary types which is kinda cool;
// need to double check this though!
object FunctorLaws extends App {
  def identity[F[_],A](f: Functor[F[A]], gen: Gen[F[A]]): Prop =
    forAll(gen)((functorInstance: F[A]) => f.map(functorInstance)(a => a) == functorInstance)

  def composition[F[_],A,B,C](f: Functor[F[A]], gen: Gen[F[A]], genFab: Gen[A => B], genFbc: Gen[B => C]): Prop = {

  }

  def allLaws[A](f: Functor[A], gen: Gen[A]): Prop = ???
}
