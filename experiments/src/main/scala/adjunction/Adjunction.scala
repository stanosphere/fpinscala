package adjunction

import cats.{Functor, Monad}
import cats.implicits._

abstract class Adjunction[F[_] : Functor, G[_] : Functor] {

  def leftAdjunct[A, B](f: F[A] => B): A => G[B]

  def rightAdjunct[A, B](f: A => G[B]): F[A] => B

  def unit[A](a: A): G[F[A]] =
    leftAdjunct(identity[F[A]])(a)

  def counit[A](fga: F[G[A]]): A =
    rightAdjunct(identity[G[A]])(fga)

  type M[A] = G[F[A]]

  def _unit[A](a: A): G[F[A]] = unit[A](a)

  implicit val functor: Functor[M] = new Functor[M] {
    override def map[A, B](gfa: G[F[A]])(f: A => B): G[F[B]] =
      gfa.map(_.map(f))
  }

  // from an adjunction one can ALWAYS derive a corresponding Monad and CoMonad Pair
  val monad: Monad[M] = new Monad[M] {
    override def pure[A](a: A): M[A] = _unit(a)

    override def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
      Functor[G].map(Functor[M].map(ma)(f))(counit)

    override def tailRecM[A, B](a: A)(f: A => M[Either[A, B]]): M[B] = ???
  }


}

class AdjunctionLaws[F[_] : Functor, G[_] : Functor](adj: Adjunction[F, G]) {
  def leftIdentity[A, B](testFunction: A => G[B], testValue: A): Boolean = {
    // this needs to be the identity function
    val id: (A => G[B]) => A => G[B] = adj.leftAdjunct[A, B] compose adj.rightAdjunct[A, B]

    testFunction(testValue) == id(testFunction)(testValue)
  }

  def rightIdentity[A, B](testFunction: F[A] => B, testValue: F[A]): Boolean = {
    // this needs to be the identity function
    val id: (F[A] => B) => F[A] => B = adj.rightAdjunct[A, B] compose adj.leftAdjunct[A, B]

    testFunction(testValue) == id(testFunction)(testValue)
  }
}

object AdjunctionInstances {

  implicit def writerFunctor[R]: Functor[(*, R)] = new Functor[(*, R)] {
    override def map[A, B](fa: (A, R))(f: A => B): (B, R) = fa match {
      case (a, r) => (f(a), r)
    }
  }

  implicit def readerFunctor[R]: Functor[R => *] = new Functor[R => *] {
    override def map[A, B](f1: R => A)(f2: A => B): R => B = r => f2(f1(r))
  }

  def writerReaderAdjunction[R]: Adjunction[(*, R), R => *] = new Adjunction[(*, R), R => *] {
    // curry
    override def leftAdjunct[A, B](f: ((A, R)) => B): A => R => B =
      a => r => f((a, r))

    // uncurry
    override def rightAdjunct[A, B](f: A => R => B): ((A, R)) => B = {
      case (a, r) => f(a)(r)
    }
  }
}
