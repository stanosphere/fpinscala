package adjunction

import cats.Functor

abstract class Adjunction[F[_] : Functor, G[_] : Functor] {

  def leftAdjoint[A, B](f: F[A] => B): A => G[B]

  def rightAdjoint[A, B](f: A => G[B]): F[A] => B

}

class AdjunctionLaws[F[_] : Functor, G[_] : Functor](adj: Adjunction[F, G]) {
  def leftIdentity[A, B](testFunction: A => G[B], testValue: A): Boolean = {
    // this needs to be the identity function
    val id: (A => G[B]) => A => G[B] = adj.leftAdjoint[A, B] compose adj.rightAdjoint[A, B]

    testFunction(testValue) == id(testFunction)(testValue)
  }

  def rightIdentity[A, B](testFunction: F[A] => B, testValue: F[A]): Boolean = {
    // this needs to be the identity function
    val id: (F[A] => B) => F[A] => B = adj.rightAdjoint[A, B] compose adj.leftAdjoint[A, B]

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
    override def leftAdjoint[A, B](f: ((A, R)) => B): A => R => B =
      a => r => f((a, r))

    // uncurry
    override def rightAdjoint[A, B](f: A => R => B): ((A, R)) => B = {
      case (a, r) => f(a)(r)
    }
  }
}
