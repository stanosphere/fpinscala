package adjunction

import cats.{Functor, Monad}
import cats.implicits._

abstract class Adjunction[F[_] : Functor, G[_] : Functor] {

  // TODO write in terms of unit and counit
  def leftAdjunct[A, B](f: F[A] => B): A => G[B]

  // TODO write in terms of unit and counit
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
    override def pure[A](a: A): M[A] =
      _unit(a)

    override def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = {
      val mmb = ma.map(f)
      Functor[G].map(mmb)(counit)
    }

    // there might be a way to ACTUALLY make this tail recursive using the adjunctions? But I somewhat doubt it
    override def tailRecM[A, B](a: A)(f: A => M[Either[A, B]]): M[B] =
      flatMap(f(a))(_.fold(tailRecM(_)(f), pure))
  }

  // TODO write definition for comonad
}

class AdjunctionLaws[F[_] : Functor, G[_] : Functor](adj: Adjunction[F, G]) {
  def leftIdentity[A, B](testFunction: A => G[B], testValue: A): Boolean = {
    // this needs to be the identity function
    val id: (A => G[B]) => A => G[B] = adj.leftAdjunct[A, B] _ compose adj.rightAdjunct[A, B]

    testFunction(testValue) == id(testFunction)(testValue)
  }

  def rightIdentity[A, B](testFunction: F[A] => B, testValue: F[A]): Boolean = {
    // this needs to be the identity function
    val id: (F[A] => B) => F[A] => B = adj.rightAdjunct[A, B] _ compose adj.leftAdjunct[A, B]

    testFunction(testValue) == id(testFunction)(testValue)
  }

  // TODO write down laws in terms of unit and counit (I imagine this can be done via algebraic manipulation actually)
}

object AdjunctionInstances {

  implicit def writerFunctor[W]: Functor[(*, W)] = new Functor[(*, W)] {
    override def map[A, B](fa: (A, W))(f: A => B): (B, W) = fa match {
      case (a, w) => (f(a), w)
    }
  }

  implicit def readerFunctor[R]: Functor[R => *] = new Functor[R => *] {
    override def map[A, B](f1: R => A)(f2: A => B): R => B = r => f2(f1(r))
  }

  def writerReaderAdjunction[S]: Adjunction[(*, S), S => *] = new Adjunction[(*, S), S => *] {
    // curry
    override def leftAdjunct[A, B](f: ((A, S)) => B): A => S => B =
      a => r => f((a, r))

    // uncurry
    override def rightAdjunct[A, B](f: A => S => B): ((A, S)) => B = {
      case (a, r) => f(a)(r)
    }
  }

  type State[S, A] = S => (A, S)

  // I would have thought this would simplify to R => (*, R)???
  def stateMonad[S]: Monad[Lambda[x => S => (x, S)]] = writerReaderAdjunction[S].monad

}

object Go extends App {
  val m = AdjunctionInstances.stateMonad[Int].pure("hello")

  println(m(5))
}
