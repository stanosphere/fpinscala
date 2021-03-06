package fpinscala.adjunction

import fpinscala.monads.{Functor, Id, Monad}

trait Adjunction[F[_], G[_]] {
  def unit[A](a: A): G[F[A]]

  def counit[A](fga: F[G[A]]): A

  // F ⊣ G (F is left Adjoint to G)
  def F: Functor[F]

  def G: Functor[G]

  // this trait, as usual must satisfy some laws
  // composition laws
  // there's a nicer notation in my notebook
  def law1[A](x: F[A]): Boolean = {
    val y = counit(F.map(x)(unit))
    y == x
  }

  def law2[A](x: G[A]): Boolean = {
    val y = G.map(unit(x))(counit)
    y == x
  }

  // Another way to view an adjunction is that there is an isomorphism
  // between the types F[A] => B and A => G[B]
  def leftAdjunct[A, B](k: F[A] => B): A => G[B] =
    a => G.map(unit(a))(k)

  def rightAdjunct[A, B](k: A => G[B]): F[A] => B =
    fa => counit(F.map(fa)(k))

  // any adjunction has the property that G[F[_]] is a monad:
  type M[A] = G[F[A]]

  def monadUnit[A](a: A): M[A] = unit(a)

  def monadFromAdjunction: Monad[M] = new Monad[M] {
    override def unit[A](a: => A): M[A] =
      monadUnit(a)

    override def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
      join(map(ma)(f))

    override def join[A](mma: M[M[A]]): M[A] =
      G.map(mma)(counit)

    override def map[A, B](ma: M[A])(f: A => B): M[B] =
      G.map(ma)(F.map(_)(f))
  }

}

object Adjunction {
  // This must be isomorphic to the list monad
  val listId: Adjunction[List, Id] = new Adjunction[List, Id] {
    override def unit[A](a: A): Id[List[A]] =
      Id(List(a))

    override def counit[A](fga: List[Id[A]]): A =
      fga.head.value

    override def F: Functor[List] = new Functor[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] =
        fa map f
    }

    override def G: Functor[Id] = new Functor[Id] {
      override def map[A, B](fa: Id[A])(f: A => B): Id[B] =
        Id(f(fa.value))
    }
  }
}
