package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par.{Par, _}

import language.higherKinds

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](mas: List[M[A]]): M[List[A]] =
    traverse(mas)(identity)

  // the main logic of this function would probably be simpler to understand in terms of map2
  def traverse[A, B](as: List[A])(f: A => M[B]): M[List[B]] = {
    val zero: M[List[B]] = unit(Nil)

    as.foldRight(zero)((a, acc) => {
      val mb = f(a)
      flatMap(mb)(b => map(acc)(b :: _))
    })
  }

  def _traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = {
    val zero: M[List[B]] = unit(Nil)

    la.foldRight(zero)((a, mlb) => map2(f(a), mlb)(_ :: _))
  }

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  // this looks very very similar to my `traverse` function
  def filterM[A](as: List[A])(f: A => M[Boolean]): M[List[A]] = {
    val zero = unit(Nil: List[A])
    as.foldRight(zero)((a, acc) =>
      flatMap(f(a))(bool => if (bool) map(acc)(a :: _) else acc)
    )
  }

  // in the official answer they define it recursively but I'm pretty sure my answer is equivalent
  // the difference being that I use the foldRight to abstract out the pattern matching and the traversal of the list
  // whereas here it is done explicitly
  def _filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(b =>
        if (!b) _filterM(t)(f)
        else map(_filterM(t)(f))(h :: _))
    }


  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = {
    val thunk = (_: Unit) => ma
    val g: Unit => M[B] = compose(thunk, f)
    g(())
  }

  def join[A](mma: M[M[A]]): M[A] = ???

  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = ???
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad: Monad[Gen] = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] =
      Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =
      ma flatMap f
  }

  def parserMonad[P[+ _]](p: Parsers[P]): Monad[P] = new Monad[P] {
    def unit[A](a: => A): P[A] =
      p.succeed(a)

    override def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] =
      p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] =
      Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] =
      Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] =
      List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

  // this actually produces an infinite family of monads. One for each possible S
  // there is a way to do this more concisely but I have no idea how lol
  def stateMonad[S] = {
    // nothing more than partial application
    // analogous to if I had a function like f = (x,y) => x + y
    // I am well with my rights to define g = x => f(x, 12)
    type ParticularState[A] = State[S, A]

    new Monad[ParticularState] {
      def unit[A](a: => A): State[S, A] =
        State unit a

      override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] =
        ma flatMap f
    }
  }

  //  val idMonad: Monad[Id] = ???
  //
  //  def readerMonad[R] = ???
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = ???

  def flatMap[B](f: A => Id[B]): Id[B] = ???
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R, A] = ???

    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = ???
  }
}

