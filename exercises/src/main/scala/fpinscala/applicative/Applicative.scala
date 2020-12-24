package fpinscala
package applicative

import sext._

import monads.{Functor, Id}
import state._
import monoids._

import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {

  def unit[A](a: => A): F[A]

  // you can use either apply or map2 as the entry point;
  // whichever is easiest to define for your instance I suppose

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    // curry it and apply to both values
    val fabc = unit(f.curried)
    val fbc = apply(fabc)(fa)
    val fc = apply(fbc)(fb)

    fc
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])
                      (f: (A, B, C) => D): F[D] = {
    def g(a: A, b: B)(c: C): D = f(a, b, c)

    val fcd = map2(fa, fb)(g)
    val fd = apply(fcd)(fc)

    fd
  }

  // so I've found a kind of pseudo recursive way of doing this sort of thing
  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])
                         (f: (A, B, C, D) => E): F[E] = {
    def g(a: A, b: B, c: C)(d: D): E = f(a, b, c, d)

    val fde = map3(fa, fb, fc)(g)
    val fe = apply(fde)(fd)

    fe
  }

  def mapViaMap2[A, B](fa: F[A])(f: A => B): F[B] = {
    val fb: F[Unit] = unit(())
    map2(fa, fb)((a, _) => f(a))
  }

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = {
    val zero: F[List[B]] = unit(Nil)

    as.foldRight(zero)((a, fbs) => map2(f(a), fbs)(_ :: _))
  }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ???

  // why do we need the type lambda here but not for Monoids?
  // Because the type that a monoid takes as a type parameter is not a higher kinded type whereas F and G are
  // Without this type lambda here, we would have to write a separate product function for each x
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val F = this

    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) =
        (F unit a, G unit a)

      override def map2[A, B, C](fpga: (F[A], G[A]), fpgb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {
        val (fa, ga) = fpga
        val (fb, gb) = fpgb
        (F.map2(fa, fb)(f), G.map2(ga, gb)(f))
      }
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val F = this

    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))

      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        F.map2(fga, fgb)((ga, gb) => G.map2(ga, gb)(f))
    }

  }

  def sequenceMap[K, V](fVsByKs: Map[K, F[V]]): F[Map[K, V]] = {
    // I think we just need to fold the map
    val zero = unit(Map.empty[K, V])

    fVsByKs.foldRight(zero)((pair, fKsByVs) => {
      // Note: in the future you could use pattern matching to destructure this entirely
      val (k, fv) = pair
      map2(fKsByVs, fv)((ksByVs, v) => ksByVs + (k -> v)
      )
    })
  }

  object Laws {
    def identityLaw[A](fa: F[A]): Boolean =
      apply(unit((x: A) => x))(fa) == fa

    def homomorphismLaw[A, B](f: A => B, x: A): Boolean =
      apply(unit(f))(unit(x)) == unit(f(x))

    def interchangeLaw[A, B](u: F[A => B], y: A): Boolean =
      apply(u)(unit(y)) == apply(unit((_: A => B) => y))(u)

  }

}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))

  override def map[A, B](m: F[A])(f: A => B): F[B] =
    flatMap(m)(a => unit(f(a)))

  override def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  // Just gonna figure out _why_ this can't be done
  //  def _compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = {
  //    val F = this
  //
  //    new Monad[({type f[x] = F[G[x]]})#f] {
  //      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
  //
  //      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
  //        F.map2(fga, fgb)((ga, gb) => G.map2(ga, gb)(f))
  //
  //      // the problem is how in the hell to we implement join or flatmap???
  //      override def join[A](mma: F[G[F[G[A]]]]): F[G[A]] = {
  //        // I don't think we can join F-G-F-G :(
  //        // because we can't join F and G
  //        // now if I could swap F and G around things would be good
  //        // but I can't so tough
  //      }
  //    }
  //  }

}

object Monad {
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      ma match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_], N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
  Monad[({type f[x] = F[N[x]]})#f] = {
    new Monad[({type f[x] = F[N[x]]})#f] {
      override def unit[A](a: => A): F[N[A]] = F.unit(N.unit(a))

      override def flatMap[A, B](fna: F[N[A]])(f: A => F[N[B]]): F[N[B]] = {
        F.flatMap(fna)(na => {
          val fnnb = T.traverse(na)(f)
          F.map(fnnb)(N.join)
        })
      }
    }
  }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative: Applicative[Stream] = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A, B, C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                                                            f: (A, B) => C): Stream[C] =
      (a zip b) map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      override def unit[A](a: => A): Validation[E, A] =
        Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (Success(_), Failure(h, t)) => Failure(h, t)
          case (Failure(h, t), Success(_)) => Failure(h, t)
          case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
        }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({type f[x] = Const[M, x]})#f] =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A): M = M.zero

      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }

  // implicits for playing with traverse

  implicit val optionApp: Applicative[Option] = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      (fa, fb) match {
        case (Some(a), Some(b)) => Some(f(a, b))
        case _ => None
      }
  }

  implicit val idApp: Applicative[Id] = new Applicative[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] =
      Id(f(fa.value, fb.value))
  }

  implicit val listApp: Applicative[List] = new Applicative[List] {
    override def unit[A](a: => A): List[A] = List(a)

    // this is more of a cartesian product than a zipWith
    override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] =
      for {a <- fa; b <- fb} yield f(a, b)
  }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse(fa)(a => Id(f(a))).value

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  // get the current state, compute the next state, set it, and yield some value
  def mapAccum[S, A, B](fa: F[A], s0: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) =>
      for {
        s1 <- StateUtil.get[S]
        (b, s2) = f(a, s1)
        _ <- StateUtil.set(s2)
      } yield b
    ).run(s0)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  // this has got to be a mapAccum thing
  def reverse[A](fa: F[A]): F[A] = {
    val (_, reversedList) = mapAccum(fa, Nil: List[A])((a, s) => ((), a :: s))
    // once the reversed list is found, we can take items off the top of it
    // one at a time to repopulate F with its original contents but the other way around
    mapAccum(fa, reversedList)((_, as) => (as.head, as.tail))._1
  }

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, acc) => ((), f(acc, a)))._2

  override def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    mapAccum(as, z)((a, acc) => ((), f(a, acc)))._2


  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)

  def compose[F2[_]](implicit F2: Traverse[F2]): Traverse[({type f[x] = F[F2[x]]})#f] = {
    val F = this

    new Traverse[({type f[x] = F[F2[x]]})#f] {
      override def traverse[G[_] : Applicative, A, B](fa: F[F2[A]])(f: A => G[B]): G[F[F2[B]]] =
        F.traverse(fa)(F2.traverse(_)(f))
    }
  }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {
  // you need to implement either sequence or traverse for these things
  val listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] = {
      val zero = G.unit(Nil: List[B])
      fa.foldRight(zero)((a, glb) => {
        val gb = f(a)
        G.map2(gb, glb)(_ :: _)
      })
    }
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = {
      oa match {
        case Some(a) =>
          val gb = f(a)
          G.map(gb)(Some(_))
        case None => G.unit(None)
      }
    }
  }

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[G[_], A, B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = {
      val Tree(head, children) = ta
      val gHead = f(head)

      // if this list is empty then nothing happens and you just get G(Nil)
      val wrappedTraversedList = listTraverse.traverse(children)(traverse(_)(f))

      // now that the work has been done, we can just pop them into a new Tree
      G.map2(gHead, wrappedTraversedList)(Tree(_, _))
    }
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
