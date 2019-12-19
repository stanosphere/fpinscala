package fpinscala.monoids

import fpinscala.parallelism.Par
import fpinscala.parallelism.Par._
import fpinscala.parallelism.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds
import fpinscala.testing._
import Prop._

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    val zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    val zero: List[A] = Nil
  }

  // the following four re all commutative and therefore self-duel
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int): Int = x + y

    val zero: Int = 0
  }

  val floatAddition: Monoid[Float] = new Monoid[Float] {
    def op(x: Float, y: Float): Float = x + y

    val zero: Float = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int): Int = x * y

    val zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean): Boolean = x || y

    val zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean): Boolean = x && y

    val zero: Boolean = true
  }

  // this is associative but not commutative
  // more succinctly, op could be written x orElse y
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
      case (None, None) => None
      case (Some(a), None) => Some(a)
      case (None, Some(b)) => Some(b)
      case (Some(a), Some(_)) => Some(a)
    }

    val zero: Option[A] = None
  }

  // again we need to make a choice
  // f(g(x)) or g(f(x)); compose or andThen
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A): A => A = f compose g

    def zero: A => A = x => x
  }

  def trimMonoid(s: String): Monoid[String] = ???

  // "flips" the Monoid
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero = m.zero
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((acc, a) => m.op(acc, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    val g: A => (B => B) = x => y => f(x, y)
    foldMap(as, endoMonoid[B])(g)(z)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    val g: A => (B => B) = x => y => f(y, x)
    foldMap(as, dual(endoMonoid[B]))(g)(z)
  }

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.size <= 1) {
      as.headOption map f getOrElse m.zero
    } else {
      val (l, r) = as.splitAt(as.length / 2)
      val g: IndexedSeq[A] => B = xs => foldMapV(xs, m)(f)
      m.op(g(l), g(r))
    }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(x: Par[A], y: Par[A]): Par[A] = Par.map2(x, y)(m.op)

    val zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: List[A], m: Monoid[B])(f: A => B): Par[B] =
    Par
      .parMap(v)(f) // apply f in parallel
      .flatMap(bs => foldMap(bs, par(m))(Par.unit)) // perform fold in parallel

  // this one just does positive integers ascending
  def ordered(ints: List[Int]): Boolean = {
    case class Tracker(value: Int, isOrdered: Boolean)
    val notOrdered = Tracker(0, isOrdered = false)

    val orderTrackerMonoid: Monoid[Tracker] = new Monoid[Tracker] {
      def zero: Tracker = Tracker(0, isOrdered = true)

      def op(tL: Tracker, tR: Tracker): Tracker = (tL, tR) match {
        case (Tracker(_, false), _) => notOrdered
        case (_, Tracker(_, false)) => notOrdered
        case (Tracker(x, _), Tracker(y, _)) =>
          if (x < y) Tracker(y, isOrdered = true)
          else notOrdered
      }
    }

    foldMap(ints, orderTrackerMonoid)(Tracker(_, isOrdered = true)).isOrdered
  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(wc1: WC, wc2: WC): WC = (wc1, wc2) match {
      case (Stub(x), Stub(y)) => Stub(x + y)
      case (Stub(x), Part(l, cnt, r)) => Part(x + l, cnt, r)
      case (Part(l, cnt, r), Stub(y)) => Part(l, cnt, r + y)
      case (Part(l1, cnt1, r1), Part(l2, cnt2, r2)) => {
        val cnt3 = if ((r1 + l2).length == 0) 0 else 1
        Part(l1, cnt1 + cnt2 + cnt3, r2)
      }
    }

    def zero: WC = Stub("")
  }

  def count(s: String): Int = ???

  // it might seem sensible to try to write some monoid that
  // fuses a pair of monoids
  // something like:
  // def fuseMonoids[A,B,C](A: Monoid[A], B: Monoid[B])(f: (A,B) => C): Monoid[C]
  // but this is impossible because we cannot get the A's and B's back from C's
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(x: (A, B), y: (A, B)): (A, B) =
      (A.op(x._1, y._1), B.op(x._2, y._2))

    def zero: (A, B) = (A.zero, B.zero)
  }

  // Actually I think we can have a coproduct monoid!
  // http://blog.higher-order.com/blog/2014/03/19/monoid-morphisms-products-coproducts/

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    val zero: A => B =
      _ => B.zero

    def op(f: A => B, g: A => B): A => B =
      x => B.op(f(x), g(x))
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero: Map[K, V] = Map[K, V]()

    def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(
          k,
          V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero))
        )
      }
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val countMonoid: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)
    val toMap = (a: A) => Map[A, Int](a -> 1)
    IndexedSeqFoldable.foldMap(as)(toMap)(countMonoid)
  }

}

object MonoidLaws extends App {

  case class ThreeValues[A](x: A, y: A, z: A)

  def associativity[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield ThreeValues(x, y, z)) { vs =>
      val ThreeValues(x, y, z) = vs
      m.op(x, m.op(y, z)) == m.op(m.op(x, y), z)
    }

  def identity[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen)((a: A) => m.op(a, m.zero) == a && m.op(m.zero, a) == a)

  def allLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    identity(m, gen) && associativity(m, gen)

  Prop.run(allLaws(
    Monoid.stringMonoid,
    Gen.chooseAlphaNumericString(Gen.choose(1, 8))
  ))

  // annoyingly we have to specify A
  // although it would be rather absurd to think we could test this magically for all types
  Prop.run(allLaws(
    Monoid.listMonoid[Int],
    Gen.listOfN(10, Gen.choose(1, 10))
  ))

  import Monoid.{Stub, Part}

  val genStub = Gen
    .chooseAlphaNumericString(Gen.choose(0, 10))
    .map(Stub)

  val genPart = Gen.map3(
    Gen.chooseAlphaNumericString(Gen.choose(0, 10)),
    Gen.choose(1, 20),
    Gen.chooseAlphaNumericString(Gen.choose(0, 10))
  )(Part)

  Prop.run(allLaws(
    Monoid.wcMonoid,
    Gen.weighted((genStub, 10), (genPart, 90))
  ))
}

trait Foldable[F[_]] {

  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((acc, a) => mb.op(acc, f(a)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    ???

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(Nil: List[A])(_ :: _)
}

object ListFoldable extends Foldable[List] {
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Leaf(value) => f(z, value)
      case Branch(left, right) => {
        // fold left side first then use this value as the new zero when folding the right side
        val zz = foldLeft(left)(z)(f)
        foldLeft(right)(zz)(f)
      }
    }

  def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Leaf(value) => f(value, z)
      case Branch(left, right) => {
        // fold right side first then use this value as the new zero when folding the left side
        val zz = foldRight(right)(z)(f)
        foldRight(left)(zz)(f)
      }
    }

  // this provides a much more balanced version of foldMap than having it implemented via foldLeft
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(a) => f(a)
      case Branch(left, right) => {
        val bLeft = foldMap(left)(f)(mb)
        val bRight = foldMap(right)(f)(mb)
        mb.op(bLeft, bRight)
      }
    }
}

object OptionFoldable extends Foldable[Option] {
  def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as match {
      case None => z
      case Some(a) => f(z, a)
    }

  def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as match {
      case None => z
      case Some(a) => f(a, z)
    }

  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case None => mb.zero
      case Some(a) => f(a)
    }

}

// an interesting observation is that for the option monoid we do not use the mb.op
// and that for the tree monoid we do not use the mb.zero

// this is because an empty tree does not exist
// and because an option can contain precisely one value

