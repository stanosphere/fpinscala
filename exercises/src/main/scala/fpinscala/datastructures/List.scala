package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  type NestedList[A] = List[List[A]]

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](as1: List[A], as2: List[A]): List[A] =
    as1 match {
      case Nil => as2
      case Cons(x, xs) => Cons(x, append(xs, as2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](lst: List[A]): List[A] =
    lst match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

  def setHead[A](lst: List[A], h: A): List[A] =
    lst match {
      case Nil => Nil
      case Cons(_, xs) => Cons(h, xs)
    }

  def drop[A](l: List[A], n: Int): List[A] = {
    def go[A](i: Int, lst: List[A]): List[A] =
      if(i == 0) lst
      else go(i - 1, tail(lst))
    go(n, l)
  }

  def dropWhile[A](lst: List[A], f: A => Boolean): List[A] =
    lst match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else lst
    }

  def dropWhile2[A] (as: List[A]) (f: A => Boolean): List[A] =
    as match {
      case Cons(x, xs) if f(x) => dropWhile2(xs)(f)
      case _ => as
    }


  def init[A](lst: List[A]): List[A] =
    lst match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  def length[A](lst: List[A]): Int =
    foldRight(lst, 0)((_, x) => x + 1)

  def foldLeft[A,B](lst: List[A], z: B)(f: (B, A) => B): B =
    lst match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum3(lst: List[Int]): Int = foldLeft(lst, 0)(_+_)

  def product3(lst: List[Double]): Double = foldLeft(lst, 1.0)(_*_)

  def reverse[A](lst: List[A]): List[A] =
    lst match {
      case Nil => Nil
      case Cons(x, Nil) => List(x)
      case Cons(x, xs) => append(reverse(xs), List(x))
    }

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))

  def reverse2[A](lst: List[A]): List[A] =
    foldLeft(lst, Nil: List[A])((res, x) => append(List(x), res))

  def append2[A](xs: List[A], ys: List[A]): List[A] =
    foldRight(xs, ys)(Cons(_,_))

  def flatten[A](xss: NestedList[A]): List[A] =
    foldRight(xss, Nil: List[A])(append)

  def addOneToAll (ints: List[Int]): List[Int] =
    foldRight(ints, Nil: List[Int]) ((x, xs) => Cons(x + 1, xs))

  def doublesToStrings (ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String]) ((x, xs) => Cons(x.toString(), xs))

  def map[A,B](lst: List[A])(f: A => B): List[B] =
    foldRight(lst, Nil: List[B]) ((x, xs) => Cons(f(x), xs))

  def map2[A,B](lst: List[A])(f: A => B): List[B] =
    lst match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map2(xs)(f))
    }

  def map3[A,B](lst: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(lst, Nil: List[B]) ((x, xs) => Cons(f(x), xs))

  def filter[A](lst: List[A])(f: A => Boolean): List[A] =
    foldRight(lst, Nil: List[A]) ((x, xs) => if (f(x)) Cons(x, xs) else xs)

  def flatMap[A, B] (as: List[A]) (f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B]) ((x, xs) => append(f(x), xs))

  def flatMap2[A,B](l: List[A])(f: A => List[B]): List[B] =
    flatten(map(l)(f))

  def filterViaFlatMap[A](lst: List[A])(f: A => Boolean): List[A] =
    flatMap(lst)(x => if (f(x)) List(x) else Nil)

  def zipWithAdd(xs: List[Int], ys: List[Int]): List[Int] =
    (xs, ys) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x, _xs), Cons(y, _ys)) => Cons(x + y, zipWithAdd(_xs, _ys))
    }

  def zipWith[A,B,C](as: List[A], bs: List[B]) (f: (A, B) => C): List[C] =
    (as, bs) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }

  // if we get to the end of the sub sequence then it's a match
  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(x, xs), Cons(y, ys)) =>
      if(x == y) startsWith(xs, ys)
      else false
  }

  // the problem I had was that doing hasSubSequence(xs, ys)
  // was giving me a true for List(1,2,3), List(1,3)
  // looking at the pattern matching it's kind of obvious why
  // we were looking for a List(3) inside the remaining list (List(2,3))
  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(x, xs), Cons(y, ys)) => {
      if (x == y) startsWith(xs, ys)
      else hasSubSequence(xs, sub)
    }
  }

  // the fact that my above two functions have the same signature
  // and that they have the same matching cases suggests that
  // some abstraction may be useful here???
}
