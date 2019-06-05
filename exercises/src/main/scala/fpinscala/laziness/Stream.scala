//package fpinscala.laziness

trait Stream[+A] {

  def toList: List[A] = this match {
    case Cons(x, xs) => x() :: xs().toList
    case _ => Nil
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // this is almost correct but it gives us the taken elements in the wrong order!
  def takeAttempt1(n: Int): Stream[A] = {
    def go[A](i: Int, baseStream: Stream[A], stream: Stream[A]): Stream[A] =
      if(i == n) stream
      else baseStream match {
        case Cons(x, xs) => go(i + 1, xs(), Cons(x, () => stream))
      }
    go(0, this, Empty)
  }

  // let's try pattern matching our problems away
  // in js I'd probs just use a while loop lol
  def take(n: Int): Stream[A] = {
    if (n == 0) Empty
    else this match {
      case Cons(x, xs) => Cons(x, () => xs().take(n - 1))
    }
  }

  // looks like this isn't tail recursive either so I think my take is fine
  def takeAns(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Cons(h, () => t().takeAns(n - 1))
    case Cons(h, _) if n == 1 => Cons(h, () => Empty)
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = ???

  def takeWhile(p: A => Boolean): Stream[A] = ???

  def forAll(p: A => Boolean): Boolean = ???

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}