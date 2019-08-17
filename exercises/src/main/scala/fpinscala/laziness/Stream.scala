package fpinscala.laziness

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
    def go(i: Int, baseStream: Stream[A], stream: Stream[A]): Stream[A] =
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
      case Cons(x, xs) => Cons(x, () => xs() take(n - 1))
      case _ => Empty
    }
  }

  // let's try implementing take with the smart constructor
  def takeWithSmartConstructor(n: Int): Stream[A] = {
    if (n == 0) Empty
    else this match {
      case Cons(x, xs) => Stream.cons(x(), xs() take(n - 1))
      case _ => Empty
    }
  }

  // looks like this isn't tail recursive either so I think my take is fine
  def takeAns(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Cons(h, () => t() takeAns(n - 1))
    case Cons(h, _) if n == 1 => Cons(h, () => Empty)
    case _ => Empty
  }

  def takeViaUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n))({
      case (_, 0) => None
      case (Cons(h, t), i) => Some((h(), (t(), i - 1)))
      case _ => None
    })

  // this _is_ tail recursive which is nice!
  def drop(n: Int): Stream[A] = {
    if (n == 0) this
    else this match {
      case Cons(_, xs) => xs() drop(n - 1)
      case _ => Empty
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if (p(h()))
      Cons(h, () => t() takeWhile p ) else
      Empty
    case _ => Empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])(
      (x, acc) =>
        if(p(x)) Stream.cons(x, acc)
        else Empty
    )

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    Stream.unfold(this)({
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    })

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((x, acc) => acc && p(x))

  // since (h,_)=>Some(h) never evaluates its second argument, the recursion never occurs
  def headOption: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((h, t) => Stream.cons(f(h), t))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this)({
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    })

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])(
      (h, t) => if (p(h)) Stream.cons(h, t) else t
    )

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)(Stream.cons(_,_))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((x, xs) => f(x) append xs)

  def nth (n: Int): A = this.drop(n-1).take(1) match {
    case Cons(x,_) => x()
  }

  // I did it this way because I forgot the forAll method existed!
  def startsWith [B>:A](s: Stream[B]): Boolean =
    Stream.zipAll(this, s).foldRight(true)((x, acc) =>
    acc && (x match {
      case (None, _) => false
      case (_, None) => true
      case (a,b) => a == b
    }))

  def startsWithAns[A](s: Stream[A]): Boolean =
    Stream.zipAll(this, s).takeWhile(_._2 != None) forAll {
      case (a,b) => a == b
    }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this)({
      case Cons(h,t) => Some((Cons(h,t), t()))
      case _ => None
    }).append(Empty)

  def hasSubSequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream(z))((x, acc) => {
      lazy val s = acc
      val Cons(prev, _) = s
      val next = f(x, prev())
      Stream(next).append(s)
    })

  // their fold keeps the value itself as well as the Stream but the Stream only gets evaluated at the end i think
  // whereas in my answer I was evaluating the head of the stream at each point which is a bit silly if it can be avoided
  def scanRightAns[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, Stream.cons(b2, p1._2))
    })._2

  def zip[B](bs: Stream[B]): Stream[(A,B)] =
    Stream.zip(this, bs)
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

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constantAns[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  val fibs: Stream[BigInt] = {
    def go(prev: BigInt, curr: BigInt): Stream[BigInt] =
      cons(curr, go(curr, prev + curr))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((x, s)) => cons(x, unfold(s)(f))
      case None => Empty
    }

  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some(1,1))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a,a))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some(s + 1, s + 1))

  val fibsViaUnfold: Stream[BigInt] =
    unfold((0: BigInt,1: BigInt))(s => {
      val (prev, curr) = s
      val next = prev + curr
      Some(curr, (curr, next))
    })

  def zipWith[A,B,C](as: Stream[A], bs: Stream[B]) (f: (A, B) => C): Stream[C] =
    unfold((as, bs))({
      case (_, Empty) => None
      case (Empty, _) => None
      case (Cons(a, aas), Cons(b, bbs)) => Some((f(a(),b()), (aas(), bbs())))
      case _ => None
    })

  def zip[A,B](as: Stream[A], bs: Stream[B]): Stream[(A,B)] =
    zipWith(as, bs)((_,_))

  def zipAll[A,B](as: Stream[A], bs: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((as, bs))(s => s match {
      case (Cons(a, aas), Cons(b, bbs)) => {
        val aa = Some(a())
        val bb = Some(b())
        Some((aa, bb), (aas(), bbs()))
      }
      case (Cons(a, aas), Empty) => {
        val aa = Some(a())
        Some((aa, None), (aas(), Empty))
      }
      case (Empty, Cons(b, bbs)) => {
        val bb = Some(b())
        Some((None, bb), (Empty, bbs()))
      }
      case _ => None
    })

}