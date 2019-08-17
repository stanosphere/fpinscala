package fpinscala.errorhandling
// To get this to work you need to paste it into the repl
// Use :paste, paste it, and then use ctrl-D to exit paste mode

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  // here B is constrained to be a supertype of A
  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(x) => Some(x)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) => if (f(x)) Some(x) else None
    case None => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    (a,b) match {
      case (_, None) => None
      case (None, _) => None
      case (Some(x), Some(y)) => Some(f(x, y))
    }

  def sequence[A](xs: List[Option[A]]): Option[List[A]] =
    xs.foldRight(Some(Nil): Option[List[A]])((x, acc) => (acc, x) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(lst), Some(y)) => Some(y::lst)
    })

  // this is a much more sensible implementation and it fully demonstrates how useful map2 is!
  def sequence_1[A](xs: List[Option[A]]): Option[List[A]] =
    xs.foldRight(Some(Nil): Option[List[A]])((x,y) => map2(x,y)(_ :: _))

  def traverse[A,B](xs: List[A])(f: A => Option[B]): Option[List[B]] =
    xs.foldRight(Some(Nil): Option[List[B]])((x, acc) => (acc, f(x)) match {
      case (None, _) => None
      case (_, None) => None
      case (_, Some(value)) => acc.map(lst => value :: lst)
    })

  def traverse_1[A, B](lst: List[A])(f: A => Option[B]): Option[List[B]] =
    lst.foldRight[Option[List[B]]](Some(Nil))((x, xs) => map2(f(x), xs)(_ :: _))

  def sequenceViaTraverse[A](xs: List[Option[A]]): Option[List[A]] =
    traverse(xs)(x => x)

  def map2_2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

}