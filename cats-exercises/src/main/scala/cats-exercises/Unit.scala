import cats.Id

// just so i can see the function signatures
trait SomeId[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def coflatMap[A, B](a: F[A])(f: F[A] => B): F[B]
}

object TheEffectThatHasNoEffect extends App {
  def idIsJustANormalValue(): Unit = {
    val x: Id[Int] = 1
    val y: Int = x
    println(x, y)
  }

  def main(): Unit = {
    idIsJustANormalValue()
  }

  main()
}
