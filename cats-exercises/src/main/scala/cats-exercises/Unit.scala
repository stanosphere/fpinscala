import cats.Id

// just so i can see the function signatures
trait SomeId {
  def map[A, B](fa: Id[A])(f: A => B): Id[B]
  def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B]
  def coflatMap[A, B](a: Id[A])(f: Id[A] => B): Id[B]
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
