package experiments

// http://milessabin.com/blog/2011/06/09/scala-union-types-curry-howard/

object UnionTypes extends App {
  type ¬[A] = A => Nothing
  type ¬¬[A] = ¬[¬[A]]
  type ∨[T, U] = ¬[¬[T] with ¬[U]]
  type |∨|[T, U] = { type λ[X] = ¬¬[X] <:< (T ∨ U) }

  def size[T](t: T)(implicit ev: ¬¬[T] <:< (Int ∨ (String ∨ Char))): Int =
    t match {
      case i: Int => i
      case s: String => s.length
      case _: Char => 1
    }

  println(size(23))
}
