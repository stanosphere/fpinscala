package experiments

// http://milessabin.com/blog/2011/06/09/scala-union-types-curry-howard/

object UnionTypes extends App {
  type ¬[A] = A => Nothing
  type ∨[T, U] = ¬[¬[T] with ¬[U]]
  type ¬¬[A] = ¬[¬[A]]
  type |∨|[T, U] = { type λ[X] = ¬¬[X] <:< (T ∨ U) }

  def size[T](t: T)(implicit ev: ¬¬[T] <:< (Int ∨ String)): Int =
    t match {
      case i: Int => i
      case s: String => s.length
    }

  println(size(23))
}
