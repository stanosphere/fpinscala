package fpinscala.adjunction

import fpinscala.monads.Id

object AdjunctionPlay extends App {
  val m = Adjunction.listId.monadFromAdjunction
  println(m.unit("Hi"))
  println(m.unit("hello").flatMap(x => Id(x ++ x)))
}
