val doubleEvens: PartialFunction[Int, Int] =
  new PartialFunction[Int, Int] {
    //States that this partial function will take on the task
    def isDefinedAt(x: Int) = x % 2 == 0

    //What we do if this partial function matches
    def apply(v1: Int) = v1 * 2
  }

val tripleOdds: PartialFunction[Int, Int] = new PartialFunction[Int, Int] {
  def isDefinedAt(x: Int) = x % 2 != 0

  def apply(v1: Int) = v1 * 3
}

val whatToDo = doubleEvens orElse tripleOdds //Here we chain the partial functions together

val doubleEvens2: PartialFunction[Int, Int] = {
  case x if (x % 2) == 0 ⇒ x * 2
}
val tripleOdds2: PartialFunction[Int, Int] = {
  case x if (x % 2) != 0 ⇒ x * 3
}

val whatToDo2 = doubleEvens orElse tripleOdds

