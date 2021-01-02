package fpinscala.adjunction

// I'll revisit this when I actually understand it better!
case class GaloisConnectionForIntegerPosets(y: Int) {
  // z * y <= x <=> z <= x / y
  //   f z <= x <=> z <= g x

  val f: Int => Int = _ * y
  val g: Int => Int = _ / y

  def unit(x: Int): Boolean = f(g(x)) <= x

  def counit(x: Int): Boolean = x <= g(f(x))


}
