package scalacheck

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object PlayingWithScalaCheck extends App {
  val propReverseList = forAll { l: List[String] => l.reverse.reverse == l }

  val propConcatString = forAll { (s1: String, s2: String) =>
    (s1 + s2).endsWith(s2)
  }

  val propSqrt = forAll { (n: Int) => scala.math.sqrt(n * n) == n }

  propReverseList.check()
  propSqrt.check()
}
