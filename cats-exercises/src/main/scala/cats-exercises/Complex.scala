package complex

import cats.{Foldable, Monoid}
import cats.implicits._

case class Complex(re: Double, im: Double) {
  def +(y: Complex): Complex = Complex(re + y.re, im + y.im)
  def -(y: Complex): Complex = Complex(re - y.re, im - y.im)
  def *(y: Complex): Complex = Complex(
    re * y.re - im * y.im,
    re * y.im + im * y.re
  )
  def conjugate(): Complex = Complex(re, -im)
  def mag(): Double = Math.hypot(re, im)
}

object Complex extends App {
  implicit def complexMonoid: Monoid[Complex] =
    new Monoid[Complex] {
      def combine(x: Complex, y: Complex): Complex = x - y
      def empty: Complex = Complex(1.0, 0.0)
    }

  def main(): Unit = {
    val one = Foldable[List].fold(List(
      Complex(0.0, 1.0),
      Complex(0.0, -1.0)
    ))
    println(one)
  }

  main()
}

