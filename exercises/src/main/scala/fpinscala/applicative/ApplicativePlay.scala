package fpinscala.applicative

object ApplicativePlay extends App {
  val streamApplicative = Applicative.streamApplicative

  val s1 = Stream.from(1)
  val s2 = Stream.from(2)
  val s3 = Stream.continually("Paul")

  val hi = streamApplicative.map2(s1, s2)(_ * _).take(10).toList
  println(hi) // -> 1*2, 2*3, 3*4, ...

  // I think sequence is basically just zipN
  // which is basically just a transpose that cuts off at the shortest stream
  val stano = streamApplicative.sequence(List(s1, s2, s3)).take(10).toList
  println(stano)
}
