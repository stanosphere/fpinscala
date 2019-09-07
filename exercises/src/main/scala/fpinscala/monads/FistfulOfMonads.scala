package fpinscala.monads

import Monad._

object FistfulOfMonads extends App {
  // In my pseudo Hindley-Milner notation:
  // replicateM :: Monad m => Int -> m a -> m [a]
  def whatDoesReplicateMDo(): Unit = {
    // this will generate several lists of length n
    // where the things in those lists are taken from the values in the monadic list
    val xss = Monad.listMonad.replicateM(3, List(1,2,3))
    println(xss.length)
    println(xss)

    // this will generate a list of length 3 wrapped in a Some
    // the values in the list come from what is in the initial Some
    val summin = Monad.optionMonad.replicateM(3, Some(20))
    println(summin)

    // this will generate a None
    val nothin = Monad.optionMonad.replicateM(3, None)
    println(nothin)

    // this will print a vector of length 8 == 2 ^ 3
    // the elements of the vector will be lists of length 3
    // they will be all possible permutations of 0 and 1 (as 0 and 1 are what are drawn from the Stream)
    val mahStream = Monad.streamMonad.replicateM(3, Stream(0, 1))
    println(mahStream.toIndexedSeq)

    // so what does replicateM do in general?
    // ultimately it relies on traverse to combine values
    // the way in which these values are combined is specified, ultimately, by how flatMap is implemented for the monad in question
    // it is worth noting that although List very much is a Monad, we certainly are not using any of its Monadic features
    // In fact, I believe we could use any sort of foldable collection if we wanted to for some reason :p

    // so here goes:
    // replicateM repeats a monadic value n times,
    // and then gathers the results in a single value,
    // where the monad determines how values are actually gathered

    // for example it creates Stream(0,1) 3 times and then decides how to combine these values based on Stream's flatmap
    // I actually thing `traverse` would benefit from being written in for comprehension notation
    // that way it might be slightly clearer how the values are combined and where they come from
  }

  def whatDoesFilterMDo(): Unit = {
    // filterM :: Monad m => [a] -> (a -> m Bool) -> m [a]
    val xs = List(1,2,3)
    val f: Int => List[Boolean] = x => List(x % 2 == 0)
    val g: Int => List[Boolean] = x => List(x % 2 != 0)
    val h: Int => List[Boolean] = x => List(x % 2 == 0, x % 2 != 0)

    val computeList: (Int => List[Boolean]) => List[List[Int]] =
      Monad.listMonad.filterM(xs)

    // this is kind of fascinating, or it is for the `h` function anyway
    // I believe it is doing a permutation like thing and actually returning the powerset of the list
    // I'm going to see if I can write a generic powerset function
    println(computeList(f))
    println(computeList(g))
    println(computeList(h))

    // this allows filtering with the possibility of failure not fucking us up
    // you can just return a None to represent a failure
    val computeOption: (Int => Option[Boolean]) => Option[List[Int]] =
      Monad.optionMonad.filterM(xs)

    val f1: Int => Option[Boolean] = x => Some(x % 2 == 0)
    val g1: Int => Option[Boolean] = x => Some(x % 2 != 0)
    val h1: Int => Option[Boolean] = _ => None

    println(computeOption(f1))
    println(computeOption(g1))
    println(computeOption(h1))
  }

  def powerSet[A](as: List[A]): List[List[A]] =
    Monad.listMonad.filterM(as)(_ => List(true, false))

  def main(): Unit = {
    whatDoesReplicateMDo()
    println("=====================================================")
    whatDoesFilterMDo()
    println("=====================================================")
    println(powerSet(List("a", "b", "c")))
  }

  main()
}
