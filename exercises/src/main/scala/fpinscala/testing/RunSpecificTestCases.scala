package fpinscala.testing

import java.util.concurrent.Executors

import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par
import fpinscala.testing.Gen._
import fpinscala.testing.Prop.forAll

object RunSpecificTestCases extends App{
  val smallInt: Gen[Int] = Gen.choose(-10,10)

  // No value greater than `xMax` should exist in `xs`
  val maxProp: Prop = forAll(listOf(smallInt)) { xs =>
    val xMax = xs.max
    !xs.exists(_ > xMax)
  }

  // this fails immediately because we start with an empty list!!
  // for which max is not defined
//  Prop.run(maxProp, 10, 10)

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))

  val maxPropThatMightActuallyWork = forAll(listOf1(smallInt)) { xs =>
    val xMax = xs.max
    !xs.exists(_ > xMax)
  }

  Prop.run(maxPropThatMightActuallyWork, 10, 10)

  val sortedProp = forAll(listOf(smallInt))(xs => {
    val sortedList = xs.sorted
    lazy val isEmpty = sortedList.isEmpty
    lazy val hasLengthOfOne = sortedList.tail.isEmpty
    lazy val aPairOfElementsIsUnsorted = sortedList
      .zip(sortedList.tail)
      .exists { case (a,b) => a > b }

    val listsAreSameLength = sortedList.length == xs.length
    val isSorted = isEmpty || hasLengthOfOne || !aPairOfElementsIsUnsorted

    listsAreSameLength && isSorted
  })

  Prop.run(sortedProp, 10, 10)

  val ES = Executors.newWorkStealingPool

  val p2 = Prop.check {
    val parUsingMap = Par.map(Par.unit(1))(_ + 1)
    val parUsingUnit = Par.unit(2)
    parUsingMap(ES).get == parUsingUnit(ES).get
  }

  // this is quite a bit more idiomatic and much much clearer
  val parCheckIdiomatic = Prop.check {
    Par.equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2),
    )(ES).get
  }

  val RandomExecutorService = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> 3,
    unit(Executors.newCachedThreadPool) -> 1,
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(RandomExecutorService ** g) {
      case service ** a => f(a)(service).get
    }

  val parInt = Gen
    .choose(0,10)
    .map(Par.unit)

  val forkProp = forAllPar(parInt)(int =>
    Par.equal(
      Par.fork(int),
      int
    )).markAsFailed("fork")

}
