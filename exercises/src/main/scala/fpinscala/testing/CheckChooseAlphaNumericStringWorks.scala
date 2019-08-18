package fpinscala.testing

import fpinscala.state.RNG

// I basically just want to check that the strings come back with roughly equal probability
// Just gonna make it generate loads of characters and se what happens lol
object CheckChooseAlphaNumericStringWorks extends App {
  val bigOldString: String = Gen
    .chooseAlphaNumericString(2000)
    .sample
    .run(RNG.Simple(10000))
    ._1

  val frequencyMap = bigOldString
    .groupBy(x => x)
    .mapValues(_.length)

  val listOfFrequencies = frequencyMap
    .toList
    .map(_._2)

  println(listOfFrequencies.min)
  println(listOfFrequencies.max)
}
