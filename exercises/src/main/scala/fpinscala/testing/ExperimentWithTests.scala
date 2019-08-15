package fpinscala.testing

import fpinscala.state.RNG

object ExperimentWithTests extends App {
  def getRandomBetween(x: Int, y: Int): Int = {
    val chosen = Gen.choose(x, y)
    val int = chosen.sample.run(RNG.Simple(1))
    int._1
  }

  def checkUnitDoesWhatIThink(): Unit = {
    val x = Gen.unit(100)
    val rngs = List(1,2,3,4,5,6).map(x => RNG.Simple(x))
    println(s"the following should all be 100, no matter the generator used")
    rngs.foreach(rng => println(x.sample.run(rng)))
  }

  def main(): Unit = {
    println(getRandomBetween(1,20))
    checkUnitDoesWhatIThink()
  }

  main()
}
