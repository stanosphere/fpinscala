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
    val rngs = List(1,2,3,4,5,6).map(RNG.Simple(_))
    println(s"the following should all be 100, no matter the generator used")
    rngs.foreach(rng => println(x.sample.run(rng)))
  }

  def checkoutBoolean(): Unit = {
    val b = Gen.boolean
    val rngs = List(2,4,6,8,10,12,14,16,18).map(RNG.Simple(_))
    rngs.foreach(rng => println(b.sample.run(rng)))
  }

  def listyBoi(): Unit = {
    val lstBool = Gen.listOfN(10, Gen.boolean)
    println(lstBool.sample.run(RNG.Simple(12)))

    val lstInt = Gen.listOfN(10, Gen.choose(1,100))
    println(lstInt.sample.run(RNG.Simple(100)))
  }

  def twoInts(): Unit = {
    val two = Gen.chooseTwoInts(1, 10)
    println(two.sample.run(RNG.Simple(1)))
  }

  def makeAListOfStrings(): Unit = {
    val lst = Gen.listOfN(10, Gen.chooseString(12))
    val ss = lst.sample.run(RNG.Simple(1))
    println(ss)
  }

  def tryOutUnion(): Unit = {
    val g1 = Gen.chooseString(1)
    val g2 = Gen.chooseString(10)
    val rngs = List(2,4,6,8,10,12,14,16,18).map(RNG.Simple(_))

    val reses = rngs
      .map(rng => Gen.union(g1, g2).sample.run(rng))
      .map(_._1)
    println(reses)
  }

  def main(): Unit = {
    println(getRandomBetween(1,20))
    checkUnitDoesWhatIThink()
    checkoutBoolean()
    listyBoi()
    twoInts()
    makeAListOfStrings()
    tryOutUnion()
  }

  main()
}
