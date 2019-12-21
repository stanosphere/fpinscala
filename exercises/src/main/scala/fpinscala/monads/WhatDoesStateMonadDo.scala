package fpinscala.monads

import fpinscala.state._
import Monad.stateMonadFromAnswer


object WhatDoesStateMonadDo extends App {

  val myState = State[String, Int](s => if (s == "") (0, "") else (s.length, s.tail))

  val q = myState.run("paul")
  println(q)

  val replicants = stateMonadFromAnswer.replicateM(10, myState)

  // looks like replicateM will run the `run` function n times and put the `A`s into a list
  // and also return the final state after n applications
  // the cool  it is that it's threading through the previous state to the next call
  println(replicants.run("Hello, My Name is Paul"))

  // this is vaguely similar to what replicateM did
  // it threads state transition through
  // but rather than putting the results in a list it combines them with a function
  println(stateMonadFromAnswer.map2(myState, myState)(_ * _).run("HELLO"))
  // -> (20, LLO)

  // well this is rather boring and will just be the same as replicateM
  val sequence = stateMonadFromAnswer.sequence(List(myState, myState, myState))

  val myOtherState = State[String, Int](s => (s.length * 2, s * 2))

  val interestingSequence = stateMonadFromAnswer
    .sequence(List(
      myState,
      myOtherState,
      myOtherState,
      myState,
      myOtherState,
      myState,
      myState
    ))

  // this makes a lot of sense
  // its threading through the same sting and applying the state transitions in order
  println(interestingSequence.run("wowzas"))

  val myStateOfTheWrongType = State[Double, Double](x => (x, x))

  // if you try the below you get a runtime error of the types being wrong
  // I kind of wish there was a way to do this at compile time

  //  val naughty = stateMonadFromAnswer
  //    .sequence(List(myState, myStateOfTheWrongType))
  //
  //  println(naughty)
}
