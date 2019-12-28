package fpinscala.monads

import Reader.readerMonad

object WhatDoesReaderMonadDo extends App {
  val myReader = Reader[String, Int](_.length)

  println(myReader run "Hello")

  val replicants = readerMonad.replicateM(10, myReader)

  println(replicants run "Hello, I am Paul")
  // list of 16s, so it just reads the state each time without updating it

  println(readerMonad.map2(myReader, myReader)(_ * _).run("HELLO"))
  // -> 25 because it reads the same state each time

  val myOtherReader = Reader[String, Int](_ indexOf "I")

  val mySequenceOfReaders = readerMonad.sequence(List(
    myReader,
    myOtherReader,
    myOtherReader,
    myReader,
    myReader,
    myOtherReader
  ))

  println(mySequenceOfReaders run "HIIIIIII")
  // does what you'd expect

}
