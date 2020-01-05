package fpinscala.streamingio


object StreamTransducerPlay extends App {

  import SimpleStreamTransducers._
  import SimpleStreamTransducers.Process.{Halt, Await, Emit}

  // this will lift a single value into an await
  val p1 = Process.liftOne((x: Int) => x * 2)

  val xs = p1(Stream(1, 2, 3)).toList
  // let's try to understand what this is doing
  // it's gonna emit a single element (the head) and call that function on it
  // As they phrase it in the book: p1 just waits for one element, emits it, and then stops

  println(xs)
  // => List(2)

  // however we can replace the Halt with a recursive step if we so wish:
  def repeat[I, O](p: Process[I, O]): Process[I, O] = {
    def go(q: Process[I, O]): Process[I, O] = q match {
      case Halt() => go(p)
      case Await(recv) => Await {
        case None => recv(None)
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }

    go(p)
  }

  val ys = repeat(p1)(Stream(1, 2, 3)).toList
  // now that this process has been modified it will repeat p1 on all elements of the stream

  println(ys)
  // => List(2, 4, 6)

  // this looks rather like how a `scanLeft` would work
  // I wonder if it can be generalised?
  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] =
      Await({
        case Some(d) => Emit(d + acc, go(d + acc))
        case None => Halt()
      })

    go(0.0)
  }

  // remember process has an `apply` on it, which is why this syntax for sum works!
  val s = sum(Stream(1.0, 2.0, 3.0, 4.0)).toList
  println(s)

  // check take does what I think
  val myStream = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  println(Process.take(2)(myStream).toList)

  // and the rest

  def lessThanFive(x: Int): Boolean = x < 5

  println(Process.drop(5)(myStream).toList)
  println(Process.takeWhile(lessThanFive)(myStream).toList)
  println(Process.dropWhile(lessThanFive)(myStream).toList)

  println(Process.mean(Stream(1.0, 2.0, 3.0, 4.0)).toList)

  println(Process.sumViaLoop(Stream(1.0, 2.0, 3.0, 4.0)).toList)
  println(Process.countViaLoop(Stream(1.0, 2.0, 3.0, 4.0)).toList)

  // see if my zipWithIndex works
  val p2 = Process.id.zipWithIndex

  println(Process.id.zipWithIndex(Stream("a", "b", "c", 2)).toList)
  // the following does not compile (presumably because I've not defined the type of p2 correctly)
  //  println(p2.zipWithIndex(Stream("a", "b", "c", 2)).toList)

  println(Process.exists((x: Int) => x == 5)(Stream(1, 2, 3, 4, 5, 6, 7)).toList)
  println(Process.exists((x: Int) => x == 10)(Stream(1, 2, 3, 4, 5, 6, 7)).toList)


}
