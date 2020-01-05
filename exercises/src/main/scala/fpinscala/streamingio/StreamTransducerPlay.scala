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

}
