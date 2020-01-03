package fpinscala.localeffects

object LocalEffectsPlay extends App {

  val p = new RunnableST[(Int, Int)] {
    def apply[S]: ST[S, (Int, Int)] = for {
      r1 <- STRef(1)
      r2 <- STRef(2)
      x <- r1.read
      y <- r2.read
      _ <- r1.write(y + 1)
      _ <- r2.write(x + 1)
      a <- r1.read
      b <- r2.read
    } yield (a, b)
  }

  val r = ST.runST(p)
  println(r)

  val runnable = new RunnableST[Int] {
    override def apply[S]: ST[S, Int] =
      for {
        arr <- STArray(10, 20)
        x <- arr.read(3)
      } yield x
  }


  val res = ST.runST(runnable)
  println(res)

}
