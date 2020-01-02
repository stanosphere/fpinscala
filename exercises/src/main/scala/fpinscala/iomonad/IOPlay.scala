package fpinscala.iomonad

import java.util.concurrent.{ExecutorService, Executors}

object IOPlay extends App {
  implicit val ES: ExecutorService = Executors.newCachedThreadPool

  val x = Return(1)

}
