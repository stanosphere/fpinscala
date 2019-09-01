package experiments

import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

object MonixTask extends App {
  def fib(cycles: Int, a: BigInt, b: BigInt): Task[BigInt] =
    Task.eval(cycles > 0) flatMap {
      case true => fib(cycles - 1, b, a + b)
      case false => Task.now(b)
    }

  fib(20, 1, 1).runOnComplete(println)
}
