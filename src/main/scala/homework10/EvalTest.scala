package homework10

object EvalTest extends App {
  import cats.Eval

  def superExpensiveCalculation (a: Int, b: Int) = a + b

  val evalNow = Eval.now {
    println(">>>Evaluating evalNow...")
    superExpensiveCalculation(2, 2)
  }

  val evalLater = Eval.later {
    println(">>>Evaluating evalLater...")
    superExpensiveCalculation(2, 2)
  }

  val evalAlways = Eval.always {
    println(">>>Evaluating evalAlways...")
    superExpensiveCalculation(2, 2)
  }

  val evalDefer = Eval.defer {
    println(">>>Evaluating evalDefer...")
    Eval.now {
      superExpensiveCalculation(2, 2)
    }
  }

  println("Evals are already initialised but their value haven't been used yet")
  println()
  println(s"evalNow value is ${evalNow.value}")
  println(s"evalNow value is ${evalNow.value}")
  println()
  println(s"evalLater value is ${evalLater.value}")
  println(s"evalLater value is ${evalLater.value}")
  println()
  println(s"evalAlways value is ${evalAlways.value}")
  println(s"evalAlways value is ${evalAlways.value}")
  println()
  println(s"evalDefer value is ${evalDefer.value}")
  println(s"evalDefer value is ${evalDefer.value}")
}
