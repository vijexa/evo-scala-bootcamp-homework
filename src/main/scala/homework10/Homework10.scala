package homework10

object Homework10 extends App {
  import cats.Eval
  import scala.util.chaining._

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

  import scala.annotation.tailrec
  object MutualRecursion {
    def even(n: Int): Boolean =
      if (n == 0) true
      else odd(n - 1)

    def odd(n: Int): Boolean =
      if (n == 0) true
      else even(n - 1)
  }

  object MutualRecursionEval {
    def even(n: Int): Eval[Boolean] =
      Eval.always(n == 0).flatMap {
        case true => Eval.True
        case false => odd(n - 1)
      }

    def odd(n: Int): Eval[Boolean] =
      Eval.always(n == 0).flatMap {
        case true => Eval.False
        case false => even(n - 1)
      }
  }

  println("Trying MutualRecursion.odd(199999)...")
  try {
    println(MutualRecursion.odd(199999))
  } catch {
    case _: StackOverflowError => 
      println(
        "– I CAN'T FEEL MY STACK!\n" +
        "– Bubba... It ain't there..."
      )
  }

  println()

  println("Trying MutualRecursionEval.odd(199999)...")
  println(
    s"In this case Bubba's stack is fine and we have our value: " +
    s"${MutualRecursionEval.odd(199999).value}"
  )
}
