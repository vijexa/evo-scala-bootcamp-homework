package homework10

object MutualRec extends App {
  import cats.Eval

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
