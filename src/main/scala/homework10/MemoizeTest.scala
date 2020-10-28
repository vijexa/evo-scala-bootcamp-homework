package homework10

object MemoizeTest extends App {
  import cats.Eval

  val eval = 
    Eval.always(println("First expensive computation"))
    .map(_ => println("Second expensive computation"))
    .map(_ => println("Third expensive computation"))
    .memoize
    .map(_ => println("Fourth expensive computation" +
      " that depends on some dynamic variable" +
      " and cannot be memoized"))

  println("Accessing eval for the first time:")
  eval.value
  /*
  First expensive computation
  Second expensive computation
  Third expensive computation
  Fourth expensive computation that depends on some dynamic variable and cannot be memoized
  */
  println("\nAccessing eval for the second time:")
  eval.value
  /*
  Fourth expensive computation that depends on some dynamic variable and cannot be memoized
  */
}
