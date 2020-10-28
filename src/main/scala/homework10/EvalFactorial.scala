package homework10

object EvalFactorial extends App {
  import cats.Eval
  import scala.util.chaining._

  val n = 10000

  def factorialNoEval (x: BigInt): BigInt =
    if (x == 0)
      1
    else
      x * factorialNoEval(x - 1)

  def factorialEval1 (x: BigInt): Eval[BigInt] =
    if (x == 0) Eval.now(1)
    else
      Eval.defer(factorialEval1(x - 1).map(_ * x))

  def factorialEval2 (x: BigInt): Eval[BigInt] =
    Eval.now(x == 0).flatMap{
      case true  => Eval.now(1)
      case false => factorialEval2(x - 1).map(_ * x)
    }
  
  try {
    print(s"factorialEval1($n).value: ")
    println(factorialEval1(n).value)
    print(s"factorialEval2($n).value: ")
    println(factorialEval2(n).value)
    print(s"factorialNoEval($n): ")
    println(factorialNoEval(n))
  } catch {
    case _: StackOverflowError => println("Stack overflow!")
  }

}
