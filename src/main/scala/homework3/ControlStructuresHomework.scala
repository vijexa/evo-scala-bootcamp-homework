package homework3

import scala.io.Source

object ControlStructuresHomework {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Multiply(numbers: List[Double]) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  sealed trait Result
  object Result {
    final case class Divide(dividend: Double, divisor: Double, result: Double) extends Result
    final case class Multiply(numbers: List[Double], result: Double) extends Result
    final case class Sum(numbers: List[Double], result: Double) extends Result
    final case class Average(numbers: List[Double], result: Double) extends Result
    final case class Min(numbers: List[Double], result: Double) extends Result
    final case class Max(numbers: List[Double], result: Double) extends Result
  }

  def parseCommand(str: String): Either[ErrorMessage, Command] = {
    // Implementation hints:
    // You can use String#split, convert to List using .toList, then pattern match on:
    //   case x :: xs => ???

    // Consider how to handle extra whitespace gracefully (without errors).

    def parseNumbers (list: List[String]): Either[String, List[Double]] = {
      val doubleList = for {
        strDouble <- list
        double    <- strDouble.toDoubleOption
      } yield double
      
      if (doubleList.length != list.length) {
        Left("non-numbers detected")
      } else if (doubleList.length < 2) {
        Left("you should specify at least 2 arguments")
      } else {
        Right(doubleList)
      }
    }
    
    str.replaceAll("\\s+"," ").split(" ").toList match {
      case x :: xs => parseNumbers(xs) match {
        case Right(parsedN) => x match {
          case "divide"   => {
            if(parsedN.length == 2) Right(Command.Divide(parsedN(0), parsedN(1)))
            else Left(ErrorMessage(s"divide should have exactly 2 arguments, it has ${parsedN.length}"))
          }
          case "sum"      => Right(Command.Sum(parsedN))
          case "multiply" => Right(Command.Multiply(parsedN))
          case "average"  => Right(Command.Average(parsedN))
          case "min"      => Right(Command.Min(parsedN))
          case "max"      => Right(Command.Max(parsedN))
          case _          => Left(ErrorMessage(s"unexpected command: $x"))
        }
        case Left(e) => Left(ErrorMessage(s"Numbers parse failed: $e"))
      }
      case Nil => Left(ErrorMessage("Parse failed: empty"))
    }
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Command.Divide(dividend, divisor)  => divisor match {
        case 0 => Left(ErrorMessage("division by zero"))
        case _ => Right(Result.Divide(dividend, divisor, dividend / divisor))
      }
      case Command.Sum(numbers)               => Right(Result.Sum(numbers, numbers.sum))
      case Command.Multiply(numbers)          => Right(Result.Multiply(numbers, numbers.reduce(_*_)))
      case Command.Average(numbers)           => Right(Result.Average(numbers, numbers.sum / numbers.length))
      case Command.Min(numbers)               => Right(Result.Min(numbers, numbers.min))
      case Command.Max(numbers)               => Right(Result.Max(numbers, numbers.max))
    }
  }

  def renderResult(x: Result): String = {
    x match {
      case Result.Divide(dividend, divisor, result) => s"$dividend divided by $divisor is $result"
      case Result.Sum(numbers, result) => s"the sum of ${numbers.mkString(" ")} is $result"
      case Result.Multiply(numbers, result) => s"the multiplication of ${numbers.mkString(" ")} is $result"
      case Result.Average(numbers, result) => s"the average of ${numbers.mkString(" ")} is $result"
      case Result.Min(numbers, result) => s"the minimum of ${numbers.mkString(" ")} is $result"
      case Result.Max(numbers, result) => s"the maximum of ${numbers.mkString(" ")} is $result"
    }
  }

  def process(x: String): String = {
    val result = for {
      command <- parseCommand(x)
      result  <- calculate(command)
    } yield result
    
    result.fold(
      s"${Console.RED}Error: " + _.value + Console.RESET,
      renderResult(_)
    )
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
