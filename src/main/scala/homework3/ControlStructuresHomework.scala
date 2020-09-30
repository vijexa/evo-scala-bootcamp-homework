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
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  sealed trait Result
  final case class ChangeMe(value: String) extends Result // adjust Result as required to match requirements

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
    ??? // implement this method
  }

  def renderResult(x: Result): String = {
    ??? // implement this method
  }

  def process(x: String): String = {
    import cats.implicits._
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.

    parseCommand(x).toString // implement using a for-comprehension
/* 
    for {
      command <- parseCommand(x)
      result <- calculate(command)
      renderedRes <- renderResult(result)
    } yield renderedRes */

    // parseCommand(x)
    // calculate(command)
    // renderResult()
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
