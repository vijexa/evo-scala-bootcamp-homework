package homework10

object MutualRec2 extends App {
  import cats.Eval
  import scala.util.chaining._

  object NoEval {
    def findCharWithFinder (
      f: (Char, String) => Boolean,
      str: String
    ): Boolean = 
      if (0 < str.length) f(str.head, str drop 1)
      else false

    val fFinder: (Char, String) => Boolean = 
      (c, str) => 
        if (c == 'f') true 
        else findCharWithFinder(fFinder, str)

    val hFinder: (Char, String) => Boolean = 
      (c, str) => 
        if (c == 'h') true 
        else findCharWithFinder(hFinder, str)
  }

  try {
    print("Finding f without using eval: ")
    println(
      NoEval.findCharWithFinder (
        NoEval.fFinder,
        List.fill(1_000)("abcdefg").mkString
      ) // returns true
    )

    print("Finding h without using eval: ")
    println(
      NoEval.findCharWithFinder(
        NoEval.hFinder,
        List.fill(1_000)("abcdefg").mkString
      ) // throws StackOverflowException
    )
  } catch {
    case _: StackOverflowError => println("Oops, stackoverflow")
  }
    
  object UsingEval {
    def findCharWithFinder (
      f: (Char, String) => Eval[Boolean],
      str: String
    ): Eval[Boolean] = 
      Eval.always(0 < str.length).flatMap{
        case true  => f(str.head, str drop 1)
        case false => Eval.False
      }

    val fFinder: (Char, String) => Eval[Boolean] = 
      (c, str) => 
        Eval.always(c == 'f').flatMap{
          case true  => Eval.True
          case false => findCharWithFinder(fFinder, str)
        }

    val hFinder: (Char, String) => Eval[Boolean] = 
      (c, str) => 
        Eval.always(c == 'h').flatMap{
          case true  => Eval.True
          case false => findCharWithFinder(hFinder, str)
        }
  }

  print("Finding f with eval: ")
  println(
    UsingEval.findCharWithFinder (
      UsingEval.fFinder,
      List.fill(1_000)("abcdefg").mkString
    ).value
  ) // returns true

  print("Finding h with eval: ")
  println(
    UsingEval.findCharWithFinder (
      UsingEval.hFinder,
      List.fill(1_000)("abcdefg").mkString
    ).value
  ) // returns false, and no stack overflow!

}
