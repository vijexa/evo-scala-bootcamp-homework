package homework3

import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

import homework3.ControlStructuresHomework._

class ControlStructuresHomeworkSpec extends AnyFlatSpec with should.Matchers {

  "parseCommand" should "return Left" in {
    assert(parseCommand("sfghfg 1234 314").isLeft)
    assert(parseCommand("sum 254").isLeft)
    assert(parseCommand("average").isLeft)
    assert(parseCommand("").isLeft)
    assert(parseCommand("divide 2543 632 4563").isLeft)
  }

  "parseCommand" should "return correct Command" in {
    val longString = "1134 7584 4641 1432 13423"
    val longList: List[Double] = 1134d :: 7584d :: 4641d :: 1432d :: 13423d :: Nil

    assert(parseCommand("sum " + longString) == Right(Command.Sum(longList)))
    assert(parseCommand("divide 254 3654") == Right(Command.Divide(254, 3654)))
    assert(parseCommand("average " + longString) == Right(Command.Average(longList)))
    assert(parseCommand("min " + longString) == Right(Command.Min(longList)))
    assert(parseCommand("max " + longString) == Right(Command.Max(longList)))
  }
}
