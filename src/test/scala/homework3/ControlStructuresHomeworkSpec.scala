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
    assert(parseCommand("multiply " + longString) == Right(Command.Multiply(longList)))
    assert(parseCommand("divide 254 3654") == Right(Command.Divide(254, 3654)))
    assert(parseCommand("average " + longString) == Right(Command.Average(longList)))
    assert(parseCommand("min " + longString) == Right(Command.Min(longList)))
    assert(parseCommand("max " + longString) == Right(Command.Max(longList)))
  }

  {
    val list1 = 1d :: 2d :: 3d :: 4d :: 5d :: Nil
    val list2 = 143d :: -134d :: 45767d :: -341326d :: Nil

    "calculate" should "correctly calculate Sum" in {
      assert(calculate(Command.Sum(list1)) == Right(Result.Sum(list1, 15)))
      assert(calculate(Command.Sum(list2)) == Right(Result.Sum(list2, -295550)))
    }
    
    "calculate" should "correctly calculate Multiply" in {
      assert(calculate(Command.Multiply(list1)) == Right(Result.Multiply(list1, 120)))
      assert(calculate(Command.Multiply(list2)) == Right(Result.Multiply(list2, 299338551458804d)))
    }

    "calculate" should "correctly calculate Average" in {
      assert(calculate(Command.Average(list1)) == Right(Result.Average(list1, 3)))
      assert(calculate(Command.Average(list2)) == Right(Result.Average(list2, -73887.5)))
    }

    "calculate" should "correctly calculate Min" in {
      assert(calculate(Command.Min(list1)) == Right(Result.Min(list1, 1)))
      assert(calculate(Command.Min(list2)) == Right(Result.Min(list2, -341326)))
    }

    "calculate" should "correctly calculate Max" in {
      assert(calculate(Command.Max(list1)) == Right(Result.Max(list1, 5)))
      assert(calculate(Command.Max(list2)) == Right(Result.Max(list2, 45767)))
    }
  }

  "calculate" should "correctly calculate Divide" in {
    assert(calculate(Command.Divide(10, 15)) == Right(Result.Divide(10, 15, 2d/3d)))
    assert(calculate(Command.Divide(100, -10)) == Right(Result.Divide(100, -10, -10)))
    assert(calculate(Command.Divide(42, 2)) == Right(Result.Divide(42, 2, 21)))
    assert(calculate(Command.Divide(0, 13)) == Right(Result.Divide(0, 13, 0)))
    assert(calculate(Command.Divide(0, 0)).isLeft)
    assert(calculate(Command.Divide(345, 0)).isLeft)
  }
  
  "renderResult" should "correctly render result" in {
    assert(
      renderResult(
        Result.Sum(45d::3645d::664d::746d::324d::245d::245d::Nil, 5914d)
      ) == "the sum of 45.0 3645.0 664.0 746.0 324.0 245.0 245.0 is 5914.0"
    )
    assert(
      renderResult(
        Result.Divide(332, 432, 0.7685185185185185)
      ) == "332.0 divided by 432.0 is 0.7685185185185185"
    )
  }
}
