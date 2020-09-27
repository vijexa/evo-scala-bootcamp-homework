package homework4

import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

import homework4.Homework4._

class Homework4Spec extends AnyFlatSpec with should.Matchers {
  "vsortConsideringEqualValues" should "return correct list" in {
    val map1 = Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)
    val list1 = List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)

    val map2 = Map("abc" -> 8, "def" -> 4, "ghj" -> 0, "test" -> 6, "fdf" -> 3, "qwer" -> 1, "wasd" -> 3, "scala" -> 0)
    val list2 = List(Set("scala", "ghj") -> 0, Set("qwer") -> 1, Set("fdf", "wasd") -> 3, Set("def") -> 4, Set("test") -> 6, Set("abc") -> 8)

    sortConsideringEqualValues(map1) shouldBe list1
    sortConsideringEqualValues(map2) shouldBe list2
  }
}
