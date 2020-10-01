package homework1

class Homework1Spec extends org.scalatest.funsuite.AnyFunSuite {
  test("Homework1.lcm") {
    assert(Homework1.lcm(6, 9) === 18)
    assert(Homework1.lcm(3, 9) === 9)
    assert(Homework1.lcm(4, 8) === 8)
    assert(Homework1.lcm(25, 10) === 50)
    assert(Homework1.lcm(100, 10) === 100)
  }

  test("Homework1.gcd") {
    assert(Homework1.gcd(8, 12) === 4)
    assert(Homework1.gcd(6, 9) === 3)
    assert(Homework1.gcd(3, 9) === 3)
    assert(Homework1.gcd(4, 8) === 4)
    assert(Homework1.gcd(25, 10) === 5)
    assert(Homework1.gcd(100, 10) === 10)
  }
}