package homework2

class Homework2Spec extends org.scalatest.FunSuite {
  val testPrecision = 0.01
  def ~=(a: Double, b: Double) = 
    if ((a - b).abs < testPrecision) true else false

  /////////////////////////////////
  test("Point.describeShape") {
    assert(
      Point(3, 6).describeShape
        == "x = 3.0, y = 6.0")
  }

  test("Circle.describeShape") {
    assert(
      Circle(5, 2, 20).describeShape
        == "x = 5.0, y = 2.0, radius = 20.0")
  }

  test("Rectangle.describeShape") {
    assert(
      Rectangle(2, 3, 10, 15).describeShape
        == "x = 2.0, y = 3.0, width = 10.0, height = 15.0")
  }

  test("Square.describeShape") {
    assert(
      Square(9, -3, 7).describeShape
        == "x = 9.0, y = -3.0, side = 7.0")
  }

  test("Triangle.describeShape") {
    assert(
      Triangle(0, 0, 3, 4, 5).describeShape
        == "point1(x = 0.0, y = 0.0), point2(x = 3.0, y = 0.0), " +
          "point3(x = 3.0, y = 4.0), sideA = 3.0, sideB = 4.0, " +
          "sideC = 5.0, minX = 0.0, maxX = 3.0, minY = 0.0, maxY = 4.0")
  }
  //////////////////////////////////////////////
  test("Circle.position"){
    assert(
      Circle(9, 4, 10).position == Point(9, 4))
  }

  test("Rectangle.position"){
    assert(
      Rectangle(3, 6, 4, 5).position == Point(3, 6))
  }

  test("Square.position"){
    assert(
      Square(2, 5, 5).position == Point(2, 5))
  }

  test("Triangle.position"){
    assert(
      Triangle(2, 3, 4, 5, 6).position == Point(2, 3))
  }
  //////////////////////////////////////////////
  test("Point.move"){
    assert(
      Point(2, 5).move(2, 1) == Point(4, 6))
    assert(
      Point(10, 20).move(-20, -35) == Point(-10, -15))
    assert(
      Point(3, 4).move(0, 0) == Point(3, 4))
  }
  
  test("Circle.move"){
    assert(
      Circle(9, 4, 10).move(5, -3) == Circle(14, 1, 10))
    assert(
      Circle(-4, -5, 7).move(4, 5) == Circle(0, -0, 7))
    assert(
      Circle(4, 8, 6).move(0, 0) == Circle(4, 8, 6))
  }

  test("Rectangle.move"){
    assert(
      Rectangle(3, 6, 4, 5).move(4, 6) == Rectangle(7, 12, 4, 5))
    assert(
      Rectangle(-5, -10, 43, 123).move(52, 59) == Rectangle(47, 49, 43, 123))
    assert(
      Rectangle(0, 1, 1, 1).move(0, 0) == Rectangle(0, 1, 1, 1))
  }

  test("Square.move"){
    assert(
      Square(2, 5, 5).move(2, -7) == Square(4, -2, 5))
    assert(
      Square(-56, -4, 7).move(200, 5) == Square(144, 1, 7))
    assert(
      Square(5, 3, 1).move(0, 0) == Square(5, 3, 1))
  }

  test("Triangle.move"){
    assert(
      Triangle(2, 3, 4, 5, 6).move(15, 4) == Triangle(17, 7, 4, 5, 6))
    assert(
      Triangle(-5, -3, 67, 34, 55).move(67, -32) == Triangle(62, -35, 67, 34, 55))
    assert(
      Triangle(54, -37, 5, 5, 5).move(43, 21) == Triangle(97, -16, 5, 5, 5))
  }
  //////////////////////////////////////////////
  test("Circle.apply") {
    assertThrows[IllegalArgumentException] {
      Circle(46, 34, -1)
    }
    assertThrows[IllegalArgumentException] {
      Circle(46, 34, 0)
    }
    assert(
      Circle(-4, -6, 10).isInstanceOf[Circle])
  }

  test("Rectangle.apply") {
    assertThrows[IllegalArgumentException] {
      Rectangle(23, 4, -1, 4)
    }
    assertThrows[IllegalArgumentException] {
      Rectangle(4, -10, 15, -3)
    }
    assertThrows[IllegalArgumentException] {
      Rectangle(-54, -56, -9, -13)
    }
    assert(
      Rectangle(-4, 12, 4, 5).isInstanceOf[Rectangle])
  }
  
  test("Square.apply") {
    assertThrows[IllegalArgumentException] {
      Square(15, -7, -10)
    }
    assertThrows[IllegalArgumentException] {
      Square(5, 2, 0)
    }
    assert(
      Square(9, -6, 10).isInstanceOf[Square])
  }

  test("Triangle.apply") {
    assertThrows[IllegalArgumentException] {
      Triangle(3, 2, 4, 5, 1000000)
    }
    assertThrows[IllegalArgumentException] {
      Triangle(3, 2, 4, 1000000, 2)
    }
    assertThrows[IllegalArgumentException] {
      Triangle(3, 2, 1000000, 5, 7)
    }
    assertThrows[IllegalArgumentException] {
      Triangle(4, 8, -4, 5, 1)
    }
    assert(
      Triangle(9, 7, 3, 4, 5).isInstanceOf[Triangle])
  }
  //////////////////////////////////////////////
  test("Triangle.minX") {
    assert(
      ~=(Triangle(0, 0, 3, 4, 5).minX, 0))
    assert(
      ~=(Triangle(0, 0, 5, 8.60, 5.38).minX, -2))
    assert(
      ~=(Triangle(0, 0, 5, 5.38, 8.60).minX, 0))
    assert(
      ~=(Triangle(12, 9, 5, 5.38, 8.60).minX, 12))
    assert(
      ~=(Triangle(-12, -9, 5, 5.38, 8.60).minX, -12))
  }

  test("Triangle.maxX") {
    assert(
      ~=(Triangle(0, 0, 3, 4, 5).maxX, 3))
    assert(
      ~=(Triangle(0, 0, 5, 8.60, 5.38).maxX, 5))
    assert(
      ~=(Triangle(0, 0, 5, 5.38, 8.60).maxX, 7))
    assert(
      ~=(Triangle(5, 7, 5, 5.38, 8.60).maxX, 12))
    assert(
      ~=(Triangle(-5, -7, 5, 5.38, 8.60).maxX, 2))
  }

  test("Triangle.maxY") {
    assert(
      ~=(Triangle(0, 0, 3, 4, 5).maxY, 4))
    assert(
      ~=(Triangle(0, 0, 5, 8.60, 5.38).maxY, 5))
    assert(
      ~=(Triangle(0, 0, 5, 5.38, 8.60).maxY, 5))
    assert(
      ~=(Triangle(10, 7, 5, 5.38, 8.60).maxY, 12))
    assert(
      ~=(Triangle(-10, -7, 5, 5.38, 8.60).maxY, -2))
  }
  //////////////////////////////////////////////
  test("Circle.perimeter") {
    assert(
      ~=(Circle(5, 3, 5).perimeter, 31.42))
    assert(
      ~=(Circle(2, 7, 10).perimeter, 62.83))
    assert(
      ~=(Circle(-7, 0, 15).perimeter, 94.25))
  }
  
  test("Rectangle.perimeter") {
    assert(
      ~=(Rectangle(5, 2, 5, 14).perimeter, 5*2 + 14*2))
    assert(
      ~=(Rectangle(4, 67, 10, 34).perimeter, 10*2 + 34*2))
    assert(
      ~=(Rectangle(-10, 4, 15, 7).perimeter, 15*2 + 7*2))
  }
  
  test("Square.perimeter") {
    assert(
      ~=(Square(1, 6, 12).perimeter, 12 * 4))
    assert(
      ~=(Square(43, 5, 4).perimeter, 4 * 4))
    assert(
      ~=(Square(-54, -43, 25).perimeter, 25 * 4))
  }
  
  test("Triangle.perimeter") {
    assert(
      ~=(Triangle(2, 4, 15, 13, 5).perimeter, 33))
    assert(
      ~=(Triangle(45, 43, 37, 56, 34).perimeter, 127))
    assert(
      ~=(Triangle(-12, -55, 12, 23, 15).perimeter, 50))
  }
  //////////////////////////////////////////////
  test("Circle.area") {
    assert(
      ~=(Circle(1, 4, 5).area, 78.54))
    assert(
      ~=(Circle(2, 6, 10).area, 314.16))
    assert(
      ~=(Circle(-9, -4, 15).area, 706.86))
  }

  test("Rectangle.area") {
    assert(
      ~=(Rectangle(5, 2, 5, 14).area, 5 * 14))
    assert(
      ~=(Rectangle(4, 67, 10, 34).area, 10 * 34))
    assert(
      ~=(Rectangle(-10, 4, 15, 7).area, 15 * 7))
  }
  
  test("Square.area") {
    assert(
      ~=(Square(1, 6, 12).area, 12 * 12))
    assert(
      ~=(Square(43, 5, 4).area, 4 * 4))
    assert(
      ~=(Square(-54, -43, 25).area, 25 * 25))
  }
  
  test("Triangle.area") {
    assert(
      ~=(Triangle(2, 4, 15, 13, 5).area, 31.56))
    assert(
      ~=(Triangle(45, 43, 37, 56, 34).area, 610.17))
    assert(
      ~=(Triangle(-12, -55, 12, 23, 15).area, 80.62))
  }
}