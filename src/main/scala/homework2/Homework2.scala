package homework2

// Homework
//
// Add additional 2D shapes such as triangle and square.
//
// In addition to the 2D shapes classes, add also 3D shapes classes
// (origin, point, sphere, cube, cuboid, 3D triangle - you can add
// others if you think they are a good fit).
//
// Add method `area` to 2D shapes.
//
// Add methods `surfaceArea` and `volume` to 3D shapes.
//
// If some of the implementation involves advanced math, it is OK
// to skip it (leave unimplemented), the primary intent of this
// exercise is modelling using case classes and traits, and not math.

sealed trait Shape extends Located with Bounded with Movable {
  def describeShape: String
}

sealed trait Located {
  def x: Double
  def y: Double
}

sealed trait Bounded {
  def minX: Double
  def maxX: Double
  def minY: Double
  def maxY: Double
}

sealed trait Movable {
  def move (dx: Double, dy: Double): Shape 
}

final case class Point(x: Double, y: Double) extends Shape {
  def minX: Double = x
  def maxX: Double = x
  def minY: Double = y
  def maxY: Double = y

  def move (dx: Double, dy: Double) = Point(x + dx, y + dy)
  def describeShape: String = s"x = $x, y = $y"
}


final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape {
  require(radius > 0, "radius must be greater than 0")

  def x: Double = centerX
  def y: Double = centerY
  def minX: Double = centerX - radius
  def maxX: Double = centerX + radius
  def minY: Double = centerY - radius
  def maxY: Double = centerY + radius

  def move (dx: Double, dy: Double) = Circle(x + dx, y + dy, radius)
  def describeShape: String = s"x = $centerX, y = $centerY, radius = $radius"
}

final case class Rectangle(x: Double, y: Double, width: Double, height: Double) extends Shape {
  require(width > 0, "width must be greater than 0")
  require(height > 0, "height must be greater than 0")

  def minX: Double = x
  def maxX: Double = x + width
  def minY: Double = y
  def maxY: Double = y + height

  def move (dx: Double, dy: Double) = Rectangle(x + dx, y + dy, width, height)
  def describeShape: String = s"x = $x, y = $y, width = $width, height = $height"
}

final case class Square(x: Double, y: Double, side: Double) extends Shape {
  require(side > 0, "side should be greater than 0")

  def minX: Double = x
  def maxX: Double = x + side
  def minY: Double = y
  def maxY: Double = y + side

  def move (dx: Double, dy: Double) = Square(x + dx, y + dy, side)
  def describeShape: String = s"x = $x, y = $y, side = $side"
}

//          |\ 
//  sideC  |  \  sideB 
//        |____\
//   (x,y) sideA
// 
// sideA is always horizontal (every point on sideA line has the same y value)
final case class Triangle(x: Double, y: Double, sideA: Double, sideB: Double, sideC: Double) extends Shape {
  require(
    sideA + sideB > sideC && sideB + sideC > sideA && sideA + sideC > sideB, 
    "the triangle is non-existent")
  require(
    sideA > 0 && sideB > 0 && sideC > 0,
    "sides should be greater than 0")

  def point1: Point = Point(x, y)
  def point2: Point = Point(x + sideA, y)
  def point3: Point = {
    val x = (sideA * sideA + sideC * sideC - sideB * sideB) / (2 * sideA)
    val y = scala.math.sqrt(sideC * sideC - x * x)

    Point(x, y)
  }

  def minX: Double = if (point1.x < point3.x) point1.x else point3.x
  def maxX: Double = if (point2.x > point3.x) point2.x else point3.x
  def minY: Double = point1.y
  def maxY: Double = point3.y

  def move(dx: Double, dy: Double): Shape = Triangle(x + dx, y + dy, sideA, sideB, sideC)
  def describeShape: String = s"Point1(${point1.describeShape}), " +
    s"Point2(${point2.describeShape}), Point3(${point3.describeShape}), " +
    s"sideA = $sideA, sideB = $sideB, sideC = $sideC, " +
    s"minX = $minX, maxX = $maxX, minY = $minY, maxY = $maxY" 
}