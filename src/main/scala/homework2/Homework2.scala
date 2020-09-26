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