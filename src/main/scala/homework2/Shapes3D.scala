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

object Shapes3D {
  sealed trait Shape3D
      extends Bounded3D
      with Located3D
      with Movable3D
      with Describable3D
      with Geometric3D

  sealed trait Geometric3D {
    def surfaceArea: Double
    def volume: Double
  }

  sealed trait Bounded3D {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Located3D {
    def position: Point3D
  }

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Movable3D
  }

  sealed trait Describable3D {
    def describeShape: String
  }

  final case class Point3D(x: Double, y: Double, z: Double)
      extends Movable3D
      with Describable3D {

    def move(dx: Double, dy: Double, dz: Double) =
      Point3D(x + dx, y + dy, z + dz)

    val describeShape: String = s"x = $x, y = $y, z = $z"
  }

  // x, y, z are the sphere center coordinates
  final case class Sphere(x: Double, y: Double, z: Double, radius: Double)
      extends Shape3D {

    require(radius > 0, "radius should be greater than 0")

    val position: Point3D = Point3D(x, y, z)
    val minX: Double = x - radius
    val maxX: Double = x + radius
    val minY: Double = y - radius
    val maxY: Double = y + radius
    val minZ: Double = z - radius
    val maxZ: Double = z + radius

    def move(dx: Double, dy: Double, dz: Double) =
      copy(x = x + dx, y = y + dy, z = z + dz)
    val describeShape: String = s"x = $x, y = $y, z = $z, radius = $radius"

    lazy val surfaceArea: Double = 4 * scala.math.Pi * radius * radius
    lazy val volume: Double = (4 * scala.math.Pi * radius * radius * radius) / 3
  }

  final case class Cube(x: Double, y: Double, z: Double, side: Double)
      extends Shape3D {

    require(side > 0, "side should be greater than 0")

    val position: Point3D = Point3D(x, y, z)
    val minX: Double = x
    val maxX: Double = x + side
    val minY: Double = y
    val maxY: Double = y + side
    val minZ: Double = z
    val maxZ: Double = z + side

    def move(dx: Double, dy: Double, dz: Double) =
      copy(x = x + dx, y = y + dy, z = z + dz)
    val describeShape: String = s"x = $x, y = $y, z = $z, side = $side"

    lazy val surfaceArea: Double = 6 * side * side
    lazy val volume: Double = side * side * side
  }

  final case class Cuboid(
      x: Double,
      y: Double,
      z: Double,
      sidex: Double,
      sidey: Double,
      sidez: Double
  ) extends Shape3D {

    require(
      sidex > 0 && sidey > 0 && sidez > 0,
      "sides should be greater than 0"
    )

    val position: Point3D = Point3D(x, y, z)
    val minX: Double = x
    val maxX: Double = x + sidex
    val minY: Double = y
    val maxY: Double = y + sidey
    val minZ: Double = z
    val maxZ: Double = z + sidez

    def move(dx: Double, dy: Double, dz: Double) =
      copy(x = x + dx, y = y + dy, z = z + dz)
    val describeShape: String =
      s"x = $x, y = $y, z = $z, sidex = $sidex, sidey = $sidey, sidez = $sidez"

    lazy val surfaceArea: Double =
      2 * (sidey * sidex + sidez * (sidey + sidex))
    lazy val volume: Double = sidex * sidey * sidez
  }

  // the tetrahedron is regular and lies flat on the xy surface with one side aligned with x axis
  final case class RegTetrahedron(x: Double, y: Double, z: Double, side: Double)
      extends Shape3D {

    require(side > 0, "side should be greater than 0")

    val position: Point3D = Point3D(x, y, z)
    val minX: Double = x
    val maxX: Double = x + side
    val minY: Double = y
    // height of regular triangle
    val maxY: Double = y + ((scala.math.sqrt(3) * side) / 2)
    val minZ: Double = z
    // height of regular tetrahedron
    val maxZ: Double = z + ((scala.math.sqrt(6) * side) / 3)

    def move(dx: Double, dy: Double, dz: Double) =
      copy(x = x + dx, y = y + dy, z = z + dz)
    val describeShape: String = s"x = $x, y = $y, z = $z, side = $side"

    lazy val surfaceArea: Double = scala.math.sqrt(3) * side * side
    lazy val volume: Double = (scala.math.sqrt(2) * side * side * side) / 12
  }
}
