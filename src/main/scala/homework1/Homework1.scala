package homework1

object Homework1 {
    def lcm(a: Int, b: Int): Int = scala.math.abs(a * b) / gcd(a, b)

    def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
}
