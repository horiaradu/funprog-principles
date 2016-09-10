object rationals {
  val x = new Rational(1, 2)
  x.numer
  x.denom

  val y = new Rational(1, 4)

  x + y

  x - y

  y.max(x)

//  val strange = new Rational(1, 0)

  new Rational(4)

  class Rational(x: Int, y: Int) {
    require(y != 0, "denom must be non zero")

    def this(x: Int) = this(x, 1)

    private val divisor = gcd(x, y)

    val numer = x / divisor

    val denom = y / divisor

    def +(other: Rational) =
      new Rational(numer * other.denom + other.numer * denom, denom * other.denom)

    def -(other: Rational) = this.+(-other)

    def unary_- = new Rational(-numer, denom)

    def less(other: Rational) =
      numer * other.denom < other.numer * denom

    def max(other: Rational) =
      if (less(other)) other
      else this

    override def toString = numer + "/" + denom
  }

  private def gcd(x: Int, y: Int): Int =
    if (y == 0) x
    else gcd(y, x % y)
}
