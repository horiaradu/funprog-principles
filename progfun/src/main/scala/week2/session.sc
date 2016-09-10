import math.abs

object session {
  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)

  product(x => x * x)(1, 3)

  def factorial(n: Int) = product(x => x)(1, n)

  factorial(4)

  def mapReduce(combine: ((Int, Int) => Int), unit: Int)(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) unit
    else combine(f(a), mapReduce(combine, unit)(f)(a + 1, b))

  mapReduce((x, y) => x * y, 1)(x => x * x)(1, 3)

  /** ************************/

  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }

    iterate(firstGuess)
  }

  fixedPoint(x => 1 + x / 2)(1)

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def sqrt(x: Double) = fixedPoint(averageDamp(y => x / y))(1)

  sqrt(4)
  sqrt(2)
}