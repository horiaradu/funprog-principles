object session {

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improveGuess(guess))

    def isGoodEnough(guess: Double) = abs(x - guess * guess) / x < 0.01

    def abs(x: Double) = if (x > 0) x else -x

    def improveGuess(guess: Double) = (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  sqrt(1.0e60)
}
