object pairs {
  def isPrime(n: Int) =
    (2 to (n / 2 + 1)).forall(div => n % div != 0)

  val n = 10
  //  (1 until n).map(i =>
  //    (1 until i).map(j =>
  //      (i, j)
  //    )
  //  )
  //    .flatten

  (1 until n).flatMap(i =>
    (1 until i).map(j =>
      (i, j)
    )
  )
    .filter { case (fst, snd) => isPrime(fst + snd) }

  // or

  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)


  def scalarProduct(xs: List[Double], ys: List[Double]) =
    (for ((x, y) <- xs zip ys) yield x * y).sum

  def queens(n: Int): Set[List[Int]] = {

    def isSafe(column: Int, occupiedColumns: List[Int]) = {
      val row = occupiedColumns.length
      ((row - 1) to 0 by -1)
        .zip(occupiedColumns)
        .forall {
          case (occupiedRow, occupiedColumn) =>
            occupiedColumn != column &&
              math.abs(occupiedColumn - column) != math.abs(occupiedRow - row)
        }
    }

    def placeQueens(k: Int): Set[List[Int]] =
      if (k == 0) Set(List())
      else
        for {
          partialSolution <- placeQueens(k - 1)
          column <- 0 until n
          if isSafe(column, partialSolution)
        } yield column :: partialSolution

    placeQueens(n)
  }

  def show(queens: List[Int]) = {
    val rows = queens.length

    val lines = for (column <- queens.reverse)
      yield (0 until queens.length)
        .map(_ => " * ")
        .updated(column, " X ")
        .mkString

    s"\n${lines.mkString("\n")}\n"
  }

  queens(5)
    .map(show)

}