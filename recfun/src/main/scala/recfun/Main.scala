package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  //  def pascal(c: Int, r: Int): Int = {
  //    def store(matrix: Array[Array[Int]], c: Int, r: Int, value: Int) = {
  //      matrix(c)(r) = value
  //      matrix
  //    }
  //
  //    def pascalRec(c: Int, r: Int, acc: Array[Array[Int]], result: Int): (Array[Array[Int]], Int) =
  //      if (c == 0 || c == r) (store(acc, c, r, 1), result + 1)
  //      else if (c < 0 || c > r) (store(acc, c, r, 0), result)
  //      else {
  //        val (newAcc, newResult) = pascalRec(c - 1, r - 1, acc, result)
  //        pascalRec(c, r - 1, newAcc, newResult)
  //      }
  //
  //    val (_, result) = pascalRec(c, r, Array.ofDim[Int](c + 1, r + 1), 0)
  //    result
  //  }

  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else if (c < 0 || c > r) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def charToIncrement(char: Char) =
      if (char == '(') 1
      else if (char == ')') -1
      else 0

    @tailrec
    def balanceRec(chars: List[Char], count: Int): Boolean =
      if (count < 0) false
      else chars match {
        case char :: rest => {
          val nextCount = charToIncrement(char) + count
          balanceRec(rest, nextCount)
        }
        case _ => count == 0
      }

    balanceRec(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money < 0) 0
    else if (money == 0) 1
    else coins match {
      case coin :: rest => countChange(money - coin, coins) + countChange(money, rest)
      case _ => 0
    }
}
