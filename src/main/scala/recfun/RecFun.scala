package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(col: Int, row: Int): Int = {
    if (col == 0 || row == col) {
      1
    } else {
      pascal(col - 1, row - 1) + pascal(col, row - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def wereAllBracketsClosed(chars: List[Char], opensCount: Int): Boolean = {
      if (chars.isEmpty) {
        opensCount == 0
      } else {
        val headSymbol: Char = chars.head
        val newOpensCount =
          if (headSymbol == '(') {
            opensCount + 1
          } else if (headSymbol == ')') {
            opensCount - 1
          } else {
            opensCount
          }

        if (newOpensCount >= 0) {
          wereAllBracketsClosed(chars.tail, newOpensCount)
        } else {
          false
        }
      }
    }

    wereAllBracketsClosed(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) {
      0
    } else {
      if (money == 0) {
        1
      } else if (money < 0) {
        0
      }
      else {
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      }
    }
  }

