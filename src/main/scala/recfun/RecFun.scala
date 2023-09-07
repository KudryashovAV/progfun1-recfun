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
  def pascal(col: Int, row: Int): Int =
    if (col == 0 || row == col)
      1
    else
      pascal(col - 1, row - 1) + pascal(col, row - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def wereAllBracketsClosed(chars: List[Char], opensCount: Int): Boolean =
      if (chars.isEmpty) return opensCount == 0
      if (opensCount < 0) return false

      var newOpensCount: Int = opensCount

      if (chars.head == '(')
        newOpensCount += 1

      if (chars.head == ')')
        newOpensCount -= 1

      wereAllBracketsClosed(chars.tail, newOpensCount)

    wereAllBracketsClosed(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (coins.isEmpty || money < 0) return 0
    if (money == 0) return  1

    countChange(money - coins.head, coins) + countChange(money, coins.tail)

@main def hello() = println("Hello, World")