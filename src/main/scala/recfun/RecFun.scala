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
    if col == 0 || row == col then
      1
    else
      pascal(col - 1, row - 1) + pascal(col, row - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    @tailrec
    def wereAllBracketsClosed(chars: List[Char], opensCount: Int): Boolean =
      if chars.isEmpty then
        opensCount == 0
      else if chars.head == '(' then
        wereAllBracketsClosed(chars.tail, opensCount + 1)
      else if chars.head == ')' then
        opensCount > 0 && wereAllBracketsClosed(chars.tail, opensCount - 1)
      else
        wereAllBracketsClosed(chars.tail, opensCount)

    wereAllBracketsClosed(chars, 0)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if coins.isEmpty || money < 0 then
      0
    else if money == 0 then
      1
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
