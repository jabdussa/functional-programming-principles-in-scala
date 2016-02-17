package recfun
import common._

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
  def pascal(c: Int, r: Int): Int =
    if (c==0 || c==r) 1
    else (pascal(c-1, r-1) + pascal(c, r-1))

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def helper(numOpen: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) numOpen==0
      else {
        if (chars.head == '(') helper(numOpen + 1, chars.tail)
        else if (chars.head == ')') {
          if (numOpen == 0) false
          else helper(numOpen - 1, chars.tail)
        }
        else helper(numOpen, chars.tail)
      }
    }
    helper(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def helper(amount: Int, availCoins: List[Int]): Int = {
      if (amount==0) 1
      else  {
        if (availCoins.isEmpty) 0
        else  {
          if (availCoins.head <= amount) {
            (helper(amount-availCoins.head, availCoins) + helper(amount, availCoins.tail))
          }
          else helper(amount, availCoins.tail)
        }
      }
    } //helper
    if (money==0 || coins.isEmpty) 0
    else helper(money, coins)
  }
}
