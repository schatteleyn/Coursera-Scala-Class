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
    if (r <= 0 || c == 1 || c >= r + 1) {
      return 1
    } else if (c == 2 || c == r - 1) {
      return r
    } else {
      val value = pascal(c - 1, r - 1) + pascal(c + 1, r - 1)
      return value
    }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    if (!chars.isEmpty) {
      val l = chars.head
      val list = chars.tail
      balance(list)
    }
    else {
      return true
    }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
