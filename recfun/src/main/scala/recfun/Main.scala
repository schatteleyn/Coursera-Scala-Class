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
      1
    } else { 
	      if (c == 2 || c == r - 1) {
	        r
	    } else {
	      val value = pascal(c - 1, r - 1) + pascal(c + 1, r - 1)
	        value
	    }
    }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def parenthesis(chars: List[Char], o: Int, f: Int): Boolean = {
	    if (!chars.isEmpty) {
	      val l = chars.head
	      val list = chars.tail
	        if (l == '(') {
	          parenthesis(list, o+1, f)
	        } else {
	          if (l == ')') {
	            if (f == o) false else parenthesis(list, o, f+1)
	          } else {
		        parenthesis(list, o, f)
		      }
	        }
	    } else {
	        (o == f) 
	    }
    }
  parenthesis(chars, 0, 0)
  }
  

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
