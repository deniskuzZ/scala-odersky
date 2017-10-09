package recfun

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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balance(count: Int, chars: List[Char]): Boolean = {
        if (chars.isEmpty || count < 0) {
          count == 0

        } else {
          balance(
            chars.head match {
              case '(' => count + 1
              case ')' => count - 1
              case _ => count
            },
            chars.tail)
        }
      }
      balance(0, chars)
    }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def makeChange(change: Int, coins: List[Int]): Int = {
      if (change == 0) {
        1

      } else if (coins.isEmpty || change < 0) {
        0

      } else {
        makeChange(change - coins.head, coins) +
          makeChange(change, coins.tail)
      }
    }

    if (money > 0) makeChange(money, coins) else 0
  }
}
