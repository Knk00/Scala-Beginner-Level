package recfun

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
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
    if  (c == 0 || c == r || r == 0)  1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars : List[Char]) : Boolean = {
    val equal : Int = 0
    @tailrec
    def equality (chars : List[Char], equal : Int) : Boolean = {
      if (chars.isEmpty) equal == 0 else {
        chars.head match{
          case '(' => equality(chars.tail, equal + 1)
          case ')' => equality(chars.tail, equal - 1)
          case _ => equality(chars.tail, equal)
        }
      }
    }
    equality(chars, 0)
  }


  /**
   * Exercise 3
   */
def countChange(money: Int, coins: List[Int]): Int = { //countChange(4, [1, 2]

  def loop(money: Int, coins: List[Int]): Int = { //loop[4]
  if (money < 0 || coins.isEmpty ) 0
  else if (money == 0 ) 1
  else loop(money, coins.tail) + loop(money - coins.head, coins)
}

  loop(money, coins)
}
}

