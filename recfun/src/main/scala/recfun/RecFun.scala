package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do print(s"${pascal(col, row)} ")
      println()

  /** Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if c < 0 || r < 0 then 0
    else {
      if c == 0 || r == 0 || c == r then 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  /** Exercise 2
    */

  def isBalanced(
      chars: List[Char],
      startIndex: Int = 0,
      currentIndex: Int = 0
  ): Boolean =
    if startIndex == chars.length then currentIndex == 0
    else if currentIndex < 0 then false
    else if chars(startIndex) == '(' then
      isBalanced(chars, startIndex + 1, currentIndex + 1)
    else if chars(startIndex) == ')' then
      isBalanced(chars, startIndex + 1, currentIndex - 1)
    else isBalanced(chars, startIndex + 1, currentIndex)

  def balance(chars: List[Char]): Boolean =
    isBalanced(chars)

  /** Exercise 3
    */

  def sort(coins: List[Int]): Array[Int] =
    var updatedCoins: Array[Int] = coins.toArray
    for i <- 1 until updatedCoins.length do
      var key: Int = updatedCoins(i);
      var j: Int = i - 1;
      while (j >= 0 && key < updatedCoins(j)) {
        updatedCoins(j + 1) = updatedCoins(j)
        j -= 1
      }
      updatedCoins(j + 1) = key
    updatedCoins

  def countChangeWithSortedCoins(money: Int, sortedCoins: Array[Int]): Int =
    if money <= 0 || sortedCoins.isEmpty then 0
    else
      val smallestCoin = sortedCoins.head
      if (smallestCoin == money) then 1
      else
        countChangeWithSortedCoins(money - smallestCoin, sortedCoins) +
          countChangeWithSortedCoins(money, sortedCoins.tail)

  def countChange(money: Int, coins: List[Int]): Int =
    val sortedCoins: Array[Int] = sort(coins)
    countChangeWithSortedCoins(money, sortedCoins)

@main def run()=
  println("hello")