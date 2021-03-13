package com.dmytropakhmanov.algorithms.dp


object CoinChange extends App {
  private val INF = 987654321//Int.MaxValue

  def coinChange(coins: Array[Int], amount: Int): Int = {
    if (coins.length == 0) throw new IllegalArgumentException("No coin values :/")
    val N = coins.length
    // Initialize table and set first row to be infinity
    val DP = Array.ofDim[Int](N + 1, amount + 1)
    DP(0) = Array.fill(amount + 1)(INF)
    DP(1)(0) = 0
    // Iterate through all the coins
    for (i <- 1 until N) {
      val coinValue = coins(i - 1)
      for (j <- 1 until amount) { // Consider not selecting this coin
        DP(i)(j) = DP(i - 1)(j)
        // Try selecting this coin if it's better
        if (j - coinValue >= 0 && DP(i)(j - coinValue) + 1 < DP(i)(j)) DP(i)(j) = DP(i)(j - coinValue) + 1
      }
    }
    // The amount we wanted to make cannot be made :/
    if (DP(N)(amount) == INF) return -1
    // Return the minimum number of coins needed
    DP(N)(amount)
  }

  def coinChangeSpaceEfficient(coins: Array[Int], amount: Int): Int = {
    // Initialize table and set everything to infinity except first cell
    val DP = Array.fill(amount + 1)(INF)
    DP(0) = 0
    (1 until amount).foreach { i =>
      coins
        .view
        .filter(coinValue => i - coinValue >= 0 && DP(i - coinValue) + 1 < DP(i))
        .foreach(coinValue => DP(i) = DP(i - coinValue) + 1)
    }
 
    if (DP.last == INF) -1 else DP.last
  }

  // The recursive approach has the advantage that it does not have to visit
  // all possible states like the tabular approach does. This can speedup
  // things especially if the coin denominations are large.
  def coinChangeRecursive(coins: Array[Int], amount: Int): Int = {
    if (amount < 0) return -1
    val DP = new Array[Int](amount + 1)
    coinChangeRecursive(amount, coins, DP)
  }

  // Private helper method to actually go the recursion// Private helper method to actually go the recursion
  private def coinChangeRecursive(amount: Int, coins: Array[Int], DP: Array[Int]): Int = { 
    
    // Base cases.
    if (amount < 0) return -1
    if (amount == 0) return 0
    if (DP(amount) != 0) return DP(amount)
    var minCoins = INF
    for (coinValue <- coins) {
      val newAmount = amount - coinValue
      val value = coinChangeRecursive(newAmount, coins, DP)
      if (value != -1 && value < minCoins) minCoins = value + 1
    }
    // If we weren't able to find some coins to make our
    // amount then cache -1 as the answer.
    DP(amount) = if (minCoins == INF) -1 else minCoins
    DP(amount)
  }
  

  val coins = Array(2, 6, 1)
  println(coinChange(coins, 17))
  println(coinChangeSpaceEfficient(coins, 17))
  println(coinChangeRecursive(coins, 17))
  
}
