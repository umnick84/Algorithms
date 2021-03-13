package com.dmytropakhmanov.algorithms.dp

import scala.collection.mutable.ArrayBuffer

//todo find mistake in findRobbedHouses
object HouseRobber extends App {

  //Problem: https://leetcode.com/problems/house-robber
  // Time Complexity: O(n), space Complexity: O(n)

  def rob(houses: Array[Int]) = {
    val n = houses.length
    var i = 0
    var j = 2
    while (i < n) {
      dp(j) = Math.max(dp(j - 1), dp(j - 2) + houses(i))
      System.out.println(dp(j))
      i += 1
      j += 1
    }
    
    dp(n + 1)
  }


  // Finds a set of optimal houses to rob. This method assumes
  // the rob method was already called and 'dp' is populated.
  def findRobbedHouses(houses: Array[Int]) = {
    val n = houses.length
    val robbedHouses = ArrayBuffer.empty[Integer]
    var i = n - 1
    var j = n + 1
    while (i >= 0) {
      if (dp(j - 2) + houses(i) > dp(j - 1)) robbedHouses.addOne(i)

      i -= 1
      j -= 1
    }
    robbedHouses
  }
  
  //amount of money of each house
//  val houses: Array[Int] = Array(5, 2, 4, 7, 2, 13, 9, 1, 8, 4)
  val houses = Array(2,7,9,3,1)
  val dp = new Array[Int](houses.length + 2)
  
  val amount = rob(houses)
  System.out.println(s"Robbed: $amount$$")

  findRobbedHouses(houses).foreach(houseIndex => 
    printf("Robbed house at index %d, for %d$\n", houseIndex, houses(houseIndex))
  )
}
