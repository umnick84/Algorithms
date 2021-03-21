package com.dmytropakhmanov.algorithms.strings

object LongestCommonSubstring extends App {

  val X = "abcdxyz"
  val Y = "xyzabcd"

  val n = X.length
  val m = Y.length

  System.out.println(lcs(n, m, 0))

  def lcs(i: Int, j: Int, maxCcount: Int): Int = {
    var currentCount = 0
    if (i == 0 || j == 0) return maxCcount
    if (X.charAt(i - 1) equals Y.charAt(j - 1)) 
      currentCount = lcs(i - 1, j - 1, maxCcount + 1)
    currentCount = Math.max(currentCount, Math.max(lcs(i, j - 1, 0), lcs(i - 1, j, 0)))
    currentCount
  }

}
