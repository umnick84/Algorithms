package com.dmytropakhmanov.algorithms.math

import scala.annotation.Annotation

object LCM extends App {

  // Finds the greatest common divisor of a and b
  // Basic Euclidean Algorithm for GCD 
  def gcd(a: Long, b: Long): Long = {
    if (b == 0)
      if (a < 0) -a else a
    else
      gcd(b, a % b)
  }

  // Finds the least common multiple of a and b
  def lcm(a: Long, b: Long): Long = {
    val lcm = (a / gcd(a, b)) * b
    if (lcm > 0) lcm else -lcm 
  }
  
  println(s"the greatest common divisor of 18 and 12 is ${gcd(18, 12)}") //6
  println(s"the least common multiple of 18 and 12 is ${lcm(18, 12)}") //36
  
}
