package com.dmytropakhmanov.algorithms.math


import org.junit.Assert._
import org.junit.Test
import LCM._

class LcmTest {
  @Test 
  def t1(): Unit = {
    assertEquals(6, gcd(18, 12))
  }
}