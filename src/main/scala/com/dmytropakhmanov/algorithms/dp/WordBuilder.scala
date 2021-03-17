package com.dmytropakhmanov.algorithms.dp

// Having combination of letters need to build a word.
//Assumtions: 
//  Input array is always valid, it is always possible to build a word
// There are no letter duplication in the word

import scala.collection.mutable

object WordBuilder extends App {
  
  println(findWord(Array("G-E","E-R","R-M","M-A","A-N","N-Y"))) // GERMANY
  println(findWord(Array("N-Y","E-R","R-M","G-E","A-N","M-A"))) // GERMANY
  

  def findWord(arr: Array[String]): String = {
    val wordBuilder = new StringBuilder()
    val queue = mutable.Queue.from(arr)
    
    wordBuilder.append(queue.dequeue().replace("-", ""))
    while (queue.nonEmpty) {
      val element = queue.dequeue()
      if (wordBuilder.startsWith(element.last.toString)) wordBuilder.insert(0, element.head)
      else if (wordBuilder.endsWith(element.head.toString)) wordBuilder.append(element.last)
      else queue.enqueue(element)
    }
    
    wordBuilder.result()
  }

}
