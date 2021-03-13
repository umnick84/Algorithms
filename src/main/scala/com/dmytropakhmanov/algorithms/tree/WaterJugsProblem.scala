package com.dmytropakhmanov.algorithms.tree

import scala.collection.mutable._
import util.control.Breaks._

//You are given a m liter jug and a n liter jug. Both the jugs are initially empty. 
// The jugs don’t have markings to allow measuring smaller quantities. 
// You have to use the jugs to measure d liters of water where d is less than n. 

object WaterJugsProblem extends App {
  
  opaque type JugStates = (Int, Int)
  type Visited = Boolean

  //we will use breadth first search
  def solution(a: Int, b: Int, target: Int): Unit = {
    if (target > a || target > b) {
      println("No solution")
      return
    }
//    val maxAB = Math.max(a, b)
    
    // Map is used to store the states, every
    // state is hashed to binary value to 
    // indicate either that state is visited 
    // before or not
    val m: Map[JugStates, Visited] = Map.empty;
    var isSolvable = false
    val path: ListBuffer[JugStates] = ListBuffer.empty

    // Queue to maintain states
    val possibleSteps: Queue[JugStates] = Queue.empty

    // Initializing with initial state
    possibleSteps.enqueue((0, 0));
    
    while (possibleSteps.nonEmpty) {
      breakable {
        // Current state
        val currentState: JugStates = possibleSteps.dequeue();

        // If this state is already visited
        if ((m.getOrElse(currentState, false))) break()

        // Doesn't met jug constraints
        if (currentState._1 > a || currentState._2 > b || currentState._1 < 0 || currentState._2 < 0) break()

        // Filling the vector for constructing the solution path
        path.append(currentState);

        // Marking current state as visited
        m(currentState) = true;

        // If we reach solution state
        if (currentState._1 == target || currentState._2 == target) {
          isSolvable = true
          if (currentState._1 == target) { 
            if (currentState._2 != 0)  // Fill final state
              path.addOne((currentState._1, 0))
          } else {
            if (currentState._1 != 0)
              // Fill final state
              path.addOne((0, currentState._2))
          }

          // Print the solution path
          path.foreach(p => println(s"(${p._1}, ${p._2})"))
          
          break()
        }

        // If we have not reached final state 
        // then, start developing intermediate 
        // states to reach solution state
        
        // Fill Jug2
        possibleSteps.enqueue((currentState._1, b))

        // Fill Jug1
        possibleSteps.enqueue((a, currentState._2))

//        (0 until maxAB).foreach { ap =>
//          // Pour amount ap from Jug2 to Jug1
//          var c = currentState._1 + ap
//          var d = currentState._2 - ap
//
//          // Check if this state is possible or not
//          if (c == a || d == 0)
//            possibleSteps.enqueue((c, d));
//          
//          // Pour amount ap from Jug 1 to Jug2
//          c = currentState._1 - ap;
//          d = currentState._2 + ap;
//          // Check if this state is possible or not
//          if (c == 0 || d == b)
//            possibleSteps.enqueue((c, d))
//        }

        // Pour amount ap from Jug2 to Jug1
        var min = Math.min(a - currentState._1, currentState._2)
        if (min > 0)
          possibleSteps.enqueue((currentState._1 + min, currentState._2 - min))
        // Pour amount ap from Jug 1 to Jug2
        min = Math.min(currentState._1, b - currentState._2)
        if (min > 0)
          possibleSteps.enqueue((currentState._1 - min, currentState._2 + min))

        // Empty Jug2
        possibleSteps.enqueue((a, 0));
        // Empty Jug1
        possibleSteps.enqueue((0, b));
      }
    }

    // No, solution exists if ans=0
    if (!isSolvable)
      println("No solution");
  } 
  
  solution(4, 3, 2)
  println("-------")
  solution(9, 3, 1)
}
