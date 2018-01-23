package example

import scala.Char

object Games extends ComputeScore with App {
  printHelper("X X X X X X X X X X X X")
  printHelper("9- 9- 9- 9- 9- 9- 9- 9- 9- 9-")
  printHelper("5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5")
  printHelper("52 42 32 42 52 62 5/ 5/ X X 32")
  printHelper("-- -- -- -- -- -- -- -- -- X --")
}

trait ComputeScore {

  // Enumerate the possible cases for a single frame. 
  lazy val strike : String = "X"
  lazy val spare : String = "/"
  lazy val miss : String = "-"
  lazy val digits : Array[String] 
    = Array("1", "2", "3", "4", "5", "6", "7", "8","9")
  lazy val digitMiss : Array[String] = digits.map(x => x + miss)
  lazy val digitSpare : Array[String] = digits.map(x => x + spare)
  // Can't knock down more than 10 pins, exactly ten pins with a digit first is
  // a spare
  lazy val digitDigit : Array[String]
    = for(x <- digits; y <- digits; if x.toInt + y.toInt < 10) yield {x+y}
  
  lazy val missMiss : String = miss + miss
  lazy val missSpare : String = miss + spare
  lazy val missDigit : Array[String] = digits.map(x => miss + x) 
  
  lazy val digitSpareMiss : Array[String] = digits.map(x => x + spare + miss)
  lazy val digitSpareDigit : Array[String] 
    = for(x <- digits; y <- digits) yield {x + "/" + y}
  lazy val digitSpareStrike : Array[String] = digits.map(x => x + spare + strike)
  
  lazy val missSpareMiss : String = miss + spare + miss
  lazy val missSpareDigit : Array[String] = digits.map(x => miss + spare + x)
  lazy val missSpareStrike : String = miss + spare + strike

  // Transform the input (assumed valid) into an array of strings.
  def inputToArray(str : String): Array[String] = {
      return str.split(" ")
  }
  
  /* Given a single frame from the first 9 frames, output first and second
     turn's (simple) scores. Still assuming correct input, if not given time
     constraints, would use Optional or Try to represent invalid states.
  */
  def singleFrameScore(frame : String): (Int, Int) = {
    val result = frame match {
        case "X" => (10, 0)
        case md if missDigit.contains(md) => (0, md(1).asDigit)
        case mm if mm == missMiss => (0, 0)
        case ms if ms == missSpare => (0, 10)
        case ds if digitSpare.contains(ds) => (ds(0).asDigit, 10 - ds(0).asDigit)
        case dm if digitMiss.contains(dm) => (dm(0).asDigit, 0)
        case dd if digitDigit.contains(dd) => (dd(0).asDigit, dd(1).asDigit)
        // Catch all since not using Try/Optional due to time contraints,
        // also this should never happen with valid input
        case _ => (-1, -1)
    }
    return result
  }

  /* Given the first 9 frame strings, output Array of tuples for pins knocked
     down in first and second turn. 
  */
  def firstFramesScores(frames : Array[String]): Array[(Int, Int)] = {
    return frames.map(singleFrameScore)
  }

  /* Given the last frame and bonus throws, output pins knocked down in first,
     second, and third turn. Still assuming correct input, if not given time
     constraints, would use Optional or Try to represent invalid states.
  */
  def lastFrameScore(frames: Array[String]): (Int, Int, Int) = {
    val result = frames match {
        case Array("X","X","X") => (10, 10, 10)
        case Array("X","X","-") => (10, 10, 0)
        case Array("X","X", n ) if digits.contains(n) => (10, 10, n.toInt)
        case Array("X", ds) if digitSpare.contains(ds) 
          => (10, ds(0).asDigit, 10 - ds(0).asDigit)
        case Array("X", dm) if digitMiss.contains(dm) => (10, dm(0).asDigit, 0)
        case Array("X", dd) if digitDigit.contains(dd) 
          => (10, dd(0).asDigit, dd(1).asDigit)
        case Array("X", ms) if ms == missSpare => (10, 0, 10)
        case Array("X", md) if missDigit.contains(md) => (10, 0, md(1).asDigit)
        case Array("X", mm) if mm == missMiss => (10, 0, 0)
        case Array(dm) if digitMiss.contains(dm) => (dm(0).asDigit, 0, 0)
        case Array(dd) if digitDigit.contains(dd) 
          => (dd(0).asDigit, dd(1).asDigit, 0)
        case Array(mm) if mm == missMiss => (0, 0, 0)
        case Array(md) if missDigit.contains(md) => (0, md(1).asDigit, 0)
        case Array(dsm) if digitSpareMiss.contains(dsm)
          => (dsm(0).asDigit, 10 - dsm(0).asDigit, 0)
        case Array(dsd) if digitSpareDigit.contains(dsd)
          => (dsd(0).asDigit, 10 - dsd(0).asDigit, dsd(2).asDigit)
        case Array(dss) if digitSpareStrike.contains(dss)
          => (dss(0).asDigit, 10 - dss(0).asDigit, 10)
        case Array(msm) if msm == missSpareMiss => (0, 10, 0)
        case Array(msd) if missSpareDigit.contains(msd)
          => (0, 10, msd(2).asDigit)
        case Array(mss) if mss == missSpareStrike => (0, 10, 10)
        // Catch all since not using Try/Optional due to time contraints,
        // also this should never happen with valid input
        case _                  => (-1, -1, -1)
    }
    return result
  }

  // Checks if a given frame is a spare or not.
  def isSpare(frame: (Int, Int)): Boolean = {
    return frame != (10, 0) && frame._1 + frame._2 == 10
  }

  // Checks if a given frame is a strike or not.
  def isStrike(frame: (Int, Int)): Boolean = {
    return frame == (10, 0)
  }

  // Given the turns for all of the frames, calculate the scores for each frame.
  def totalScores(frames : Array[(Int, Int)], 
                  lastFrame: (Int, Int, Int)): Array[Int] = {
    val result = Array(0,0,0,0,0,0,0,0,0,0)
    result(9) = lastFrame._1 + lastFrame._2 + lastFrame._3
    for ( i <- 0 to 8) {
        val (nextTurn, nextNextTurn) = i match {
          case 8 => (lastFrame._1, lastFrame._2)
          case 7 if isStrike(frames(i+1)) => (10, lastFrame._1)
          case _ if isStrike(frames(i+1)) => (10, frames(i+2)._1)
          case _ => frames(i+1)
        }
        result(i) = frames(i) match {
          case (fst, snd) if isStrike((fst, snd))
            => fst + snd + nextTurn + nextNextTurn
          case (fst, snd) if isSpare((fst, snd)) => fst + snd + nextTurn
          case (fst, snd) => fst + snd
        } 
          
    }
    return result 
  }

  // Given a valid input string, calculate game score
  def evalScore(input : String): Int = {
    val arr : Array[String] = inputToArray(input)
    val firstFrames : Array[(Int, Int)] = firstFramesScores(arr.take(9))
    val lastFrame : (Int, Int, Int) = lastFrameScore(arr.drop(9))
    val scores = totalScores(firstFrames, lastFrame)
    return scores.sum
  }
  
  // Helper function to print score, given input
  def printHelper(input : String) {
    println(evalScore(input))  
  }
}
