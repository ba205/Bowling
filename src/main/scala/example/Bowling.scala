package example

import scala.Char
import scalaz._
import Scalaz._

object Games extends ComputeScore with App {
  //printHelper("X X X X X X X X X X X X")
  //printHelper("9- 9- 9- 9- 9- 9- 9- 9- 9- 9-")
  //printHelper("5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5")
  //printHelper("52 42 32 42 52 62 5/ 5/ X X 32")
  //printHelper("-- -- -- -- -- -- -- -- -- X --")
  printHelper("X X 9/")
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
     turn's (simple) scores. If frame has invalid syntax, or has two digits
     whose sum exceeds 10, return None.
  */
  def singleFrameScore(frame : String): Option[(Int, Int)] = {
    val result : Option[(Int, Int)] = frame match {
        case "X" => Some((10, 0))
        case md if missDigit.contains(md) => Some((0, md(1).asDigit))
        case mm if mm == missMiss => Some((0, 0))
        case ms if ms == missSpare => Some((0, 10))
        case ds if digitSpare.contains(ds) 
          => Some((ds(0).asDigit, 10 - ds(0).asDigit))
        case dm if digitMiss.contains(dm) => Some((dm(0).asDigit, 0))
        case dd if digitDigit.contains(dd) => 
          Some((dd(0).asDigit, dd(1).asDigit))
        case _ => None
    }
    return result
  }

  /* Given the first 9 frame strings, output Array of tuples for pins knocked
     down in first and second turn. If any frame is invalid, return None.
  */
  def firstFramesScores(frames : Array[String]): 
    Option[Array[(Int, Int)]] = {
    val mappedFrames: Array[Option[(Int, Int)]]
      = frames.map(singleFrameScore)
    val result = mappedFrames.toList.sequence.map(x => x.toArray)
    return result
  }

  /* Given the last frame and bonus throws, output pins knocked down in first,
     second, and third turn. If any frame is invalid, return None.
  */
  def lastFrameScore(frames: Array[String]): 
    Option[(Int, Int, Int)] = {
    val result = frames match {
        case Array("X","X","X") => Some((10, 10, 10))
        case Array("X","X","-") => Some((10, 10, 0))
        case Array("X","X", n ) if digits.contains(n) 
          => Some((10, 10, n.toInt))
        case Array("X", ds) if digitSpare.contains(ds) 
          => Some((10, ds(0).asDigit, 10 - ds(0).asDigit))
        case Array("X", dm) if digitMiss.contains(dm) 
          => Some((10, dm(0).asDigit, 0))
        case Array("X", dd) if digitDigit.contains(dd) 
          => Some((10, dd(0).asDigit, dd(1).asDigit))
        case Array("X", ms) if ms == missSpare => Some((10, 0, 10))
        case Array("X", md) if missDigit.contains(md) 
          => Some((10, 0, md(1).asDigit))
        case Array("X", mm) if mm == missMiss => Some((10, 0, 0))
        case Array(dm) if digitMiss.contains(dm) 
          => Some((dm(0).asDigit, 0, 0))
        case Array(dd) if digitDigit.contains(dd) 
          => Some((dd(0).asDigit, dd(1).asDigit, 0))
        case Array(mm) if mm == missMiss => Some((0, 0, 0))
        case Array(md) if missDigit.contains(md) 
          => Some((0, md(1).asDigit, 0))
        case Array(dsm) if digitSpareMiss.contains(dsm)
          => Some((dsm(0).asDigit, 10 - dsm(0).asDigit, 0))
        case Array(dsd) if digitSpareDigit.contains(dsd)
          => Some((dsd(0).asDigit, 10 - dsd(0).asDigit, dsd(2).asDigit))
        case Array(dss) if digitSpareStrike.contains(dss)
          => Some((dss(0).asDigit, 10 - dss(0).asDigit, 10))
        case Array(msm) if msm == missSpareMiss => Some((0, 10, 0))
        case Array(msd) if missSpareDigit.contains(msd)
          => Some((0, 10, msd(2).asDigit))
        case Array(mss) if mss == missSpareStrike => Some((0, 10, 10))
        case Array() => Some((0,0,0))
        case _ => None
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
    val n = frames.length
    for ( i <- 0 to n-1) {
        val (nextTurn, nextNextTurn) = i match {
          case _ if i == n-1 => (lastFrame._1, lastFrame._2)
          case _ if i == n-2 && isStrike(frames(i+1)) => (10, lastFrame._1)
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

  def invalidPartialGame(arr : Array[String]): Boolean = {
    val n : Int = arr.length - 1
    val lastFirstFrame : String = arr(n)
    val result = lastFirstFrame match {
      case _ if lastFirstFrame == missSpare => true
      case _ if digitSpare.contains(lastFirstFrame)
        => true
      case _ => false
    }
    return result
  }

  // Given a valid input string, calculate game score
  def evalScore(input : String): Option[Int] = {
    val arr : Array[String] = inputToArray(input)
    val firstFrames : Option[Array[(Int, Int)]] 
      = firstFramesScores(arr.take(9))
    val lastFrame : Option[(Int, Int, Int)] 
      = lastFrameScore(arr.drop(9))
    val result = (firstFrames, lastFrame) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(frames), Some(last)) 
        if arr.drop(9).isEmpty && invalidPartialGame(arr.take(9))
        => None
      // Handles all valid partial and full games
      case (Some(frames), Some(last)) 
        => Some((totalScores(frames, last).sum))
    }
    return result 
  }
  
  // Helper function to print score, given input
  def printHelper(input : String) {
    println(evalScore(input))  
  }
}
