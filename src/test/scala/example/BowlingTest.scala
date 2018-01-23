package example

import org.scalatest._

class BowlingTest extends FlatSpec with Matchers {
  
  "inputToArray" should "turn a string of space delimited strikes to an array of strikes" in {
    Games.inputToArray("X X X X X X X X X X X X") shouldEqual 
    Array("X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X")
  }

  "lastFrameScore" should "interpret strike, strike, digit" in {
    Games.lastFrameScore(Array("X","X","2")) shouldEqual (10, 10, 2)
  }
  
  "lastFrameScore" should "interpret strike, digit, spare" in {
    Games.lastFrameScore(Array("X","2/")) shouldEqual (10, 2, 8)
  }

  "lastFrameScore" should "interpret strike, digit, miss" in {
    Games.lastFrameScore(Array("X", "2-")) shouldEqual (10, 2, 0)
  }
  
  "lastFrameScore" should "interpret strike, digit, digit" in {
    Games.lastFrameScore(Array("X", "27")) shouldEqual (10, 2, 7)
  }

  "lastFrameScore" should "interpret strike, miss, digit" in {
    Games.lastFrameScore(Array("X", "-2")) shouldEqual (10, 0, 2)
  }

  "lastFrameScore" should "interpret digit, miss" in {
    Games.lastFrameScore(Array("2-")) shouldEqual (2, 0, 0)
  }

  "lastFrameScore" should "interpret digit, digit" in {
    Games.lastFrameScore(Array("27")) shouldEqual (2,7,0)
  }

  "lastFrameScore" should "interpret miss, digit" in {
    Games.lastFrameScore(Array("-2")) shouldEqual (0, 2, 0)
  }

  "lastFrameScore" should "interpret digit, spare, miss" in {
    Games.lastFrameScore(Array("2/-")) shouldEqual (2,8,0)
  }

  "lastFrameScore" should "interpret digit, spare, digit" in {
    Games.lastFrameScore(Array("2/2")) shouldEqual (2,8,2)
  }

  "lastFrameScore" should "interpret digit, spare, strike" in {
    Games.lastFrameScore(Array("2/X")) shouldEqual (2, 8, 10)
  }

  "lastFrameScore" should "interpret miss, spare, digit" in {
    Games.lastFrameScore(Array("-/2")) shouldEqual (0,10,2)
  }

  "singleFrameScore" should "interpret miss, digit" in {
    Games.singleFrameScore("-2") shouldEqual (0,2)
  }

  "singleFrameScore" should "interpret miss, miss" in {
    Games.singleFrameScore("--") shouldEqual (0,0)
  }

  "singleFrameScore" should "interpret miss, spare" in {
    Games.singleFrameScore("-/") shouldEqual (0,10)
  }

  "singleFrameScore" should "interpret digit, spare" in {
    Games.singleFrameScore("2/") shouldEqual (2, 8)
  }

  "singleFrameScore" should "interpret digit, miss" in {
    Games.singleFrameScore("2-") shouldEqual (2, 0)
  }

  "singleFrameScore" should "interpret digit, digit" in {
    Games.singleFrameScore("27") shouldEqual (2, 7)
  }
}
