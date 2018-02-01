package example

import org.scalatest._

class BowlingTest extends FlatSpec with Matchers {
  
  "inputToArray" should "turn a string of space delimited strikes to an array of strikes" in {
    Games.inputToArray("X X X X X X X X X X X X") shouldEqual 
    Array("X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X")
  }

  "lastFrameScore" should "interpret strike, strike, digit" in {
    Games.lastFrameScore(Array("X","X","2")) shouldEqual Some((10, 10, 2))
  }
  
  "lastFrameScore" should "interpret strike, digit, spare" in {
    Games.lastFrameScore(Array("X","2/")) shouldEqual Some((10, 2, 8))
  }

  "lastFrameScore" should "interpret strike, digit, miss" in {
    Games.lastFrameScore(Array("X", "2-")) shouldEqual Some((10, 2, 0))
  }
  
  "lastFrameScore" should "interpret strike, digit, digit" in {
    Games.lastFrameScore(Array("X", "27")) shouldEqual Some((10, 2, 7))
  }

  "lastFrameScore" should "interpret strike, miss, digit" in {
    Games.lastFrameScore(Array("X", "-2")) shouldEqual Some((10, 0, 2))
  }

  "lastFrameScore" should "interpret digit, miss" in {
    Games.lastFrameScore(Array("2-")) shouldEqual Some((2, 0, 0))
  }

  "lastFrameScore" should "interpret digit, digit" in {
    Games.lastFrameScore(Array("27")) shouldEqual Some((2,7,0))
  }

  "lastFrameScore" should "interpret miss, digit" in {
    Games.lastFrameScore(Array("-2")) shouldEqual Some((0, 2, 0))
  }

  "lastFrameScore" should "interpret digit, spare, miss" in {
    Games.lastFrameScore(Array("2/-")) shouldEqual Some((2,8,0))
  }

  "lastFrameScore" should "interpret digit, spare, digit" in {
    Games.lastFrameScore(Array("2/2")) shouldEqual Some((2,8,2))
  }

  "lastFrameScore" should "interpret digit, spare, strike" in {
    Games.lastFrameScore(Array("2/X")) shouldEqual Some((2, 8, 10))
  }

  "lastFrameScore" should "interpret miss, spare, digit" in {
    Games.lastFrameScore(Array("-/2")) shouldEqual Some((0,10,2))
  }

  "lastFrameScore" should "not interpret miss, spare, digit, strike" in {
    Games.lastFrameScore(Array("-/2", "X")) shouldEqual None
  }

  "lastFrameScore" should "not interpret four strikes" in {
    Games.lastFrameScore(Array("X", "X", "X", "X")) shouldEqual None
  }

  "lastFrameScore" should "interpret empty array as misses" in {
    Games.lastFrameScore(Array()) shouldEqual Some((0,0,0))
  }

  "singleFrameScore" should "interpret miss, digit" in {
    Games.singleFrameScore("-2") shouldEqual Some((0,2))
  }

  "singleFrameScore" should "interpret miss, miss" in {
    Games.singleFrameScore("--") shouldEqual Some((0,0))
  }

  "singleFrameScore" should "interpret miss, spare" in {
    Games.singleFrameScore("-/") shouldEqual Some((0,10))
  }

  "singleFrameScore" should "interpret digit, spare" in {
    Games.singleFrameScore("2/") shouldEqual Some((2, 8))
  }

  "singleFrameScore" should "interpret digit, miss" in {
    Games.singleFrameScore("2-") shouldEqual Some((2, 0))
  }

  "singleFrameScore" should "interpret digit, digit" in {
    Games.singleFrameScore("27") shouldEqual Some((2, 7))
  }
  
  "evalScore" should "score 'X X X X X X X X X X X X' as 300" in {
    Games.evalScore("X X X X X X X X X X X X") shouldEqual Some(300)
  }

  "evalScore" should "score '9- 9- 9- 9- 9- 9- 9- 9- 9- 9-' as 90" in {
    Games.evalScore("9- 9- 9- 9- 9- 9- 9- 9- 9- 9-") shouldEqual Some(90)
  }

  "evalScore" should "score '5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5' as 150" in {
    Games.evalScore("5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5") shouldEqual Some(150)
  }

  "evalScore" should "score '5/ X 5/ X 5/ X 5/ X 5/ 5/X' as 195" in {
    Games.evalScore("5/ X 5/ X 5/ X 5/ X 5/ 5/X") shouldEqual Some(195)
  }

  "evalScore" should "score '-- -- -- -- -- -- -- -- -- X --' as 10" in {
    Games.evalScore("-- -- -- -- -- -- -- -- -- X --") shouldEqual Some(10)
  }
 
  "evalScore" should "score '52 42 32 42 52 62 5/ 5/ X X 32' as 112" in {
    Games.evalScore("52 42 32 42 52 62 5/ 5/ X X 32") shouldEqual Some(112)
  }

  "evalScore" should "score '58 42 32 42 52 62 5/ 5/ X X 32' as None" in {
    Games.evalScore("58 42 32 42 52 62 5/ 5/ X X 32") shouldEqual None
  }
  
  "evalScore" should "score '/2 42 32 42 52 62 5/ 5/ X X 32' as None" in {
    Games.evalScore("/2 42 32 42 52 62 5/ 5/ X X 32") shouldEqual None
  }

  "evalScore" should "score '5X 42 32 42 52 62 5/ 5/ X X 32' as None" in {
    Games.evalScore("5X 42 32 42 52 62 5/ 5/ X X 32") shouldEqual None
  }

  "evalScore" should "score 'X X 9-' as 57" in {
    Games.evalScore("X X 9-") shouldEqual Some(57)
  }

  "evalScore" should "not score a partial game ending in a spare" in {
    Games.evalScore("X X 9/") shouldEqual None
  }
  
  "evalScore" should "not score a partial game ending in a strike" in {
    Games.evalScore("X X X") shouldEqual None
  }

}
