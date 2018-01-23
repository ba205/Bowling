package example

import org.scalatest._

class BowlingTest extends FlatSpec with Matchers {
  "inputToArray" should "turn a string of space delimited strikes to an array of strikes" in {
    Games.inputToArray("X X X X X X X X X X X X") shouldEqual 
    Array("X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X")
  }
}
