package example

import org.scalatest._

class BowlingTest extends FlatSpec with Matchers {
  "The Hello object" should "say hello" in {
    Games.greeting shouldEqual "hello"
  }
}
