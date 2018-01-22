package example

object Games extends ComputeScore with App {
  printHello()
}

trait ComputeScore {
 
  lazy val greeting: String = "hello"

  def printHello() {
      println(greeting)
  }
}
