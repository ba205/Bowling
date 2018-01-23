package example

object Games extends ComputeScore with App {
  printHello()
}

trait ComputeScore {
 
  lazy val greeting: String = "hello"

  lazy val strike : String = "X"
  lazy val spare : String = "/"
  lazy val miss : String = "-"
  lazy val digits : Array[String] = Array("1", "2", "3", "4", "5", "6", "7", "8","9")
  lazy val digitMiss : Array[String] = digits.map(x => x + miss)
  lazy val digitSpare : Array[String] = digits.map(x => x + spare)
  lazy val digitDigit : Array[String] = Array("")
  lazy val missMiss : String = miss + miss
  lazy val missSpare : String = miss + spare
  lazy val missDigit : Array[String] = digits.map(x => miss + x) 
  lazy val digitSpareMiss : Array[String] = digits.map(x => x + spare + miss)
  lazy val digitSpareDigit : Array[String] = Array("")
  lazy val digitSpareStrike : Array[String] = digits.map(x => x + spare + strike)
  lazy val missSpareMiss : String = miss + spare + miss
  lazy val missSpareDigit : Array[String] = digits.map(x => miss + spare + x)
  lazy val missSpareStrike : String = miss + spare + strike

  def printHello() {
      println(missSpareDigit.mkString(" "))
  }
}
