import scala.util._
object SolutionTempreture extends App{readInt
Try(print(readLine.split(" ")map(_.toInt)sortWith(_>_)minBy math.abs))recover{case _=>print(0)}

  def readLine = "-3 -8 4 5 3"
  def readInt = 5
}