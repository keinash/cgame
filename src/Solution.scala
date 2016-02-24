import math._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util._

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {

  var data = scala.collection.mutable.ListBuffer(
    "38 79 3", "28 36", "0 2", "3 34", "29 21", "37 35", "28 32", "0 10", "37 2", "4 5", "13 14", "34 35", "27 19", "28 34", "30 31", "18 26", "0 9", "7 8", "18 24", "18 23", "0 5", "16 17", "29 30", "10 11", "0 12", "15 16", "0 11", "0 17", "18 22", "23 24", "0 7", "35 23", "22 23", "1 2", "0 13", "18 27", "25 26", "32 33", "28 31", "24 25", "28 35", "21 22", "4 33", "28 29", "36 22", "18 25", "37 23", "18 21", "5 6", "19 20", "0 14", "35 36", "9 10", "0 6", "20 21", "0 3", "33 34", "14 15", "28 33", "11 12", "12 13", "17 1", "18 19", "36 29", "0 4", "0 15", "0 1", "18 20", "2 3", "0 16", "8 9", "0 8", "26 27", "28 30", "3 4", "31 32", "6 7", "37 1", "37 24", "35 2", "0", "18", "28")

  // n: the total number of nodes in the level, including the gateways
  // l: the number of links
  // e: the number of exit gateways
  val Array(n, l, e) = for (i <- readLine split " ") yield i.toInt
  val graphSet = collection.mutable.ListBuffer[Graph]()
  for (i <- 0 until l) {
    // n1: N1 and N2 defines a link between these nodes
    val Array(n1, n2) = for (i <- readLine split " ") yield i.toInt
    val g1 = graphSet.find(_.number == n1).fold {
      val g = Graph(n1)
      graphSet += g
      g
    } { x => x }
    val g2 = graphSet.find(_.number == n2).fold {
      val g = Graph(n2)
      graphSet += g
      g
    } { x => x }
    g1.addLink(g2)
  }

  for (i <- 0 until e) {
    val ei = readLine.toInt // the index of a gateway node
    graphSet.find(_.number == ei).foreach(_.setAsGateway())
  }

  // game loop
  while (true) {
    val si = readLine.toInt // The index of the node on which the Skynet agent is positioned this turn
    println(graphSet.find(_.number == si).map(_.infectAndDestroyNearest).get)
  }

  def readLine = {
    val head = data.head
    data = data.tail
    head
  }
}

case class Graph(number: Int, var gateway: Boolean = false) {
  val links: mutable.HashSet[Link] = mutable.HashSet()

  def addLink(newGraph: Graph) = {
    val link: Link = Link(this, newGraph)
    links += link
    newGraph.links += link
    newGraph
  }

  def setAsGateway() = this.gateway = true

  def remove(graph: Graph) = links.find(x => x.graph1.number == graph.number || x.graph2.number == graph.number)
    .foreach(_.break)

  def infectAndDestroyNearest = {
    findShortestLinkToGateway(0).link.map {
      l => l.break; l
    }.get
  }

  def findShortestLinkToGateway(currentLen: Int): Result = {
    links.foldLeft(Result(None, currentLen)) {
      case (Result(Some(link), l), next) => Result(Some(link), l)
      case (Result(None, l), next) if next.graph2.gateway || next.graph1.gateway => Result(Some(next), l)
      case (Result(None, l), next) if !next.graph2.gateway && !next.graph1.gateway =>
        if (next.graph1 == this) {
          next.graph2.findShortestLinkToGateway(currentLen + 1)
        } else {
          next.graph1.findShortestLinkToGateway(currentLen + 1)
        }
    }
  }

  override def equals(obj: scala.Any): Boolean = {
    val g = obj.asInstanceOf[Graph]
    g.number == number
  }
}

case class Result(var link: Option[Link], val len: Int)

case class Link(graph1: Graph, graph2: Graph) {
  override def toString: String = s"${graph1.number} ${graph2.number}"

  def break = {
    graph1.links -= this
    graph2.links -= this
  }

  override def equals(obj: scala.Any): Boolean = {
    val link = obj.asInstanceOf[Link]
    link.graph1.number == this.graph1.number ||
    link.graph2.number == this.graph1.number ||
    link.graph1.number == this.graph2.number ||
    link.graph2.number == this.graph2.number
  }

  override def hashCode(): Int = {
    graph1.number * 17 + graph2.number * 27
  }

}