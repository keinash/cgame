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
    "4 4 1",
    "0 1",
    "0 2",
    "1 3",
    "2 3")
  val intData = 3

  // n: the total number of nodes in the level, including the gateways
  // l: the number of links
  // e: the number of exit gateways
  val Array(n, l, e) = for (i <- readLine split " ") yield i.toInt
  val graphSet = collection.mutable.HashSet[Graph]()
  for (i <- 0 until l) {
    // n1: N1 and N2 defines a link between these nodes
    val Array(n1, n2) = for (i <- readLine split " ") yield i.toInt
    val g1 = Graph(n1)
    val g2 = Graph(n2)
    g1.addLink(g2)
    graphSet += g1
    graphSet += g2
  }

  for (i <- 0 until e) {
    val ei = readInt // the index of a gateway node
    graphSet.find(_.number == ei).foreach(_.setAsGateway)
  }

  // game loop
  while (true) {
    val si = readInt // The index of the node on which the Skynet agent is positioned this turn
    println(graphSet.find(_.number == si).map(_.infectAndDestroyNearest))
  }

  def readLine = {
    val head = data.head
    data = data.tail
    head
  }

  def readInt = intData

}

case class Graph(number: Int, var gateway: Boolean = false) {
  val links: mutable.HashSet[Link] = mutable.HashSet()

  def addLink(newGraph: Graph) = {
    val link: Link = Link(this, newGraph)
    links += link
    newGraph.links += link
    newGraph
  }

  def setAsGateway = this.gateway = true

  def remove(graph: Graph) = links.find(x => x.graph1.number == graph.number || x.graph2.number == graph.number)
    .foreach(_.break)

  def infectAndDestroyNearest = {
    findShortestLinkToGateway(0).link.get
  }

  def findShortestLinkToGateway(currentLen: Int): Result = {
    links.foldLeft(Result(None, currentLen)) {
      case (Result(Some(link), l), next) => Result(Some(link), l)
      case (Result(None, l), next) if next.graph1.gateway || next.graph1.gateway => Result(Some(next), l)
      case (Result(None, l), next) if !next.graph1.gateway && !next.graph1.gateway =>
        if (next.graph1 == this) {
          next.graph2.findShortestLinkToGateway(currentLen + 1)
        } else {
          next.graph1.findShortestLinkToGateway(currentLen + 1)
        }
    }
  }


}

case class Result(var link: Option[Link], val len: Int)

case class Link(graph1: Graph, graph2: Graph) {
  override def toString: String = s"${graph1.number} ${graph2.number}"

  def break = {
    graph1.links -= this
    graph2.links -= this
  }
}

object NodeTypes {
  val Gateway = "gateway"
  val Virus = "virus"
}