import scala.collection.mutable

/**
  * @author akanas
  */
class Graph {
  val nodes = mutable.HashSet[Node]()

  def setAsGateway(number: Int) = nodes.find(_.number == number).foreach(_.gate = true)

  def findNearestEdgeToGateway() = ???

  def addEdge(number1: Int, number2: Int) = nodes.find(_.number == number1)
    .orElse(nodes.find(_.number == number2))
    .orElse(Some(newNode(number1)))
    .foreach(_.addNode(number1, number2))

  def newNode(number: Int): Node = {
    val node: Node = Node(number)
    nodes += node
    node
  }
}

case class Node(number: Int) {
  var gate: Boolean = false
  val edges = mutable.HashSet[Edge]()

  def addNode(number: Int) = {
    if (number == this.number) {
      throw new RuntimeException("All numbers must be unique")
    }
    val node: Node = Node(number)
    val edge: Edge = Edge(this, node)
    edges += edge
    node.edges += edge
    node
  }
  
  def addNode(number1: Int, number2: Int) = {
    if (number1 == number) {
      addNode(number2)
    } else if (number2 == number){
      addNode(number1)
    } else {
      throw new RuntimeException(s"Incorrect numbers supplied $number2 and $number1 for node $number")
    }
  }
}

case class Edge(n1: Node, n2: Node) {
  def break = {
    n1.edges -= this
    n2.edges -= this
  }
}
