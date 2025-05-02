package playgraund.InnerClasses

//https://docs.scala-lang.org/tour/inner-classes.html#inner-main
object GraphWithInnerNodes extends App {

  val graph1: Graph = new Graph
  val node1: graph1.Node = graph1.newNode
  val node2: graph1.Node = graph1.newNode
  val node3: graph1.Node = graph1.newNode
  node1.connectTo(node2)
  node3.connectTo(node1)

  val graph2: Graph = new Graph
  val node33: graph2.Node = graph2.newNode
  // node1.connectTo(node33) // illegal!

}

class Graph():

  var nodes: List[Node] = List.empty

  def newNode: Node =
    val n = Node()
    nodes = n :: nodes
    n

  class Node:
    var connectedNodes: List[Node] = Nil

    def connectTo(node: Node): Unit =
      if !connectedNodes.exists(node.equals) then connectedNodes = node :: connectedNodes


class Graph2:
  class Node:
    var connectedNodes: List[Graph2#Node] = Nil
    def connectTo(node: Graph2#Node): Unit =
      if !connectedNodes.exists(node.equals) then
        connectedNodes = node :: connectedNodes

  var nodes: List[Node] = Nil
  def newNode: Node =
    val res = Node()
    nodes = res :: nodes
    res