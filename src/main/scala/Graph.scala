import scala.collection.mutable.ListBuffer

/**
  * Exercises: http://aperiodic.net/phil/scala/s-99/#graphs
  */
abstract class GraphBase[T, U] {

  case class Edge(n1: Node, n2: Node, value: U) {
    def toTuple = (n1.value, n2.value, value)
  }

  case class Node(value: T) {
    var adj: List[Edge] = Nil

    // neighbors are all nodes adjacent to this node.
    def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)

    // P86
    def degree: Int = ???
  }

  var nodes: Map[T, Node] = Map()
  var edges: List[Edge] = Nil

  // If the edge E connects N to another node, returns the other node,
  // otherwise returns None.
  def edgeTarget(e: Edge, n: Node): Option[Node]

  def canEqual(other: Any): Boolean

  override def equals(o: Any) = o match {
    case g: GraphBase[_, _] =>
      (g canEqual this) &&
        (nodes.keys.groupBy(identity) == g.nodes.keys.groupBy(identity)) &&
        (edges.map(_.toTuple).groupBy(identity) == g.edges.map(_.toTuple).groupBy(identity))
    case _ => false
  }

  override def hashCode(): Int =
    41 * (41 + nodes.keys.groupBy(identity).hashCode) + edges.map(_.toTuple).groupBy(identity).hashCode

  def addNode(value: T): Unit = {
    val n = new Node(value)
    nodes = Map(value -> n) ++ nodes
  }

  // P80
  def toTermForm: (List[T], List[(T, T, U)]) = {
    val nodesRes = nodes.keys.toList
    val edgesRes: List[(T, T, U)] = edges.map(e => (e.n1.value, e.n2.value, e.value))

    (nodesRes, edgesRes)
  }

  def toAdjacentForm: List[(T, List[(T, U)])] = {
    nodes.values.map { n => (n.value, edges
      .filter(e => e.n1 == n || e.n2 == n)
      .foldLeft(Set[(T,U)]()) { (acc, e) =>
        if (e.n1 == n)
          acc + ((e.n2.value, e.value))
        else
          acc + ((e.n1.value, e.value))
      }.toList)
    }.toList
  }

  override def toString: String = {
    val (edgeStrings, edgesNodes) = edges.foldRight((List[String](), List[Node]())) {(e, acc) =>
      val maybeLabel = if (e.value != ()) s"/${e.value}" else ""
      (s"${e.n1.value}-${e.n2.value}$maybeLabel" :: acc._1, e.n1::e.n2::acc._2)
    }
    val missingNodes = nodes.values.toList.diff(edgesNodes).map(n => s"${n.value}"):::edgeStrings

    val x = missingNodes.mkString(", ")
    s"[$x]"
  }

  // P81
  def findPaths(from: T, to: T): List[List[T]] = ???

  // P82
  def findCycles(from: T): List[List[T]] = ???

  // P85
  def isIsomorphicTo[R, S](g: GraphBase[R, S]): Boolean = ???

  // P86
  def nodesByDegree: List[T] = ???

  def colorNodes: List[(T, Int)] = ???

  // P87
  def nodesByDepthFrom(from: T): List[T] = ???

  // P88
  def splitGraph: List[GraphBase[T, U]] = ???

  // P89
  def isBipartite: Boolean = ???
}

class Graph[T, U] extends GraphBase[T, U] {
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Graph[_, _]]

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else if (e.n2 == n) Some(e.n1)
    else None

  def addEdge(n1: T, n2: T, value: U): Unit = {
    val e = new Edge(nodes(n1), nodes(n2), value)
    edges = e :: edges
    nodes(n1).adj = e :: nodes(n1).adj
    nodes(n2).adj = e :: nodes(n2).adj
  }

  // P83
  def spanningTrees: List[Graph[T, U]] = {
    def go1(edges:List[Edge], cur:List[Edge]):List[List[Edge]] = {
      val visited = ListBuffer[Node]()
      val res: List[List[Edge]] = for {
        e <- edges
      } yield {
//        println(s"Calling findSingleSpanningTree with Edge: $e | edges: ${edges.filterNot(_ ==e)}")
        //visited += e.n1
        findSingleSpanningTree(e, edges.filterNot(_ == e), nodes.size -1, List(e), List(), visited)
      }.flatten
      res

    }

    def findSingleSpanningTree(edge: Edge, edges: List[Edge], counter: Int,
                               current: List[Edge], acc:List[List[Edge]], visited: ListBuffer[Node] ): List[List[Edge]] = {
      println(s"--Edge: $edge | Edges: $edges | Counter: $counter | Current: $current | Acc: $acc | Visted: $visited")
      if (counter == 1)
        current::acc
      else {
        for {
          (e,n) <- everyAdjacentEdgeTo(edge, edges, visited)
        } yield {
          findSingleSpanningTree(e, edges.filterNot(_ == e), counter - 1, e::current, acc, visited)
        }
      }.flatten
    }

    def everyAdjacentEdgeTo(edge: Edge, edges: List[Edge], visitedNode: ListBuffer[Node]): List[(Edge,Node)] = {
      val adj = edges.filter(e =>  e.n1 == edge.n1 || e.n1 == edge.n2 || e.n2 == edge.n1 || e.n2 == edge.n2)
        .filterNot(e => visitedNode.contains(e.n1) || visitedNode.contains(e.n2))
      val x = adj.zip(adj.map {e => if(e.n1 == edge.n1 ) {visitedNode += e.n1;e.n1} else {visitedNode += e.n2;e.n2}})
//      println(s"edge $edge edges $edges visited $visitedNode --> $x")
      x
    }

    if (edges.isEmpty) {
      List(Graph.termLabel[T, U](nodes.keys.toList, List()))
    } else if (nodes.values.toList.diff(edges.flatMap(e => List(e.n1, e.n2))).size > 0) {
      List()
    } else {
      val trees: List[List[Edge]] = go1(edges, List(edges.head))
      trees.map { (l: List[Edge]) =>
        Graph.termLabel[T, U](l.flatMap(e => List(e.n1.value, e.n2.value)), l.flatMap(e => List((e.n1.value, e.n2.value, e.value))))
      }
    }
  }

  def isTree: Boolean = ???

  def isConnected: Boolean = ???

  // P84
  def minimalSpanningTree(implicit o: U => Ordered[U]): Graph[T, U] = ???
}

class Digraph[T, U] extends GraphBase[T, U] {
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Digraph[_, _]]

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else None

  def addArc(source: T, dest: T, value: U): Unit = {
    val e = new Edge(nodes(source), nodes(dest), value)
    edges = e :: edges
    nodes(source).adj = e :: nodes(source).adj
  }

  override def toAdjacentForm: List[(T, List[(T, U)])] = {
    nodes.values.map { n => (n.value, edges
      .filter(e => e.n1 == n)
      .foldLeft(Set[(T, U)]()) { (acc, e) =>
        acc + ((e.n2.value, e.value))
      }.toList)
    }.toList
  }
}

abstract class GraphObjBase {
  type GraphClass[T, U]

  def addLabel[T](edges: List[(T, T)]) =
    edges.map(v => (v._1, v._2, ()))

  def term[T](nodes: List[T], edges: List[(T, T)]) =
    termLabel(nodes, addLabel(edges))

  def termLabel[T, U](nodes: List[T], edges: List[(T, T, U)]): GraphClass[T, U]

  def addAdjacentLabel[T](nodes: List[(T, List[T])]) =
    nodes.map(a => (a._1, a._2.map((_, ()))))

  def adjacent[T](nodes: List[(T, List[T])]) =
    adjacentLabel(addAdjacentLabel(nodes))

  def adjacentLabel[T, U](nodes: List[(T, List[(T, U)])]): GraphClass[T, U]

  def fromString(s: String): GraphClass[String, Unit] = ???

  def fromStringLabel(s: String): GraphClass[String, Any] = ???
}

object Graph extends GraphObjBase {
  type GraphClass[T, U] = Graph[T, U]

  def termLabel[T, U](nodes: List[T], edges: List[(T, T, U)]) = {
    val g = new Graph[T, U]
    nodes.foreach(g.addNode)
    edges.foreach((g.addEdge _).tupled)
    g
  }

  def adjacentLabel[T, U](nodes: List[(T, List[(T, U)])]) = {
    val g = new Graph[T, U]
    for ((v, a) <- nodes) g.addNode(v)
    for ((n1, a) <- nodes; (n2, l) <- a) {
      if (!g.nodes(n1).neighbors.contains(g.nodes(n2)))
        g.addEdge(n1, n2, l)
    }
    g
  }

  // P80
  override def fromString(s: String): GraphClass[String, Unit] = {
    val stringRegex = "\\[(.*?)\\]".r
    val edgeRegex = "(.*?)-(.*?)".r

    def go(stringNodes: List[String], nodes: List[String], edges: List[(String, String)]):GraphClass[String, Unit] = {
      stringNodes match {
        case Nil => Graph.term(nodes.distinct, edges)
        case h::t => h match {
          case edgeRegex(n1, n2) => go(t, n1::n2::nodes, (n1, n2)::edges)
          case x => go(t, x::nodes, edges)
        }
      }
    }

    s match {
      case stringRegex(string) => go(string.split(",").map(_.trim).toList, List(), List())
      case _ => throw new IllegalArgumentException("")
    }
  }

  override def fromStringLabel(s: String): GraphClass[String, Any] = {
    val stringRegex = "\\[(.*?)\\]".r
    val edgeRegex = "(.*?)-(.*?)".r
    val labelRegex = "/(.*?)".r

    def go(stringNodes: List[String], nodes: List[String], edges: List[(String, String, Any)]):GraphClass[String, Any] = {
      stringNodes match {
        case Nil => Graph.termLabel(nodes.distinct, edges)
        case h::t => h match {
          case edgeRegex(n1, n2) => n2 match {
            case labelRegex(label) =>  go(t, n1::n2::nodes, (n1, n2, label)::edges)
            case _ => go(t, n1::n2::nodes, (n1, n2, ())::edges)
          }
          case x => go(t, x::nodes, edges)
        }
      }
    }

    s match {
      case stringRegex(string) => go(string.split(",").map(_.trim).toList, List(), List())
      case _ => throw new IllegalArgumentException("")
    }
  }

}

object Digraph extends GraphObjBase {
  type GraphClass[T, U] = Digraph[T, U]

  def termLabel[T, U](nodes: List[T], edges: List[(T, T, U)]) = {
    val g = new Digraph[T, U]
    nodes.foreach(g.addNode)
    edges.foreach((g.addArc _).tupled)
    g
  }

  def adjacentLabel[T, U](nodes: List[(T, List[(T, U)])]) = {
    val g = new Digraph[T, U]
    for ((n, a) <- nodes) g.addNode(n)
    for ((s, a) <- nodes; (d, l) <- a) g.addArc(s, d, l)
    g
  }
}
