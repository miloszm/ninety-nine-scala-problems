import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Exercises: http://aperiodic.net/phil/scala/s-99/#graphs
  */
abstract class GraphBase[T, U] {

  case class Edge(n1: Node, n2: Node, value: U) {
    def toTuple = (n1.value, n2.value, value)
    override def toString: String = n1.value.toString + "-" + n2.value.toString
  }

  case class Node(value: T) {
    var adj: List[Edge] = Nil

    // neighbors are all nodes adjacent to this node.
    def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)

    // P86
    def degree: Int = ???

    override def toString: String = this.value.toString
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

object SubsetsGenerator {

  def generateSubsets[T](set:List[T]): List[List[T]] = {
    def go(set:List[T], index:Int): List[List[T]] = {
      if (index > 0) {
        val all = go(set, index-1)
        val item = set(index-1)
        val moreSubsets =
          for {subset <- all}
            yield {
              item :: subset
            }
        all ::: moreSubsets
      }
      else {
        List(List())
      }
    }
    go(set, set.size)
  }

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


/////////////////////////////////////////////////////////////////////


  // P83
  def spanningTrees: List[Graph[T, U]] = {

    val sol = mutable.Set[String]()

    def isOK(ee:List[Edge]):Boolean = {
      ee.map(e => List(e.n1, e.n2)).flatten.distinct.size == 5
    }

    def edgesToStr(ee:List[Edge]):String = {

      def go(ee:List[Edge], prev:Option[Node], acc:String):String = ee match {
        case Nil => acc
        case h :: t =>
          if (prev.isEmpty) {
            go(t, Some(h.n2), acc + ee.head.n1.value + ee.head.n2.value)
          }
          else {
            val f1 = ee.find(e => e.n1 == prev.get)
            val f2 = ee.find(e => e.n2 == prev.get)
            (f1,f2) match {
              case (Some(x),_) => go(ee.filter(_ != x), Some(x.n2), acc + x.n2.value)
              case (None,Some(y)) => go(ee.filter(_ != y), Some(y.n1), acc + y.n1.value)
              case _ => go(ee, None, acc)// should not happen
            }
          }
      }
      go(ee, None, "")
    }

    def go(edges: List[Edge], stNumEdges: Int): List[List[Edge]] = {
      if (sol.contains(edgesToStr(edges))){
        List()
      }
      else {
        edges match {
          case ee if (ee.size == stNumEdges) => if (isOK(ee)) sol += edgesToStr(ee); List(ee)
          case ee if (ee.size > stNumEdges) =>
            val x = for {
              e <- ee
              if (canBeRemoved(e, ee))
            } yield {
              go(ee.filter(_ != e), stNumEdges)
            }
            x.flatten.filter(!_.isEmpty)
        }
      }
    }

    def canBeRemoved(e:Edge, ee:List[Edge]):Boolean = {
      (ee.filter(edge => edge.n1 == e.n1 || edge.n2 == e.n1).size >= 2) &&
      (ee.filter(edge => edge.n1 == e.n2 || edge.n2 == e.n2).size >= 2)
    }

    if (edges.isEmpty) {
      List(Graph.termLabel[T, U](nodes.keys.toList, List()))
    } else if (nodes.values.toList.diff(edges.flatMap(e => List(e.n1, e.n2))).size > 0) {
      List()
    } else {
      val trees: List[List[Edge]] = go(edges, nodes.size-1)
      println(trees.distinct)
      println(trees.distinct.size)
      println(trees.size)
      val treesSorted = trees.map(_.sortBy(_.toString)).distinct
      println(treesSorted.size)
      println(s"sol=${sol.size}")
      sol.toList.sortBy(_.toString).foreach(a => println(a))
      trees.distinct.map { (l: List[Edge]) =>
        val nodes = l.foldLeft(List[T]())((a,b) => b.n1.value::(b.n2.value::a)).distinct
        val edges = l.map(e => (e.n1.value, e.n2.value, e.value))
        Graph.termLabel[T, U](nodes, edges)
      }
    }

  }


/////////////////////////////////////////////////////////////////////




  // P83
  def spanningTrees3: List[Graph[T, U]] = {

    def go(nodes:List[Node]):List[List[Edge]] = {
      println(s"nodes $nodes")
      nodes match {
        case n1 :: n2 :: Nil => List(getEdge(n1,n2).toList)
        case h :: t =>
          val goresult = if (go(t).isEmpty) List(getEdge(t.head,h).toList:::getEdge(t.tail.head,h).toList) else go(t)
            val ll = for {
              l <- goresult
            } yield {
              addVertex(l, h)
            }
            ll.flatten
      }
    }

    def addVertex(ee:List[Edge], n:Node):List[List[Edge]] = {
      val sol = mutable.Set[List[Edge]]()
      for(v <- toSetOfVertices(ee)
          if (existsEdge(v,n)))
      {
        val newEdges = getEdge(v,n).get::ee
        sol += newEdges
        permuteEdges(ee, n, sol)
      }
      println(s"sol $sol")

      sol.toList
    }

    def permuteEdges(ee:List[Edge], n:Node, sol:mutable.Set[List[Edge]]):Unit = {
      if (!ee.isEmpty) {
        for {
          s <- SubsetsGenerator.generateSubsets(ee)
          d = ee.diff(s)
          if (!d.isEmpty)
          if (d.forall(e => existsEdge(e.n1, n) && existsEdge(e.n2, n)))
        } {
          val newEdges = for {
            dd <- d
          } yield {
            getEdge(dd.n1, n).toList:::getEdge(dd.n2, n).toList
          }
          val x = newEdges.flatten.distinct
          sol += x ::: s
        }
      }
    }

    def toSetOfVertices(ee:List[Edge]):Set[Node] = ee match {
      case Nil => Set.empty
      case h::tail => toSetOfVertices(tail) + h.n1 + h.n2
    }

    def existsEdge(n1:Node, n2:Node):Boolean = getEdge(n1, n2).isDefined

    def getEdge(n1:Node, n2:Node):Option[Edge] = {
      edges.find(e => (e.n1 == n1 && e.n2 == n2 || e.n1 == n2 && e.n2 == n1))
    }

    if (edges.isEmpty) {
      List(Graph.termLabel[T, U](nodes.keys.toList, List()))
    } else if (nodes.values.toList.diff(edges.flatMap(e => List(e.n1, e.n2))).size > 0) {
      List()
    } else {
      val trees: List[List[Edge]] = go(nodes.values.toList)
      trees.map { (l: List[Edge]) =>
        Graph.termLabel[T, U](l.flatMap(e => List(e.n1.value, e.n2.value)), l.flatMap(e => List((e.n1.value, e.n2.value, e.value))))
      }
    }
  }



  // P83
  def spanningTrees2: List[Graph[T, U]] = {
    def go1(edges:List[Edge], cur:List[Edge]):List[List[Edge]] = {
      val trees = ListBuffer[List[Edge]]()
      nodes.values.toList.foreach{
        n => findSingleSpanningTree(n, nodes.values.toList.filterNot(_ == n), nodes.size -1, List(n), trees)
      }
      trees.toList

    }

    def findSingleSpanningTree(node: Node, nds: List[Node], counter: Int,
                               current: List[Node], acc:ListBuffer[List[Edge]]): List[Edge] = {
//      println(s"--Node: $node | Nodes: $nds | Counter: $counter | Current: $current | Acc: $acc ")
//      if (counter == 0)
//        println(s"--Current: $current")
      if (counter == 1)
        toEdges(current)
      else {
        val x = for {
          v <- everyAdjacentVertexTo(node, nds)
        } yield {
          findSingleSpanningTree(v, nds.filterNot(_ == v), counter - 1, v::current, acc)
        }
        acc += x.flatten
        x.flatten
      }
    }

    def everyAdjacentVertexTo(node: Node, nds:List[Node]): List[Node] = {
      for {
        v <- nds
        if (existsEdge(node,v))
      } yield {
        v
      }
    }

    def existsEdge(n1:Node, n2:Node):Boolean = getEdge(n1, n2).isDefined

    def getEdge(n1:Node, n2:Node):Option[Edge] = {
      edges.find(e => (e.n1 == n1 && e.n2 == n2 || e.n1 == n2 && e.n2 == n1))
    }

    def toEdges2(nds: List[List[Node]]):List[List[Edge]] = {
      println(nds)
      for {
        l <- nds
        z = l.zip(l.drop(1))
      } yield {
        for {
          (n1, n2) <- z
          if (existsEdge(n1, n2))
        } yield {
          getEdge(n1, n2).get
        }
      }
    }

    def toEdges(nds: List[Node]):List[Edge] = {
      val l = nds
      val z = l.zip(l.drop(1))
      for {
        (n1, n2) <- z
        if (existsEdge(n1, n2))
      } yield {
        getEdge(n1, n2).get
      }
    }

//    def higherNode(edge:Edge):Node = {
//      val n1 = edge.n1
//      val n2 = edge.n2
//      if (n1.hashCode() > n2.hashCode()) n1 else n2
//    }








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
