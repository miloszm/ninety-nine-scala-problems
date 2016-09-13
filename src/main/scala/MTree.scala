/**
  * Exercises: http://aperiodic.net/phil/scala/s-99/#mtrees
  */
case class MTree[+T](value: T, children: List[MTree[T]]) {
  override def toString: String = value + children.map(_.toString).mkString + "^"

  // P70
  def nodeCount: Int = 1 + children.map(_.nodeCount).sum

  // P71
  def internalPathLength: Int = ???

  // P72
  def postorder: List[T] = ???

  // P73
  def toLispyString: String = ???
}

object MTree {
  def apply[T](value: T) = new MTree(value, List())

  // P70
  implicit def string2MTree(s: String): MTree[String] = {
    s match {
      case "" | "^" => throw new IllegalArgumentException
      case x if !x.endsWith("^") => throw new IllegalArgumentException
      case x if x.count(_ == '^') != x.count(_ != '^') => throw new IllegalArgumentException
      case x => {
        val list = convertToList(s.toList, List(), 0)
        for {
          l <- list
        } yield {

        }
      }
        ???
    }

    def go(s: String, pos: Int, nbArrow: Int, tree: MTree[String]) = {
      for {
        c <- s
      } yield {
        if (c != '^') {
          tree.copy(children = tree.children:::List(MTree(c)))
          //pos ++
        } else {
          val children = tree.children

        }
      }
    }

    def convertToList(s: List[Char], acc: List[List[String]], current: Int):List[List[String]] = s match {
      case h::t if h != '^' =>
        val s = if (current == 0) List() else acc.slice(0, current)
        val e = acc.slice(current+1, acc.length)
        convertToList(t, s:::(acc(current):::List(h.toString))::e, current)
      case h::t if h == '^' => convertToList(t, acc, current-1)
    }
    ???
  }

  def fromString(s: String): MTree[String] = string2MTree(s)

  // P73
  def fromLispyString(s: String): MTree[String] = ???
}
