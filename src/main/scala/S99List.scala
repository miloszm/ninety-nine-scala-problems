import scala.annotation.tailrec

/**
  * Exercises: http://aperiodic.net/phil/scala/s-99/#lists
  *
  * For the list exercises, avoid using builtin functions such as length, slice, ...
  */
object S99List {
  // P01
  def last[T](l: List[T]): T = l match {
    case h :: Nil => h
    case a :: tail => last(tail)
    case Nil => throw new NoSuchElementException("Empty list")
  }

  // P02
  def penultimate[T](l: List[T]): T = l match {
    case h :: _ :: Nil => h
    case h :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException("List too small")
  }

  // P03
  def nth[T](pos: Int, l: List[T]): T = {

    def _nth[T](current: Int, l: List[T]): T = l match {
      case h:: tail => if (pos == current) h else _nth(current + 1, tail)
      case _ => throw new NoSuchElementException("")
    }

    if (pos > 0)
      _nth(0, l)
    else
      throw new IllegalArgumentException
  }

  // P04
  def length[T](l: List[T]): Int = {
    @tailrec
    def length(counter: Int, l:List[T]): Int =
      l match {
        case Nil => counter
        case h::tail => length(counter + 1, tail)
      }
    length(0, l)
  }

  def lengthFunctional[T](l: List[T]): Int = l.foldLeft(0) {(c, _) => c+ 1 }

  // P05
  def reverse[T](l: List[T]): List[T] = {
    @tailrec
    def _reverse(output: List[T], l: List[T]): List[T] = {
      l match {
        case Nil => output
        case a::tail => _reverse(a::output, tail)
      }
    }
    _reverse(List(), l)
  }

  def reverseFunctional[T](l: List[T]): List[T] =
    l.foldLeft(List[T]()) {(output, h) => h :: output }


  // P06
  def isPalindrome[T](l: List[T]): Boolean =
    l == reverse(l)

  // P07
  def flatten(xs: List[Any]): List[Any] = xs match {
    case Nil => Nil
    case (head: List[_]) :: tail => flatten(head) ++ flatten(tail)
    case head :: tail => head :: flatten(tail)
  }

  // P08
  def compress[T](l: List[T]): List[T] = {
    l.foldRight(List[T]()) { (e, acc) => acc match {
      case x :: tail if x == e => acc
      case _ => e::acc
      }
    }
  }

  // P09
  def pack[T](l: List[T]): List[List[T]] = {
    def go(acc: List[List[T]], rem: List[T]): List[List[T]] = {
      rem  match {
        case Nil => acc
        case h::t if acc.isEmpty || acc.last.head != h => go(acc:::List(List(h)), t)
        case h:: t => go(acc.init:::List(acc.last:::List(h)), t)
      }
    }
    go(List.empty, l)
  }

  // P10
  def encode[T](l: List[T]): List[(Int, T)] = {
    l.foldRight(List[(Int, T)]()) {(e, acc) => acc match {
      case x::tail => if (x._2 == e) (x._1 + 1, e)::tail else (1, e)::acc
      case _ => (1, e)::acc
    }
    }
  }

  // P11
  def encodeModified[T](l: List[T]): List[Any] = {
    encode(l).foldRight(List[Any]()){ (e, acc) => e match {
      case (i, x) if i > 1 => (i, x)::acc
      case (i, x) => x::acc
    }
    }
  }

  // P12
  def decode[T](l: List[(Int, T)]): List[T] = {
    for {
      e <- l
      i <- 0 until e._1
    } yield {
      e._2
    }
  }

  // P13
  def encodeDirect[T](l: List[T]): List[(Int, T)] = ???

  // P14
  def duplicate[T](l: List[T]): List[T] = ???

  // P15
  def duplicateN[T](n: Int, l: List[T]): List[T] = l.flatMap(List.fill(n)(_))


  // P16
  def drop[T](n: Int, l: List[T]): List[T] = {
    l.zipWithIndex
      .filter { p => (p._2+1) % n != 0}
      .map(p => p._1)
  }

  // P17
  def split[T](n: Int, l: List[T]): (List[T], List[T]) = {
    (l.take(n), l.takeRight(l.length - n))
    //l.splitAt(n)
  }

  def split2[T](n: Int, l: List[T]): (List[T], List[T]) =
    l.foldRight((l.size, (List[T](), List[T]())))((x, acc) =>
      acc match {
        case (i, (l1, l2)) if i <= n => (i - 1, (x :: l1, l2))
        case (i, (l1, l2)) => (i - 1, (l1, x:: l2))
      }
    )._2

  // P18
  def slice[T](from: Int, to: Int, l: List[T]): List[T] = {
    l.zipWithIndex.filter(p => p._2 >= from && p._2 < to).map( p => p._1)
  }

  // P19
  def rotate[T](n: Int, l: List[T]): List[T] = {
    if (n > 0) {
      List(l.slice(n, l.length), l.slice(0, n)).flatten
    } else {
      List(l.slice(n, l.length), l.slice(0, n)).flatten
    }
  }


  // P20
  def removeAt[T](n: Int, l: List[T]): (List[T], T) = ???

  // P21
  def insertAt[T](elem: T, n: Int, l: List[T]): List[T] = ???

  // P22
  def range(from: Int, to: Int): List[Int] = ???

  // P23
  def randomSelect[T](n: Int, l: List[T]): List[T] = ???

  // P24
  def lotto(howMany: Int, outOf: Int): List[Int] = ???

  // P25
  def randomPermute[T](l: List[T]): List[T] = ???

  // P26
  def combinations[T](n: Int, l: List[T]): List[List[T]] = ???

  // P27
  def group3[T](l: List[T]): List[List[List[T]]] = ???

  def group[T](groupSizes: List[Int], l: List[T]): List[List[List[T]]] = ???

  // P28
  def lsort[T](l: List[List[T]]): List[List[T]] = ???

  def lsortFreq[T](l: List[List[T]]): List[List[T]] = ???
}
