package ds

sealed abstract class PriorityQueue[+T : Orderable] {
  def insert[U >: T : Orderable](x: U) : PriorityQueue[U]
  def deleteMin() : PriorityQueue[T]
  def findMin : T
  def isEmpty: Boolean
}

object PriorityQueue {

  case class Node[+T : Orderable](v: T, rank: Int = 0, children: List[Node[T]] = Nil) {
    def link[U >: T : Orderable](other: Node[U]) =
      if (v < other.v) Node(v, rank+1, other :: children)
      else Node(other.v, other.rank+1, this :: other.children)
  }

  case class BinomialQueue[+T : Orderable](nodes: List[Node[T]]) extends PriorityQueue[T] {
    def insert[U >: T : Orderable](x: U) : BinomialQueue[U] = BinomialQueue(insertNode(Node(x), nodes))
    override def findMin: T = getMinNode.v

    override def deleteMin(): PriorityQueue[T] = {
      val minNode = getMinNode
      BinomialQueue(meldLists(nodes.filter(_ != minNode), minNode.children.reverse))
    }

    def meld[U >: T : Orderable](that: BinomialQueue[U]) : BinomialQueue[U] = (this, that) match {
      case (BinomialQueue(Nil), q) => q
      case (q, BinomialQueue(Nil)) => q
      case (BinomialQueue(thisList), BinomialQueue(thatList)) => BinomialQueue(meldLists(thisList, thatList))
    }

    private def getMinNode : Node[T] = {
      lazy val cmp: Ordering[Node[T]] = new Ordering[Node[T]] {
        override def compare(x: Node[T], y: Node[T]): Int = x.v compare y.v
      }
      nodes.min(cmp)
    }

    override def isEmpty: Boolean = nodes.isEmpty
  }

  private def meldLists[T : Orderable](q1: List[Node[T]], q2: List[Node[T]]) : List[Node[T]] = (q1, q2) match {
    case (Nil, q) => q
    case (q, Nil) => q
    case (x :: xs, y :: ys) => if (x.rank < y.rank) x :: meldLists(xs, y :: ys)
                               else if (x.rank > y.rank) y :: meldLists(x :: xs, ys)
                               else insertNode(x.link(y), meldLists(xs, ys))
  }

  private def insertNode[T : Orderable](n: Node[T], lst: List[Node[T]]) : List[Node[T]] = lst match {
    case Nil => List(n)
    case x :: xs => if (n.rank < x.rank) n :: x :: xs
    else insertNode(x.link(n), xs)
  }

  def apply[T : Orderable](xs: T*): PriorityQueue[T] = xs.foldLeft(BinomialQueue[T](Nil))((t, x) => t.insert(x))
}