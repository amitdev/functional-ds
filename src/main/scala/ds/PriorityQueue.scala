package ds

import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.collection.generic.{CanBuildFrom, OrderedTraversableFactory, GenericOrderedCompanion, GenericOrderedTraversableTemplate}
import scala.collection._
import scala.Iterator

case class Node[A](v: A, rank: Int = 0, children: List[Node[A]] = Nil)(implicit val ord: Ordering[A]) {
  def link(other: Node[A]) =
    if (ord.compare(v, other.v) < 0) Node(v, rank+1, other :: children)
    else Node(other.v, other.rank+1, this :: other.children)

  def toList : List[A] = v :: children.flatMap(_.toList)
}

final case class PriorityQueue[T](private val nodes: List[Node[T]])(implicit val ord: Ordering[T])
  extends Iterable[T]
    with GenericOrderedTraversableTemplate[T, PriorityQueue]
    with IterableLike[T, PriorityQueue[T]] {

  import PriorityQueue._

  def +(x: T) : PriorityQueue[T] = PriorityQueue(insertNode(Node(x), nodes))
  def findMin: T = getMinNode.v

  def deleteMin(): PriorityQueue[T] = {
    val minNode = getMinNode
    PriorityQueue(meldLists(nodes.filter(_ != minNode), minNode.children.reverse))
  }

  def meld(that: PriorityQueue[T]) : PriorityQueue[T] = (this, that) match {
    case (PriorityQueue(Nil), q) => q
    case (q, PriorityQueue(Nil)) => q
    case (PriorityQueue(thisList), PriorityQueue(thatList)) => PriorityQueue(meldLists(thisList, thatList))
  }

  private def getMinNode : Node[T] = {
    lazy val cmp: Ordering[Node[T]] = new Ordering[Node[T]] {
      override def compare(x: Node[T], y: Node[T]): Int = ord.compare(x.v, y.v)
    }
    nodes.min(cmp)
  }

  override def isEmpty: Boolean = nodes.isEmpty

  override def orderedCompanion: GenericOrderedCompanion[PriorityQueue] = PriorityQueue

  override def iterator: Iterator[T] = nodes.flatMap(_.toList).iterator

  override protected[this] def newBuilder: mutable.Builder[T, PriorityQueue[T]] = PriorityQueue.newBuilder
}

object PriorityQueue extends OrderedTraversableFactory[PriorityQueue] {

  private def meldLists[T](q1: List[Node[T]], q2: List[Node[T]]) : List[Node[T]] = (q1, q2) match {
    case (Nil, q) => q
    case (q, Nil) => q
    case (x :: xs, y :: ys) => if (x.rank < y.rank) x :: meldLists(xs, y :: ys)
                               else if (x.rank > y.rank) y :: meldLists(x :: xs, ys)
                               else insertNode(x.link(y), meldLists(xs, ys))
  }

  private def insertNode[T](n: Node[T], lst: List[Node[T]]) : List[Node[T]] = lst match {
    case Nil => List(n)
    case x :: xs => if (n.rank < x.rank) n :: x :: xs
    else insertNode(x.link(n), xs)
  }

  override def newBuilder[A](implicit ord: Ordering[A]): mutable.Builder[A, PriorityQueue[A]] = new ArrayBuffer[A] mapResult {xs =>
    var r = new PriorityQueue[A](Nil)
    for (x <- xs) r = r + x
    r
  }

  implicit def canBuildFrom[A](implicit ord: Ordering[A]): CanBuildFrom[Coll, A, PriorityQueue[A]] = new GenericCanBuildFrom[A]
}