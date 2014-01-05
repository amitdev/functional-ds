package ds

sealed abstract class Set[+T <% Ordered[T]] {
  def insert[U >: T <% Ordered[U]](x: U) : Set[U]
  def member[U >: T <% Ordered[U]](x: U) : Boolean
}

abstract class BST[+T <% Ordered[T]] extends Set[T] {
  def depth : Int
  def insert[U >: T <% Ordered[U]](x: U) : BST[U]
}

object BST {

  class Leaf extends BST[Nothing] {
    def member[U <% Ordered[U]](x: U) = false
    def insert[U <% Ordered[U]](x: U) : BST[U] = Node(x, Leaf, Leaf)
    def depth: Int = 0
  }

  class Node[+T <% Ordered[T]](y: T, left: BST[T], right: BST[T]) extends BST[T] {        
    def member[U >: T <% Ordered[U]](x: U) = if (x < y) left.member(x)
                       else if (x > y) right.member(x)
                       else true

    def insert[U >: T <% Ordered[U]](x: U) : BST[U] = if (x < y) Node(y, left.insert(x), right)
                               else if (x > y) Node(y, left, right.insert(x))
                               else this

    def depth : Int = 1 + Math.max(left.depth, right.depth)
  }
  
  object Node {
    def apply[T <% Ordered[T]](y: T, left: BST[T], right: BST[T]) = new Node(y, left, right)
  }
  val Leaf = new Leaf()

  def apply[T <% Ordered[T]](xs: T*) : BST[T] = xs.foldLeft(Leaf.asInstanceOf[BST[T]])((t, x) => t.insert(x))
}

trait RBT[+T] extends BST[T] {
  def balancedInsert[U >: T <% Ordered[U]](x: U) : RBT[U]
  def insert[U >: T <% Ordered[U]](x: U) : RBT[U]
}

object RBT {
  sealed abstract class Color
  case object Red extends Color
  case object Black extends Color

  case object Leaf extends BST.Leaf with RBT[Nothing] {
    val color = Black
    override def insert[U <% Ordered[U]](x: U): RBT[U] = Node(Red, x, Leaf, Leaf)
    def balancedInsert[U <% Ordered[U]](x: U): RBT[U] = insert(x)
  }

  case class Node[+T <% Ordered[T]](color: Color, v: T, left: RBT[T], right: RBT[T]) extends BST.Node[T](v, left, right) with RBT[T] {
    def balancedInsert[U >: T <% Ordered[U]](e: U): RBT[U] = {
      def balance(cn: RBT[U]): RBT[U] = cn match {
        case         Node(Black, z,
                   Node(Red, y,
                Node(Red, x,
                       a,    b),   c),   d) =>
          Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
        case Node(Black, z,
            Node(Red, x,
           a, Node(Red, y,
                       b, c)),  d) =>
          Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
        case Node(Black, x,
                       a,  Node(Red, y,
                                   b, Node(Red, z,
                                               c, d))) =>
          Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
        case Node(Black, x,
                      a,  Node(Red, z,
                         Node(Red, y,
                                  b, c), d)) =>
          Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
        case c => c
      }

      if (e < v) balance(Node(color, v, left.balancedInsert(e), right))
      else if (e > v) balance(Node(color, v, left, right.balancedInsert(e)))
      else this
    }

    override def insert[U >: T <% Ordered[U]](e: U) : RBT[U] = {
      val Node(_, a1,a2,a3) = balancedInsert(e)
      Node(Black, a1, a2, a3)
    }
  }

  def apply[T <% Ordered[T]](xs: T*) = xs.foldLeft(Leaf.asInstanceOf[RBT[T]])((t, x) => t.insert(x))
}