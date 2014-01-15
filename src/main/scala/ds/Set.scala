package ds

sealed abstract class Set[+T : Orderable] {
  def insert[U >: T : Orderable](x: U) : Set[U]
  def member[U >: T : Orderable](x: U) : Boolean
  def delete[U >: T : Orderable](x: U) : Set[U]
}

abstract class BST[+T : Orderable] extends Set[T] {
  def depth : Int
  def insert[U >: T : Orderable](x: U) : BST[U]
  def delete[U >: T : Orderable](x: U) : BST[U]
  protected def deleteCurrent[U >: T : Orderable](t: BST[U]) : BST[U]
  protected def fixNodes[U >: T : Orderable] : (U, BST[U])
  def toList : List[T]
}

object BST {

  class Leaf extends BST[Nothing] {
    def member[U : Orderable](x: U) = false
    def insert[U : Orderable](x: U) : BST[U] = Node(x, Leaf, Leaf)
    def delete[U : Orderable](x: U) : BST[U] = this
    protected def deleteCurrent[U : Orderable](t: BST[U]) = t
    protected def fixNodes[U : Orderable] = null
    def depth: Int = 0
    def toList = Nil
    override def toString = "L"
  }

  class Node[+T : Orderable](y: T, left: BST[T], right: BST[T]) extends BST[T] {
    def member[U >: T : Orderable](x: U) = if (x < y) left.member(x)
                       else if (x > y) right.member(x)
                       else true

    def insert[U >: T : Orderable](x: U) : BST[U] = if (x < y) Node(y, left.insert(x), right)
                               else if (x > y) Node(y, left, right.insert(x))
                               else this

    def delete[U >: T : Orderable](x: U) : BST[U] =
                      if (x < y) Node(y, left.delete(x), right)
                      else if (x > y) Node(y, left, right.delete(x))
                      else left.deleteCurrent(right)

    protected def fixNodes[U >: T : Orderable] : (U, BST[U]) = {
      val r = right.fixNodes
      if (r != null) {
        val (ny, nr) = r
        (ny, Node(y, left, nr))
      } else (y, left)
    }

    protected def deleteCurrent[U >: T : Orderable](t: BST[U]): BST[U] = {
       val (ny, nl) = fixNodes
       Node(ny, nl, t)
    }

    def depth : Int = 1 + Math.max(left.depth, right.depth)

    def toList = (left.toList :+ y) ++ right.toList

    override def toString = "(" + y.toString + ", " + left.toString + ", " + right.toString + ")"
  }

  object Node {
    def apply[T : Orderable](y: T, left: BST[T], right: BST[T]) = new Node(y, left, right)
  }
  val Leaf = new Leaf()

  def apply[T : Orderable](xs: T*) : BST[T] = xs.foldLeft(Leaf.asInstanceOf[BST[T]])((t, x) => t.insert(x))
}

trait RBT[+T] extends BST[T] {
  def balancedInsert[U >: T : Orderable](x: U) : RBT[U]
  def insert[U >: T : Orderable](x: U) : RBT[U]
}

object RBT {
  sealed abstract class Color
  case object Red extends Color
  case object Black extends Color

  case object Leaf extends BST.Leaf with RBT[Nothing] {
    val color = Black
    override def insert[U : Orderable](x: U): RBT[U] = Node(Red, x, Leaf, Leaf)
    def balancedInsert[U : Orderable](x: U): RBT[U] = insert(x)
    override def delete[U : Orderable](x: U) = throw new UnsupportedOperationException
  }

  case class Node[+T : Orderable](color: Color, v: T, left: RBT[T], right: RBT[T]) extends BST.Node[T](v, left, right) with RBT[T] {
    def balancedInsert[U >: T : Orderable](e: U): RBT[U] = {
      def balance(cn: RBT[U]): RBT[U] = cn match {
        case Node(Black, z, Node(Red, y, Node(Red, x, a, b), c), d) =>
          Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
        case Node(Black, z, Node(Red, x, a, Node(Red, y, b, c)), d) =>
          Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
        case Node(Black, x, a, Node(Red, y, b, Node(Red, z, c, d))) =>
          Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
        case Node(Black, x, a, Node(Red, z, Node(Red, y, b, c), d)) =>
          Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
        case c => c
      }

      if (e < v) balance(Node(color, v, left.balancedInsert(e), right))
      else if (e > v) balance(Node(color, v, left, right.balancedInsert(e)))
      else this
    }

    override def insert[U >: T : Orderable](e: U) : RBT[U] = {
      val Node(_, a1,a2,a3) = balancedInsert(e)
      Node(Black, a1, a2, a3)
    }

    override def delete[U >: T : Orderable](x: U) = throw new UnsupportedOperationException
  }

  def apply[T : Orderable](xs: T*) = xs.foldLeft(Leaf.asInstanceOf[RBT[T]])((t, x) => t.insert(x))
}
