package ds

import java.util.NoSuchElementException

trait Stack[+T] {
  def isEmpty: Boolean
  def cons[U >: T](t: U): Stack[U]
  def head: T
  def tail: Stack[T]
}

sealed abstract class CList[+T] extends Stack[T] {
  def cons[U >: T](t: U): CList[U] = new Cons(t, this)
  def ++[U >: T](ys: CList[U]): CList[U]
  def update[U >: T](t: U, i: Int): CList[U]
  def suffixes: CList[CList[T]]
  def toList: List[T]
}

case object Empty extends CList[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException()
  def tail: Nothing = throw new NoSuchElementException()
  def ++[U](ys: CList[U]): CList[U] = ys
  def update[U](t: U, i: Int) = throw new IndexOutOfBoundsException()
  def suffixes = Cons(Empty, Empty)
  def toList = Nil

  override def toString = "Nil"
}

case class Cons[+T](hd: T, tl: CList[T]) extends CList[T] {
  def isEmpty: Boolean = false
  def head: T = hd
  def tail: CList[T] = tl
  def ++[U >: T](ys: CList[U]): CList[U] = Cons(head, tail ++ ys)
  def update[U >: T](t: U, i: Int): CList[U] = if (i == 0) Cons(t, this) else Cons(head, tail.update(t, i-1))

  def suffixes: CList[CList[T]] = Cons(this, tail.suffixes)

  def toList = head :: tail.toList
  override def toString = head + " :: " + tail.toString
}