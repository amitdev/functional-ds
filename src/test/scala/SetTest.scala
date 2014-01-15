import ds.{Orderable, BST}
import org.junit.runner.RunWith
import org.scalacheck.Arbitrary._
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary

@RunWith(classOf[JUnitRunner])
class SetTest extends FunSuite with Checkers {

  implicit def arbBST[T : Orderable](implicit a: Arbitrary[T]) = Arbitrary {
    for {
      v <- Arbitrary.arbitrary[List[T]]
    } yield BST(v:_*)
  }

  def isSorted(l: List[Int]) = l.isEmpty || l.zip(l.tail).forall(x => x._1 <= x._2)

  test("Set Insert") {
    check((s: BST[Int], x: Int) => s.insert(x).member(x))
  }

  test("Set delete") {
    check((s: BST[Int], x: Int) => !s.delete(x).member(x))
  }

  test("Set Inorder") {
    check((s: BST[Int]) => isSorted(s.toList))
  }
}