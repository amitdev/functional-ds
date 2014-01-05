import ds.BST
import org.junit.runner.RunWith
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import org.scalacheck.Gen.oneOf
import org.scalacheck.Arbitrary

@RunWith(classOf[JUnitRunner])
class SetTest extends FunSuite with Checkers {

  implicit def arbSet[T <% Ordered[T]](implicit a: Arbitrary[T]): Arbitrary[BST[T]] = Arbitrary {
    def genNode: Gen[BST[T]] = oneOf(BST.Leaf, for {
      v <- arbitrary[T]
      l <- genNode
      r <- genNode
    } yield BST.Node(v, l, r))
    genNode
  }

  test("Set Insert") {
    check((s: BST[Int], x: Int) => s.insert(x).member(x))
  }

}
