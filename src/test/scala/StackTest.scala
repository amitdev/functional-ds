import ds.{Stack, CList, Empty}
import org.junit.runner.RunWith
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import org.scalacheck.Gen.oneOf
import org.scalacheck.Arbitrary

@RunWith(classOf[JUnitRunner])
class StackTest extends FunSuite with Checkers {

  implicit def arbStack[T](implicit a: Arbitrary[T]): Arbitrary[CList[T]] = Arbitrary {
    def genStack: Gen[CList[T]] = oneOf(Empty, for {
      v <- arbitrary[T]
      s <- genStack
    } yield s.cons(v))
    genStack
  }

  def size[T](s: Stack[T]): Int = if (s.isEmpty) 0 else 1 + size(s.tail)

  test("Empty Stack") {
    assert(Empty.isEmpty, "Empty stack should be empty")
  }

  test("Stack head") {
    check((s: CList[Int], x: Int) => s.cons(x).head == x)
  }

  test("Stack tail") {
    check((s: CList[Int], x: Int) => s.cons(x).tail == s)
  }

  test("Stack append") {
    check((xs: CList[Int], ys: CList[Int]) => size(xs ++ ys) == size(xs) + size(ys))
  }

  test("Stack update") {
    check((xs: CList[Int], x: Int, i: Int) => {
      if (i >= 0 && i < size(xs)) size(xs.update(x, i)) == size(xs)+1
      else try {
        xs.update(x, i)
        false
      } catch {
        case _:IndexOutOfBoundsException => true
      }
    })
  }

  test("Stack suffixes") {
    check((xs: CList[Int]) => {
      val suffixes = xs.suffixes
      size(suffixes) == size(xs) + 1
    })
  }
}
