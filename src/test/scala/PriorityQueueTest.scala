import ds.PriorityQueue
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class PriorityQueueTest extends FunSuite with Checkers {

  val inputs = Map(List(1) -> 1,
    List(2,1,4,5) -> 1,
    List(4,5,6,7,8,9) -> 4)

  test("empty") {
    val empty = PriorityQueue(3).deleteMin()
    assert(empty.isEmpty, "Empty queue is really empty")
    intercept[UnsupportedOperationException] {
      empty.findMin
    }
    intercept[UnsupportedOperationException] {
      empty.deleteMin()
    }
    assert(empty.size == 0)
  }

  test("Insert") {
    for ((k,v) <- inputs) {
      val q = PriorityQueue(k: _*)
      assert(q.findMin == v, "Insert element works")
      assert(q.size == k.length)
    }
  }

  test("Delete") {
    for ((k,v) <- inputs) {
      var pq = PriorityQueue(k:_*)
      val s = k.sorted
      for (i <- 1 to k.length) {
        val sub = s.slice(i, k.length)
        if (!sub.isEmpty) {
          pq = pq.deleteMin()
          assert(pq.size == sub.length)
          assert(pq.findMin == sub(0), "Delete element works")
        }
      }
    }
  }

  test("Common Operations") {
    for ((k,v) <- inputs) {
      val q = PriorityQueue(k: _*)
      //Test Iteration
      for (i <- q) assert(k.contains(i))
      //Test map, reduce etc
      assert((q map (_*10) reduce (_+_)) == (k map (_*10) reduce (_+_)))
      assert(q.map(_*10).isInstanceOf[PriorityQueue[Int]])
    }
  }
}