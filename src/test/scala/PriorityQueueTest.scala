import ds.PriorityQueue
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class PriorityQueueTest extends FunSuite with Checkers {

  test("empty") {
    assert(PriorityQueue().isEmpty, "Empty queue is really empty")
  }

  test("Insert") {
    val inputs = Map(List(1) -> 1,
                     List(2,1,4,5) -> 1,
                     List(4,5,6,7,8,9) -> 4)
    for ((k,v) <- inputs)
      assert(PriorityQueue(k:_*).findMin == v, "Insert element works")
  }

  test("Delete") {
    val inputs = Map(List(1) -> 1,
      List(2,1,4,5) -> 1,
      List(4,5,6,7,8,9) -> 4)
    for ((k,v) <- inputs) {
      var pq = PriorityQueue(k:_*)
      val s = k.sorted
      for (i <- 1 to k.length) {
        val sub = s.slice(i, k.length)
        if (!sub.isEmpty) {
          pq = pq.deleteMin()
          assert(pq.findMin == sub(0), "Delete element works")
        }
      }
    }
  }
}
