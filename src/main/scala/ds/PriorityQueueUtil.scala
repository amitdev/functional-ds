package ds

import scala.xml.NodeSeq

object PriorityQueueUtil {

  case class PosNode[A](data: A, x: Int, y: Int, children: List[PosNode[A]] = Nil)

  def nodeToPos[A](node: Node[A], x: Int, y: Int = 30) : (PosNode[A], Int) =
    if (node.rank == 0) (PosNode(node.data, x,y), x)
    else {
      val res = node.children.reverse.zipWithIndex.map(t => nodeToPos(t._1, x + t._2 * 50, y + 50))
      val mx = Math.max(res.maxBy(_._2)._2, x)
      val lst = PosNode(node.data, x,y, res.map(_._1))
      (lst, mx)
    }

  def toPos[A](bq : List[Node[A]], startX: Int) : List[PosNode[A]] = bq match {
    case Nil => Nil
    case x :: xs =>
      val (pnode, nextPos) = nodeToPos(x, startX)
      pnode :: toPos(xs, nextPos+50)
  }

  def toPos[A](bq : BinomialQueue[A], startX: Int = 30) : List[PosNode[A]] = toPos(bq.nodes, startX)

  def draw[A](pos: PosNode[A]) : NodeSeq = {
    val cs = pos.children map { c : PosNode[A] =>
      <line id="svg_1" y2={c.y.toString} x2={c.x.toString} y1={pos.y.toString} x1={pos.x.toString} stroke-width="3" stroke="#000000" fill="none"/> ++
      draw(c)
    }
    val ns = cs.flatten
     ns ++
     <ellipse stroke={"#000000"} fill={"#000000"} cy={pos.y.toString} cx={pos.x.toString} id={"svg"+pos.x.toString+pos.y.toString} ry="10" rx="10" stroke-width="5"/> ++
     <text xml:space="preserve" text-anchor="middle" font-family="serif" font-size="14" id="svgt" y={(pos.y+7).toString} x={pos.x.toString} stroke-width="0" stroke="#ffffff" fill="#ffffff">{pos.data}</text>
  }

  def toSvg[A](lst: List[PosNode[A]], width: Int = 640, height: Int = 480) = {
    <svg width={width.toString} height={height.toString} xmlns="http://www.w3.org/2000/svg">
      <g>
        <title>PQ</title>
        {(lst map draw).flatten}
      </g>
    </svg>
  }
}
