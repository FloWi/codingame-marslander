package webapp.vectory

//TODO: use Fast Inverse Square Root where possible
// for example Vec2.normalized
// https://en.wikipedia.org/wiki/Fast_inverse_square_root

object Algorithms {
  def polygonCornersToEdges(corners: IndexedSeq[Vec2]): IndexedSeq[Line] = {
    val n     = corners.size
    val edges = new Array[Line](n)
    var i     = 0
    var last  = corners(n - 1)
    while (i < n) {
      val current = corners(i)
      edges(i) = Line(last, current)
      last = current
      i += 1
    }
    edges
  }

  def distancePointLine(x0: Double, y0: Double, x1: Double, y1: Double, x2: Double, y2: Double): Double =
    // Point: x0, y0
    // Line: x1, y1 --- x2, y2
    Math.abs((y2 - y1) * x0 - (x2 - x1) * y0 + x2 * y1 - y2 * x1) / Math.sqrt(
      (y2 - y1) * (y2 - y1) + (x2 - x1) * (x2 - x1),
    )

  def distancePointLineSegment(x0: Double, y0: Double, x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    import Math.{max, min}
    // https://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
    // Return minimum distance between line segment vw and point p
    val p  = Vec2(x0, y0)
    val v  = Vec2(x1, y1)
    val w  = Vec2(x2, y2)
    val l2 = Line(v, w).lengthSq // i.e. |w-v|^2 -  avoid a sqrt
    if (l2 == 0.0) return Line(p, v).length // v == w case
    // Consider the line extending the segment, parameterized as v + t (w - v).
    // We find projection of point p onto the line.
    // It falls where t = [(p-v) . (w-v)] / |w-v|^2
    // We clamp t from [0,1] to handle points outside the segment vw.
    val t          = max(0, min(1, ((p - v) dot (w - v)) / l2))
    val projection = v + (w - v) * t // Projection falls on the segment
    return Line(p, projection).length
  }

  def projectPointOnLine(x0: Double, y0: Double, x1: Double, y1: Double, x2: Double, y2: Double): Vec2 = {
    val m = (y2 - y1) / (x2 - x1)
    val b = y1 - m * x1
    val x = (m * y0 + x0 - m * b) / (m * m + 1)
    val y = (m * m * y0 + m * x0 + b) / (m * m + 1)
    Vec2(x, y)
  }

  final case class LineIntersection(pos: Vec2, onLine1: Boolean, onLine2: Boolean)
  def intersect(line1: Line, line2: Line): Option[LineIntersection] = {
    // if the lines intersect, the result contains the x and y of the intersection
    // (treating the lines as infinite) and booleans for
    // whether line segment 1 or line segment 2 contain the point
    // from: http://jsfiddle.net/justin_c_rounds/Gd2S2
    // ported to scala

    val line1Dx = line1.end.x - line1.start.x
    val line1Dy = line1.end.y - line1.start.y
    val line2Dx = line2.end.x - line2.start.x
    val line2Dy = line2.end.y - line2.start.y

    val denominator = (line2Dy * line1Dx) - (line2Dx * line1Dy)

    if (denominator == 0) return None // lines are parallel

    val startDx = line1.start.x - line2.start.x
    val startDy = line1.start.y - line2.start.y

    val numerator1 = (line2Dx * startDy) - (line2Dy * startDx)
    val numerator2 = (line1Dx * startDy) - (line1Dy * startDx)
    val a          = numerator1 / denominator
    val b          = numerator2 / denominator

    // if we cast these lines infinitely in both directions, they intersect here:
    val resultX       = line1.start.x + (a * line1Dx)
    val resultY       = line1.start.y + (a * line1Dy)
    /*
    // it is worth noting that this should be the same as:
    x = line2StartX + (b * (line2EndX - line2StartX))
    y = line2StartX + (b * (line2EndY - line2StartY))
     */
    // if line1 is a segment and line2 is infinite, they intersect if:
    val resultOnLine1 = a > 0 && a < 1
    // if line2 is a segment and line1 is infinite, they intersect if:
    val resultOnLine2 = b > 0 && b < 1
    // if line1 and line2 are segments, they intersect if both of the above are true
    return Some(LineIntersection(Vec2(resultX, resultY), resultOnLine1, resultOnLine2))
  }

}
