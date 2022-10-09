package webapp.graphics

import simulator.Simulator.PreciseState
import webapp.marslander.Coord
import webapp.vectory.Vec2

object Helper {

  def toDisplayCoord(coord: Coord): Coord =
    Coord(coord.x, 3000 - coord.y)


  def toCoord(v: Vec2): Coord =
    Coord(PreciseState.myRound(v.x), PreciseState.myRound(v.y))


}
