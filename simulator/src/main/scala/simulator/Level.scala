package webapp.marslander

import io.circe.{Decoder, Encoder}
import webapp.vectory.Line

//"lander_initial_state": {
//  "x": 2500,
//  "y": 2700,
//  "hSpeed": 0,
//  "vSpeed": 0,
//  "fuel": 550,
//  "rotate": 0,
//  "power": 0
//  }

case class Level(name: String, initialState: Surface, landerInitialState: LanderInitialState) {
  def toInputLines: List[String] =
    initialState.surfaceN.toString :: initialState.surfaceCoords.map { case Coord(x, y) => s"$x $y" }

  val surfaceLines: List[Line] = initialState.surfaceCoords
    .sliding(2, 1)
    .toList
    .map { case List(first, second) =>
      Line(first.x, first.y, second.x, second.y)
    }

  val landingArea: Line = surfaceLines.find { l =>
    l.end.y == l.start.y && math.abs(l.end.x - l.start.x) >= 1000
  }.get

}
case class LanderInitialState(x: Int, y: Int, hSpeed: Int, vSpeed: Int, fuel: Int, rotate: Int, power: Int)
case class Surface(surfaceN: Int, surfaceCoords: List[Coord])
case class Coord(x: Int, y: Int)

object Codecs {
  implicit val landerInitialStateDecoder: Decoder[LanderInitialState] = io.circe.generic.semiauto.deriveDecoder
  implicit val coordDecoder: Decoder[Coord]                           = io.circe.generic.semiauto.deriveDecoder
  implicit val initialStateDecoder: Decoder[Surface]                  = io.circe.generic.semiauto.deriveDecoder
  implicit val surfaceModelDecoder: Decoder[Level]                    = io.circe.generic.semiauto.deriveDecoder

  implicit val landerInitialStateEncoder: Encoder[LanderInitialState] = io.circe.generic.semiauto.deriveEncoder
  implicit val coordEncoder: Encoder[Coord]                           = io.circe.generic.semiauto.deriveEncoder
  implicit val initialStateEncoder: Encoder[Surface]                  = io.circe.generic.semiauto.deriveEncoder
  implicit val surfaceModelEncoder: Encoder[Level]                    = io.circe.generic.semiauto.deriveEncoder
}
//
//class Server(initialState: InitialState, stdInQueue: Queue[IO, String], stdOutQueue: Queue[IO, String]) {
//
//  var initialDumpDone = false
//
//  def
//
//}
