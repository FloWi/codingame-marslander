package webapp.marslander

import cats.effect.IO
import cats.effect.std.Queue
import io.circe.{Decoder, Encoder}

case class SurfaceModel(name: String, initialState: InitialState) {
  def toInputLines: List[String] =
    initialState.surfaceN.toString :: initialState.surfaceCoords.map { case Coord(x, y) => s"$x $y" }
}
case class InitialState(surfaceN: Int, surfaceCoords: List[Coord])
case class Coord(x: Int, y: Int)

object Codecs {
  implicit val coordDecoder: Decoder[Coord]               = io.circe.generic.semiauto.deriveDecoder
  implicit val initialStateDecoder: Decoder[InitialState] = io.circe.generic.semiauto.deriveDecoder
  implicit val surfaceModelDecoder: Decoder[SurfaceModel] = io.circe.generic.semiauto.deriveDecoder

  implicit val coordEncoder: Encoder[Coord]               = io.circe.generic.semiauto.deriveEncoder
  implicit val initialStateEncoder: Encoder[InitialState] = io.circe.generic.semiauto.deriveEncoder
  implicit val surfaceModelEncoder: Encoder[SurfaceModel] = io.circe.generic.semiauto.deriveEncoder
}
//
//class Server(initialState: InitialState, stdInQueue: Queue[IO, String], stdOutQueue: Queue[IO, String]) {
//
//  var initialDumpDone = false
//
//  def
//
//}
