package webapp.marslander

object Game {

  sealed trait GameState {}

  object GameState {
    case object Paused extends GameState

    case object Running extends GameState

    case object Crashed extends GameState

    case object Initial extends GameState
  }
  case class GameSettings(fps: Int)

}
