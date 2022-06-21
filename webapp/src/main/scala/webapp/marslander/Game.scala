package webapp.marslander

import webapp.simulator.Simulator.EvaluationResult

object Game {

  sealed trait GameState {}

  object GameState {
    case object Paused extends GameState

    case object Running                                    extends GameState
    case class Stopped(evaluationResult: EvaluationResult) extends GameState
  }
  case class GameSettings(fps: Int)

}
