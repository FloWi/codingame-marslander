package webapp.marslander

import webapp.simulator.Simulator.PreciseState

import scala.collection.immutable.Queue

object Game {

  sealed trait GameState {
    def state: PreciseState
  }

  object GameState {
    case class Paused(state: PreciseState, previous: Queue[PreciseState]) extends GameState

    case class Running(state: PreciseState, previous: Queue[PreciseState]) extends GameState

    case class Crashed(state: PreciseState, previous: Queue[PreciseState]) extends GameState

    case class Initial(state: PreciseState, previous: Queue[PreciseState]) extends GameState
  }
  case class GameSettings(fps: Int)

}
