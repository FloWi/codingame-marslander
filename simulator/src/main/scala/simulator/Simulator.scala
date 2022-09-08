package simulator

import io.circe.{Decoder, Encoder}
import simulator.Simulator.EvaluationResult.{AllClear, Crashed, Landed, OffLimits}
import webapp.marslander.Level
import webapp.vectory.{Line, Vec2}

object Simulator {

  case class State(x: Int, y: Int, hSpeed: Int, vSpeed: Int, fuel: Int, rotate: Int, power: Int)                    {
    override def toString: String =
      s"(x:$x, y:$y, hSpeed:$hSpeed, vSpeed:$vSpeed, fuel:$fuel, rotate:$rotate, power:$power)"
  }
  case class PreciseState(x: Double, y: Double, hSpeed: Double, vSpeed: Double, fuel: Int, rotate: Int, power: Int) {
    def evaluate(level: Level, lastOption: Option[PreciseState]): EvaluationResult = {

      def intersectsWithSurface(previous: PreciseState): Boolean = {
        val now              = Vec2(x, y)
        val prev             = Vec2(previous.x, previous.y)
        val connectingVector = Line(prev, now)
        level.surfaceLines.exists { surfaceLine =>
          surfaceLine.intersect(connectingVector).exists(intersection => intersection.onLine1 && intersection.onLine2)
        }
      }

      def hasLanded: Boolean = {
        val isAbove                  = x <= level.landingArea.end.x && x >= level.landingArea.start.x
        val isAtHeight               = y <= level.landingArea.start.y + Constants.maxLandingVerticalSpeed
        val isSlowEnoughHorizontally = math.abs(hSpeed) <= Constants.maxLandingHorizontalSpeed
        val isSlowEnoughVertically   = math.abs(vSpeed) <= Constants.maxLandingVerticalSpeed
        val isCorrectRotation        = rotate == 0
        isAbove && isAtHeight && isSlowEnoughHorizontally && isSlowEnoughVertically && isCorrectRotation
      }

      if (y < 0 || y > Constants.gameHeight || x < 0 || x > Constants.gameWidth)
        OffLimits
      else if (lastOption.exists(intersectsWithSurface)) {
        Crashed
      }
      else if (hasLanded) {
        Landed
      }
      else AllClear
    }
    //    override def toString: String =
//      s"""     x:$x
//         |     y:$y
//         |hSpeed:$hSpeed
//         |vSpeed:$vSpeed
//         |  fuel:$fuel
//         |rotate:$rotate
//         | power:$power""".stripMargin
  }

  sealed trait EvaluationResult
  object EvaluationResult {
    case object AllClear  extends EvaluationResult
    case object Crashed   extends EvaluationResult
    case object OffLimits extends EvaluationResult
    case object Landed    extends EvaluationResult
  }

  object PreciseState {
    def toRoundedState(state: PreciseState): State = {
      val PreciseState(x, y, hSpeed, vSpeed, fuel, rotate, power) = state

      State(myRound(x), myRound(y), myRound(hSpeed), myRound(vSpeed), fuel, rotate, power)
    }

    def fromRoundedState(state: State): PreciseState = {
      val State(x, y, hSpeed, vSpeed, fuel, rotate, power) = state

      PreciseState(x, y, hSpeed, vSpeed, fuel, rotate, power)
    }

    def myRound(value: Double): Int = {
      val sign = value.sign.toInt
      math.abs(value).round.toInt * sign
    }
  }

  case class GameCommand(rotation: Int, power: Int)
  case class GameCommandDelta(rotation: Int, power: Int)

  object GameCommand {
    def parse(str: String): GameCommand =
      str.split(" ") match { case Array(rotation, thrust) => GameCommand(rotation.toInt, thrust.toInt) }

    implicit val gameCommandDecoder: Decoder[GameCommand] = io.circe.generic.semiauto.deriveDecoder
    implicit val gameCommandEncoder: Encoder[GameCommand] = io.circe.generic.semiauto.deriveEncoder
  }

  object Constants {
    val g                         = 3.711
    val minAngle                  = -90
    val maxAngle                  = 90
    val minThrust                 = 0
    val maxThrust                 = 4
    val maxLandingVerticalSpeed   = 40
    val maxLandingHorizontalSpeed = 20
    val gameWidth                 = 7000
    val gameHeight                = 3000
    val maxThrustTurnDelta        = 1
    val maxRotationTurnDelta      = 15
  }

  case class SimulationStepInput(initialState: Level, state: PreciseState, command: GameCommand)

  def runUntilCrashed(input: SimulationStepInput): List[PreciseState] =
    LazyList
      .from(0)
      .scanLeft(input.state) { (current, _) =>
        simulate(input.copy(state = current))
      }
      .takeWhile { s =>
        s.fuel >= 0 && s.x >= 0 && s.x <= Constants.gameWidth &&
        s.y >= 0 && s.y <= Constants.gameHeight
      }
      .toList
  def simulate(input: SimulationStepInput): PreciseState              = {

    val velocity = Vec2(input.state.hSpeed, input.state.vSpeed)
    val gravity  = Vec2.unitY.*(-Constants.g)
    val position = Vec2(input.state.x, input.state.y)

    val newRotation = calcNewRotation(input.state.rotate, input.command.rotation)
    val newThrust   = calcNewThrust(input.state.power, input.command.power)

    val angle = math.toRadians(newRotation + 90)

    val newAcceleration     = Vec2.unit(angle) * newThrust
    val overallAcceleration = newAcceleration + gravity
    val newVelocity         = velocity + overallAcceleration
    val avgVelocity         = (velocity + newVelocity) / 2
    val newPosition         = position + avgVelocity

    PreciseState(
      newPosition.x,
      newPosition.y,
      newVelocity.x,
      newVelocity.y,
      input.state.fuel - newThrust,
      newRotation,
      newThrust,
    )
  }

  def runFlightRecorder(level: Level, flightRecoder: FlightRecoder): List[PreciseState] = {
    val initialState = PreciseState(
      x = level.landerInitialState.x,
      y = level.landerInitialState.y,
      rotate = level.landerInitialState.rotate,
      hSpeed = level.landerInitialState.hSpeed,
      vSpeed = level.landerInitialState.vSpeed,
      power = level.landerInitialState.power,
      fuel = level.landerInitialState.fuel,
    )

    val result =
      flightRecoder.commands.scanLeft(initialState)((state, gc) => simulate(SimulationStepInput(level, state, gc)))
    assert(result.last.fuel == flightRecoder.score)
    result
  }

  def calcNewRotation(oldRotation: Int, requestedRotation: Int): Int = {
    // -90 --> 90

    // example 1:
    // old:  -45°
    // req:   30°
    // diff: +75°
    // granted: +15°
    // new:   -30°

    val normalizedOld       = oldRotation + math.abs(Constants.minAngle)
    val normalizedRequested = requestedRotation + math.abs(Constants.minAngle)

    val diff           = normalizedRequested - normalizedOld
    val absGrantedDiff =
      if (math.abs(diff) > Constants.maxRotationTurnDelta) Constants.maxRotationTurnDelta else math.abs(diff)
    val grantedDiff    = if (diff > 0) absGrantedDiff else -absGrantedDiff

    val normalizedNew = normalizedOld + grantedDiff
    val newRotation   = normalizedNew - math.abs(Constants.minAngle)

    val result =
      if (newRotation < Constants.minAngle) Constants.minAngle
      else if (newRotation > Constants.maxAngle) Constants.maxAngle
      else newRotation

    result
  }

  def calcNewThrust(oldPower: Int, newPower: Int): Int = {
    val diff = newPower - oldPower

    val grantedDiff =
      if (diff == 0) 0
      else if (diff > 0) Constants.maxThrustTurnDelta
      else -Constants.maxThrustTurnDelta

    val newThrust = oldPower + grantedDiff

    if (newThrust < Constants.minThrust) Constants.minThrust
    else if (newThrust > Constants.maxThrust) Constants.maxThrust
    else newThrust

  }

  def score(level: Level, state: PreciseState): Int =
    state.fuel

  case class FlightRecoder(score: Int, commands: List[GameCommand])

  object FlightRecoder {
    implicit val landerInitialStateDecoder: Decoder[FlightRecoder] = io.circe.generic.semiauto.deriveDecoder
    implicit val landerInitialStateEncoder: Encoder[FlightRecoder] = io.circe.generic.semiauto.deriveEncoder
  }

}
