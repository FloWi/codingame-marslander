package simulator

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonNumber, JsonObject}
import simulator.Simulator.EvaluationResult.{AllClear, Crashed, Landed, OffLimits}
import webapp.marslander.Level
import webapp.vectory.{Algorithms, Line, Vec2}

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

      def isLanded: Boolean = {
        val isAbove                  = x <= level.landingArea.end.x && x >= level.landingArea.start.x
        val isAtHeight               = y <= level.landingArea.start.y + Constants.maxLandingVerticalSpeed
        val isSlowEnoughHorizontally = math.abs(hSpeed) <= Constants.maxLandingHorizontalSpeed
        val isSlowEnoughVertically   = math.abs(vSpeed) <= Constants.maxLandingVerticalSpeed
        val isCorrectRotation        = rotate == 0
        isAbove && isAtHeight && isSlowEnoughHorizontally && isSlowEnoughVertically && isCorrectRotation
      }

      val isOffLimits = y < 0 || y > Constants.gameHeight || x < 0 || x > Constants.gameWidth
      val isCrashed   = lastOption.exists(intersectsWithSurface)
      val isOutOfFuel = fuel <= 0

      val radar        = calcShipRadar(this, level)
      val landingRadar = calcLandingAreaAccess(this, level)

      val isAboveLandingArea            = x >= level.landingArea.start.x && x <= level.landingArea.end.x
      val horizontalDistanceLandingArea =
        if (isAboveLandingArea) 0.0
        else {
          val ship    = Vec2(x, y)
          val toStart = level.landingArea.start - ship
          val toEnd   = level.landingArea.end - ship

          List(toStart, toEnd).minBy(_.length).x
        }
      val verticalDistanceLandingArea   = y - level.landingArea.start.y

      val enrichedState = EnrichedState(
        x = x,
        y = y,
        rotation = rotate,
        power = power,
        fuel = fuel,
        hSpeed = hSpeed,
        vSpeed = vSpeed,
        landingRadarDistances = landingRadar,
        horizontalDistanceLandingArea = horizontalDistanceLandingArea,
        verticalDistanceLandingArea = verticalDistanceLandingArea,
        distanceLandingArea = math.sqrt(
          horizontalDistanceLandingArea * horizontalDistanceLandingArea + verticalDistanceLandingArea * verticalDistanceLandingArea,
        ),
        isCrashed = isCrashed,
        isLanded = isLanded,
        isOffLimits = isOffLimits,
        isOutOfFuel = isOutOfFuel,
        radarDistances = radar.map { ray =>
          RadarRay(ray.angleDeg, ray.maybeClosestCollisionPointAndDistance.map(_._2))
        },
      )

      if (isOffLimits)
        OffLimits(enrichedState)
      else {
        if (isCrashed) {
          Crashed(enrichedState)
        }
        else if (isLanded) {
          Landed(enrichedState)
        }
        else AllClear(enrichedState)
      }
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

  case class EnrichedState(
    x: Double,
    y: Double,
    rotation: Int,
    power: Int,
    fuel: Int,
    hSpeed: Double,
    vSpeed: Double,
    landingRadarDistances: List[LandingRadarRay],
    horizontalDistanceLandingArea: Double,
    verticalDistanceLandingArea: Double,
    distanceLandingArea: Double,
    isCrashed: Boolean,
    isLanded: Boolean,
    isOffLimits: Boolean,
    isOutOfFuel: Boolean,
    radarDistances: List[RadarRay],
  ) {
    val isBad: Boolean   = isCrashed || isOffLimits || isOutOfFuel
    val isGoing: Boolean = !isLanded && !isBad

    def flattened: JsonObject = {
      val map = Map(
        "x" -> x.asJson,
        "y" -> y.asJson,
        "rotation" -> rotation.asJson,
        "power" -> power.asJson,
        "fuel" -> fuel.asJson,
        "hSpeed" -> hSpeed.asJson,
        "vSpeed" -> vSpeed.asJson,
        "horizontalDistanceLandingArea" -> horizontalDistanceLandingArea.asJson,
        "verticalDistanceLandingArea" -> verticalDistanceLandingArea.asJson,
        "distanceLandingArea" -> distanceLandingArea.asJson,
        "isCrashed" -> isCrashed.asJson,
        "isLanded" -> isLanded.asJson,
        "isOffLimits" -> isOffLimits.asJson,
        "isOutOfFuel" -> isOutOfFuel.asJson,
      )

      val landingRadars = landingRadarDistances.flatMap { lr =>
        val percent = (lr.percentX * 100).toInt
        List(
          s"landing-distance-$percent" -> lr.distance.asJson,
          s"landing-path-free-$percent" -> lr.maybeNearestCollision.isEmpty.asJson,
        ).toMap
      }.toMap
      val radars = radarDistances.flatMap { r =>
        List(
          s"radar-maybe-intersection-distance-${r.angleDeg}" -> r.maybeIntersectionDistance.asJson,
        ).toMap
      }.toMap

      JsonObject.fromMap(map ++ landingRadars ++ radars)

    }


  }

  case class RadarRay(angleDeg: Int, maybeIntersectionDistance: Option[Double])

  object RadarRay {
    implicit val decoder: Decoder[RadarRay] = io.circe.generic.semiauto.deriveDecoder
    implicit val encoder: Encoder[RadarRay] = io.circe.generic.semiauto.deriveEncoder
  }

  case class LandingRadarRay(
    percentX: Double,
    landingAreaLocation: Vec2,
    distance: Double,
    maybeNearestCollision: Option[Vec2],
  )

  object LandingRadarRay {

    implicit val vec2Decoder: Decoder[Vec2] = io.circe.generic.semiauto.deriveDecoder
    implicit val vec2Encoder: Encoder[Vec2] = io.circe.generic.semiauto.deriveEncoder

    implicit val decoder: Decoder[LandingRadarRay] = io.circe.generic.semiauto.deriveDecoder
    implicit val encoder: Encoder[LandingRadarRay] = io.circe.generic.semiauto.deriveEncoder
  }

  object EnrichedState {
    implicit val enrichedStateDecoder: Decoder[EnrichedState] = io.circe.generic.semiauto.deriveDecoder
    implicit val enrichedStateEncoder: Encoder[EnrichedState] = io.circe.generic.semiauto.deriveEncoder

  }

  sealed trait EvaluationResult {
    def enrichedState: EnrichedState
  }
  object EvaluationResult       {
    case class AllClear(enrichedState: EnrichedState)  extends EvaluationResult
    case class Crashed(enrichedState: EnrichedState)   extends EvaluationResult
    case class OffLimits(enrichedState: EnrichedState) extends EvaluationResult
    case class Landed(enrichedState: EnrichedState)    extends EvaluationResult
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

  case class SimulationStepInput(level: Level, state: PreciseState, command: GameCommand)

  def runUntilCrashed(input: SimulationStepInput): List[PreciseState] =
    LazyList
      .from(0)
      .scanLeft((Option.empty[PreciseState], input.state)) { case ((old, current), _) =>
        val newState = simulate(input.copy(state = current))
        (Some(current), newState)
      }
      .map { case (old, current) => (old, current, current.evaluate(input.level, old)) }
      .takeWhile { s =>
        val result = s._3.enrichedState
//        println("===================== runUntilCrashed =====================")
//        println(s"old: ${s._1.map(old => (old.x, old.y))}, new: ${(s._2.x, s._2.y)}")
//        println(s._3)
        result.isGoing
      }
      .map(_._2)
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

  def calcSuicideBurnHeight(preciseState: PreciseState, level: Level): Double = {
    val d = calcSuicideBurnDistance(preciseState.vSpeed)
    val h = level.landingArea.y1 + d

//    println(s"level.landingArea.y1 = ${level.landingArea.y1}")
//    println(s"preciseState.vSpeed = ${preciseState.vSpeed}")
//    println(s"suicide d = $d")
//    println(s"suicide h = $h")
    h
  }

  def calcSuicideBurnDistance(vVertical: Double): Double = {
    /*
    Distance to ground:
    1000m

                vSpeed: -50
            max vSpeed: -40
          diff to kill: -10
    max thrust upwards: 4
               gravity: -3,711

    max verticalAcc: 4-3,711 = 0,289

    v = v0 + a*t
    t = (v-v0)/a
    v = -40 (we'd use 0 if we want to slow down completely - we use the maximum allowed vSpeed)

    t = (-40 - -50) / 0,289
    t = 34,6s


    Lastly we want to know how far we will go in that time. Under constant acceleration
    dist = v_avg * t
    v_avg = (v0 + v) / 2

    dist = ((v0 + v) / 2) * ((v-v0)/a)

    dist = ((-40 + -50) / 2) * ((-50 - -40) / 0,289)
    dist = 1557,1m
     */

    val v0 = -Constants.maxLandingVerticalSpeed.toDouble
    val v  = vVertical
    val a  = -(Constants.maxThrust - Constants.g)

    ((v0 + v) / 2) * ((v - v0) / a)
  }

  def score(level: Level, state: PreciseState): Int =
    state.fuel

  def calcShipRadar(landerState: PreciseState, level: Level): List[ShipRay] = {

    val ship = Vec2(landerState.x, landerState.y)

    // calculating the lines between ship and every corner of the map and pick the longest.
    // no radar ray needs to be longer
    val longestRayLength = List(
      Vec2(0, 0),
      Vec2(Constants.gameWidth, 0),
      Vec2(0, Constants.gameHeight),
      Vec2(Constants.gameWidth, Constants.gameHeight),
    ).map(corner => Line(ship, corner))
      .maxBy(_.length)

    // create rays in 5° distance
    val rays = 0.until(end = 360, step = 5).map { angleDeg =>
      val radarRay = Line.apply(ship, ship + Vec2.unit(angleDeg.toRadians) * longestRayLength.length)

      val collisions = level.surfaceLines.flatMap { l =>
        l.intersect(radarRay) match {
          case Some(i) if i.onLine1 && i.onLine2 => List(i)
          case _                                 => List.empty
        }
      }

      ShipRay(ship, radarRay, angleDeg - 90, collisions)

    }
    rays.toList
  }

  def calcLandingAreaAccess(landerState: PreciseState, level: Level): List[LandingRadarRay] = {

    val start = level.landingArea.start + Vec2(0.001, 0)
    val end   = level.landingArea.end - Vec2(0.001, 0)

    val ship = Vec2(landerState.x, landerState.y)

    val rays = 0
      .to(end = 100, step = 25)
      .toList
      .map(_ / 100.0)
      .map { relativeAmount =>
        start + ((end - start) * relativeAmount) -> relativeAmount
      }
      .map { case (landingLocation, percentX) =>
        val radarRay              = Line(ship, landingLocation)
        val maybeNearestCollision = level.surfaceLines
          .diff(List(level.landingArea))
          .flatMap { l =>
            l.intersect(radarRay) match {
              case Some(i) if i.onLine1 && i.onLine2 => List(i)
              case _                                 => List.empty
            }
          }
          .minByOption { i =>
            val collisionVec = ship - i.pos
            collisionVec.length
          }
          .map(_.pos)

        LandingRadarRay(percentX, landingLocation, radarRay.length, maybeNearestCollision)
      }

    rays
  }

  case class ShipRay(ship: Vec2, ray: Line, angleDeg: Int, intersections: List[Algorithms.LineIntersection]) {
    def maybeClosestCollisionPointAndDistance: Option[(Algorithms.LineIntersection, Double)] = intersections.map {
      intersection =>
        val distance = ship.-(intersection.pos).abs.length
        intersection -> distance
    }.minByOption(_._2)
  }

  case class FlightRecoder(score: Int, commands: List[GameCommand])

  object FlightRecoder {
    implicit val landerInitialStateDecoder: Decoder[FlightRecoder] = io.circe.generic.semiauto.deriveDecoder
    implicit val landerInitialStateEncoder: Encoder[FlightRecoder] = io.circe.generic.semiauto.deriveEncoder
  }

}
