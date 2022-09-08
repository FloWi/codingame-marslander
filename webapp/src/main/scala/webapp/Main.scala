package webapp

import cats.effect.{IO, SyncIO}
import colibri.{BehaviorSubject, Observable, Subject}
import com.raquo.domtypes.jsdom.defs.events.TypedTargetMouseEvent
import org.scalajs.dom.Element
import outwatch._
import outwatch.dsl._
import webapp.graphics.Rocket
import webapp.marslander.Game.{GameSettings, GameState}
import webapp.marslander.{Coord, Level}
import simulator.Simulator
import simulator.Simulator.PreciseState.toRoundedState
import simulator.Simulator._
import webapp.vectory.{Algorithms, Line, Vec2}

import scala.collection.immutable.Queue

// Outwatch documentation:
// https://outwatch.github.io/docs/readme.html

object Main {

  def main(args: Array[String]): Unit =
    Outwatch.renderInto[SyncIO]("#app", app).unsafeRunSync()
//    Outwatch.renderInto[SyncIO]("#app", svgPlayground).unsafeRunSync()

  def app =
    div(
      Communication.getLevels.map(renderUi),
    )

  def svgPlayground = {
    import svg._
    val activeRocket = Subject.behavior(0)

    val location = Vec2(500, 500)

    div(
      button("+", onClick(activeRocket.map(_ + 1)) --> activeRocket),
      button("-", onClick(activeRocket.map(_ - 1)) --> activeRocket),
      activeRocket.map { value =>
        val thrust = value % 5
        svg(
          viewBox := "0 0 1400 700",
          width   := "1400",
          height  := "700",
          circle(cx := location.x.toString, cy := location.y.toString, r := "10", fill := "black"),
          Rocket.rocketWithFlame(thrust, location, -45, 75),
        )
      },
    )
  }

  case class HighScore(level: Level, score: Option[Int])

  def allHighscores(allLevels: List[Level]): IO[List[HighScore]] = {
    import cats.implicits._
    allLevels.traverse { l =>
      loadBestRecording(l).map(recorder => HighScore(l, recorder.map(_.score)))
    }
  }

  def loadBestRecording(level: Level): IO[Option[FlightRecoder]] =
    IO(
      Option(org.scalajs.dom.window.localStorage.getItem(localStorageName(level)))
        .flatMap(io.circe.parser.decode[FlightRecoder](_).toOption),
    )

  def localStorageName(level: Level): String =
    s"level-${level.name}"

  def storeBestRecording(level: Level, flightRecoder: FlightRecoder): IO[Unit] = {
    import io.circe.syntax._
    IO(
      org.scalajs.dom.window.localStorage.setItem(localStorageName(level), flightRecoder.asJson.noSpacesSortKeys),
    )
  }

  def renderUi(allLevels: List[Level]) = {
    val selectedLevel = Subject.behavior(allLevels.lastOption)

    val refreshHighscoresSub = Subject.behavior(())

    val levelSelection = refreshHighscoresSub.map(_ =>
      div(
        position.absolute,
        right := "1rem",
        top   := "1rem",
        allHighscores(allLevels).map { highScores =>
          table(
            thead(
              tr(th("Level"), th("Action"), th("Your Highscore")),
            ),
            tbody(highScores.map { case HighScore(level, maybeScore) =>
              tr(
                td(level.name),
                td(button("Play", onClick.as(Some(level)) --> selectedLevel)),
                td(maybeScore),
              )
            }),
          )
        },
      ),
    )

    val levelDisplay = selectedLevel.mapEffect {
      case Some(level) => loadBestRecording(level).map(rec => Some(level -> rec))
      case None        => IO.pure(None)
    }.map {
      case Some((level, maybeRecorder)) =>
        VModifier(allHighscores(allLevels).map { highScores =>
          renderLevel(level, maybeRecorder, selectedLevel, refreshHighscoresSub)
        })

      case None => VModifier.empty
    }
    div(
      levelSelection,
      levelDisplay,
    )
  }

  def landerSettings(state: BehaviorSubject[Simulator.State]) =
    state.map { s =>
      div(
        display.flex,
        flexDirection.column,
        label(
          "x",
          input(
            tpe     := "range",
            minAttr := "0",
            maxAttr := "7000",
            value   := s.x.toString,
            cls     := "range",
            onInput.value.map(v => s.copy(x = v.toInt)) --> state,
          ),
        ),
        label(
          "y",
          input(
            tpe     := "range",
            minAttr := "0",
            maxAttr := "3000",
            value   := s.y.toString,
            cls     := "range",
            onInput.value.map(v => s.copy(y = v.toInt)) --> state,
          ),
        ),
        label(
          "rotation",
          input(
            tpe     := "range",
            minAttr := "-90",
            maxAttr := "90",
            value   := s.rotate.toString,
            cls     := "range",
            onInput.value.map(v => s.copy(rotate = v.toInt)) --> state,
          ),
        ),
      )
    }

  def renderLevel(
    level: Level,
    maybeRecorder: Option[FlightRecoder],
    selectedLevelSub: BehaviorSubject[Option[Level]],
    refreshHighScoreSub: BehaviorSubject[Unit],
  ): VModifier = {

    val initialState = PreciseState(
      x = level.landerInitialState.x,
      y = level.landerInitialState.y,
      rotate = level.landerInitialState.rotate,
      hSpeed = level.landerInitialState.hSpeed,
      vSpeed = level.landerInitialState.vSpeed,
      power = level.landerInitialState.power,
      fuel = level.landerInitialState.fuel,
    )

    val best = maybeRecorder.map { rec =>
      val bestPath = Simulator.runFlightRecorder(level, rec)
      (rec, bestPath)
    }

    val initialGameState: GameState = GameState.Paused

    val gameState: BehaviorSubject[GameState] =
      Subject.behavior(initialGameState)
    val gameSettings                          = Subject.behavior(GameSettings(fps = 5))

    val interval = gameState.withLatest(gameSettings).map { case (gs, settings) =>
      gs match {
        case GameState.Stopped(_) => None
        case GameState.Paused     => None
        case GameState.Running    => Some(settings.fps)
      }
    }

    val tick: Observable[Long] = interval.switchMap {
      case None      => Observable.fromIterable(List.empty)
      case Some(fps) => Observable.intervalMillis((1000.0 / fps).toInt)
    }

    val landerControlSub =
      Subject.behavior(MouseControlState(level.landerInitialState.rotate, level.landerInitialState.power))

    val simulation: Observable[((Long, PreciseState, Queue[(PreciseState, GameCommand)]), EvaluationResult)] =
      tick
        .withLatest(landerControlSub)
        .scan0((0L, initialState, Queue.empty[(PreciseState, GameCommand)])) {
          case ((_, state, previous), (tickMs, control)) =>
            val gameCommand         = GameCommand(control.angle, control.thrust)
            val simulationStepInput = SimulationStepInput(level, state, gameCommand)
            val newState            = simulate(simulationStepInput)
            (tickMs, newState, previous.enqueue((state, gameCommand)))
        }
        .map { case msg @ (_, current, previous) =>
          msg -> current.evaluate(level, previous.lastOption.map(_._1))
        }
        .map { case msg @ ((_, current, previous), result) =>
          result match {
            case EvaluationResult.AllClear =>
            case other                     =>
              gameState.unsafeOnNext(GameState.Stopped(other))
          }
          msg
        }
        .mapEffect { case msg @ ((_, state, previous), result) =>
          result match {
            case EvaluationResult.Landed =>
              val currentScore = score(level, state)
              if (currentScore > best.map(_._1.score).getOrElse(0)) {
                storeBestRecording(level, FlightRecoder(currentScore, previous.map(_._2).toList))
                  .map(_ => refreshHighScoreSub.unsafeOnNext(()))
                  .as(msg)
              }
              else IO(msg)
            case _                       => IO(msg)
          }
        }
        .debugLog("Simulation")
//        .via(simulationState.contramap[(Long, PreciseState, Queue[PreciseState])] { case (_, s, _) =>
//          if (s.y < 0) {
//            println("CRASHED")
//            SimulationState.Crashed
//          }
//          else {
//            SimulationState.AllClear
//          }
//        })

    def formatNum(n: Double): String = f"$n%1.2f"

    div(
      h1(s"Level ${level.name}"),
      simulation.map { case ((_, s, previous), result) =>
        val radar = calcShipRadar(s, level)

        VModifier(
          h2("Score"),
          table(
            thead(tr(th(textAlign.right, "Score"), th(textAlign.right, "Highscore"))),
            tbody(tr(td(textAlign.right, score(level, s)), td(textAlign.right, best.map(_._1.score)))),
          ),
          div(
            display.flex,
            flex                   := "row",
            VModifier.style("gap") := "3rem",
            div(
              h2("Game Control"),
              div(
                gameState.map { gs =>
                  val gsSpecific = gs match {
                    case GameState.Paused          =>
                      button(s"Start", onClick.as(GameState.Running) --> gameState)
                    case GameState.Running         =>
                      button(s"Pause", onClick.as(GameState.Paused) --> gameState)
                    case GameState.Stopped(reason) =>
                      reason match {
                        case EvaluationResult.AllClear  => p("this shouldn't happen")
                        case EvaluationResult.Crashed   => p("Houston, we had a problem...")
                        case EvaluationResult.OffLimits => p("Off script is ok, but off screen...?")
                        case EvaluationResult.Landed    => p(fontSize := "2.5rem", "🎉")
                      }
                  }
                  VModifier(
                    gsSpecific,
                    button(s"Restart", onClick.as(Some(level)) --> selectedLevelSub),
                  )
                },
              ),
            ),
            div(
              h2("Lander Control"),
              landerControlSub.map { ctrl =>
                table(
                  thead(tr(th(textAlign.right, "angle"), th(textAlign.right, "thrust"))),
                  tbody(
                    tr(
                      td(textAlign.right, width := "50%", fontFamily := "monospace", roundAt(2)(ctrl.angle)),
                      td(textAlign.right, width := "50%", fontFamily := "monospace", roundAt(2)(ctrl.thrust)),
                    ),
                  ),
                )
              },
            ),
            div(
              h2("Lander State"),
              div(
                table(
                  thead(
                    tr(
                      th(textAlign.right, "x"),
                      th(textAlign.right, "y"),
                      th(textAlign.right, "rotation"),
                      th(textAlign.right, "power"),
                      th(textAlign.right, "fuel"),
                      th(textAlign.right, "hSpeed"),
                      th(textAlign.right, "vSpeed"),
                      th(textAlign.right, "closest distance"),
                      th(textAlign.right, "radars"),
                    ),
                  ),
                  tbody(
                    tr(
                      td(textAlign.right, width := "10.0%", fontFamily := "monospace", formatNum(roundAt(2)(s.x))),
                      td(textAlign.right, width := "10.0%", fontFamily := "monospace", formatNum(roundAt(2)(s.y))),
                      td(textAlign.right, width := "10.0%", fontFamily := "monospace", s.rotate),
                      td(textAlign.right, width := "10.0%", fontFamily := "monospace", s.power),
                      td(textAlign.right, width := "10.0%", fontFamily := "monospace", s.fuel),
                      td(textAlign.right, width := "10.0%", fontFamily := "monospace", formatNum(roundAt(2)(s.hSpeed))),
                      td(textAlign.right, width := "10.0%", fontFamily := "monospace", formatNum(roundAt(2)(s.vSpeed))),
                      td(
                        textAlign.right,
                        width                   := "10.0%",
                        fontFamily              := "monospace", {
                          val collidingRays: List[(Algorithms.LineIntersection, Double)] =
                            radar.flatMap(_.maybeClosestCollisionPointAndDistance)
                          val closestDistance                                            = collidingRays.map(_._2).minOption
                          val foo: String                                                = closestDistance.map(d => formatNum(roundAt(2)(d))).getOrElse("---")
                          foo
                        },
                      ),
                      td(textAlign.right, width := "10.0%", fontFamily := "monospace", "???"),
                    ),
                  ),
                ),
              ),
            ),
          ),
          renderLevelGraphic(
            level,
            s,
            previous.map(_._1),
            landerControlSub,
            best.map(_._2).getOrElse(List.empty),
            radar,
          ),
          h2("Game Log"),
          div(
            table(
              thead(
                tr(
                  th(textAlign.right, "tick"),
                  th(textAlign.right, "x"),
                  th(textAlign.right, "y"),
                  th(textAlign.right, "rotation"),
                  th(textAlign.right, "power"),
                  th(textAlign.right, "fuel"),
                  th(textAlign.right, "hSpeed"),
                  th(textAlign.right, "vSpeed"),
                  th(textAlign.right, "power cmd"),
                  th(textAlign.right, "rotation cmd"),
                ),
              ),
              tbody(
                previous.map { case (oldS, gc) => (oldS, Some(gc)) }
                  .enqueue(s -> None)
                  .zipWithIndex
                  .map { case ((s, gc), tick) =>
                    val powerStr    = gc.map(x => x.power.toString).getOrElse("---")
                    val rotationStr = gc.map(x => x.rotation.toString).getOrElse("---")
                    tr(
                      td(textAlign.right, width := "10%", fontFamily := "monospace", tick),
                      td(textAlign.right, width := "10%", fontFamily := "monospace", formatNum(roundAt(2)(s.x))),
                      td(textAlign.right, width := "10%", fontFamily := "monospace", formatNum(roundAt(2)(s.y))),
                      td(textAlign.right, width := "10%", fontFamily := "monospace", formatNum(roundAt(2)(s.rotate))),
                      td(textAlign.right, width := "10%", fontFamily := "monospace", s.power),
                      td(textAlign.right, width := "10%", fontFamily := "monospace", s.fuel),
                      td(textAlign.right, width := "10%", fontFamily := "monospace", formatNum(roundAt(2)(s.hSpeed))),
                      td(textAlign.right, width := "10%", fontFamily := "monospace", formatNum(roundAt(2)(s.vSpeed))),
                      td(textAlign.right, width := "10%", fontFamily := "monospace", powerStr),
                      td(textAlign.right, width := "10%", fontFamily := "monospace", rotationStr),
                    )
                  },
              ),
            ),
          ),
          h2("Game Input"),
          pre(level.toInputLines.mkString("\n")),
        )
      },
    )
  }

  case class ShipRay(ship: Vec2, ray: Line, angleDeg: Int, intersections: List[Algorithms.LineIntersection]) {
    def maybeClosestCollisionPointAndDistance: Option[(Algorithms.LineIntersection, Double)] = intersections.map {
      intersection =>
        val distance = ship.-(intersection.pos).abs.length
        intersection -> distance
    }.minByOption(_._2)

  }

  def calcShipRadar(landerState: PreciseState, level: Level): List[ShipRay] = {

    val ship            = Vec2(landerState.x, landerState.y)
    val gameMapDiagonal =
      math.sqrt(Constants.gameHeight * Constants.gameHeight + Constants.gameWidth * Constants.gameWidth)

    // create rays in 5° distance
    val rays = 0.until(end = 360, step = 5).map { angleDeg =>
      val radarRay = Line.apply(ship, ship + Vec2.unit(angleDeg) * gameMapDiagonal)

      val collisions = level.surfaceLines.flatMap { l =>
        l.intersect(radarRay) match {
          case Some(i) if i.onLine1 && i.onLine2 => List(i)
          case _                                 => List.empty
        }
      }

      ShipRay(ship, radarRay, angleDeg, collisions)

    }
    rays.toList
  }

  def toDisplayCoord(coord: Coord): Coord =
    Coord(coord.x, 3000 - coord.y)

  case class MouseControlState(angle: Int, thrust: Int)

  def toCoord(v: Vec2): Coord =
    Coord(PreciseState.myRound(v.x), PreciseState.myRound(v.y))

  def renderVelocityIndicator(landerSettings: PreciseState) = {

    val v               = Vec2(landerSettings.hSpeed, -landerSettings.vSpeed)
    val l               = v.length
    val max             = 400
    val maxLength       = max.toString
    val centerIndicator = Vec2(max, max)
    val end             = toCoord(centerIndicator + v)

    import svg._
    svg(
      // circle(cx := maxLength, cy := maxLength, r := maxLength, fill := "none", stroke := "black", strokeWidth := "5"),
      circle(cx := maxLength, cy := maxLength, r := "20", fill := "black"),
      defs(
        marker(
          idAttr       := "arrowhead",
          markerWidth  := "10",
          markerHeight := "7",
          refX         := "0",
          refY         := "3.5",
          orient       := "auto",
          polygon(points := "0 0, 10 3.5, 0 7"),
        ),
      ),
      VModifier.ifTrue(landerSettings.hSpeed != 0 || landerSettings.vSpeed != 0) {
        line(
          x1                             := centerIndicator.x.toString,
          y1                             := centerIndicator.y.toString,
          x2                             := end.x.toString,
          y2                             := end.y.toString,
          stroke                         := "#000",
          VModifier.attr("stroke-width") := "8",
          VModifier.attr("marker-end")   := "url(#arrowhead)",
        )
      },
    )
  }

  case class MouseDragThrustControl(startDragPercent: Option[Vec2], dragLocationPercent: Option[Vec2])

  def clamp[N: Numeric](x: N, lower: N, upper: N): N = {
    val ord = implicitly[Ordering[N]]

    if (ord.gt(x, upper)) upper
    else if (ord.lt(x, lower)) lower
    else x
  }

  def thrustVectoringControl(
    mouseDragControlSub: Subject[MouseDragThrustControl],
    ctrlSub: Subject[MouseControlState],
  ): Observable[VModifier] = {
    /*
    - mouse down (capture mouse point --> translate to percent-coord)
    - mouse move (capture 2nd mouse point --> translate to percent-coord)
    - calculate vector between points
    - draw vector
    - translate vector to control (angle (-90 - 90), thrust (0 - 5) )

     */

    import svg._

    def toViewBox(c: Vec2): Vec2 =
      Vec2(viewBoxSize.x * c.x, viewBoxSize.y * c.y)

    mouseDragControlSub.distinctOnEquals.map { ctrl =>
      ctrl.startDragPercent.map(toViewBox).zip(ctrl.dragLocationPercent.map(toViewBox)) match {
        case None                   => VModifier.empty
        case Some((start, current)) =>
          val canvasVector = current - start

          val thrustVector = {
            val isDraggingBelowHorizon = canvasVector.y > 0

            val y =
              if (isDraggingBelowHorizon) -1e-10 // mini number, to prevent weird edge case behavior
              else canvasVector.y

            canvasVector.copy(y = y)
          }
          val length       = thrustVector.length

          // control is a half circle (upper half)
          val maxLength   = 400
          val controlSize = Vec2(2 * maxLength + 10, maxLength + 10)

          val lengthRatio      = length / maxLength
          val normalized       = clamp(myRound(25)((lengthRatio * 100).toInt), 0, 100) // [0, 25, 50, 75, 100]
          val normalizedLength = normalized / 100.0 * maxLength

          val thrustAngle    = thrustVector.copy(x = thrustVector.x).angle
          val thrustAngleDeg = math.round(thrustAngle.toDegrees) // -180 - +180
          val clampedAngle   = thrustAngleDeg

          val normalizedThrustVector = Vec2.unit(clampedAngle.toRadians) * normalizedLength

//          debugSub.unsafeOnNext(s"""
//              |val canvasVector           = $canvasVector
//              |val thrustVector           = $thrustVector
//              |val length                 = $length
//              |val maxLength              = $maxLength
//              |val controlSize            = $controlSize
//              |val lengthRatio            = $lengthRatio
//              |val normalized             = $normalized
//              |val normalizedLength       = $normalizedLength
//              |val thrustAngle            = $thrustAngle
//              |val thrustAngleDeg         = $thrustAngleDeg
//              |val normalizedThrustVector = $normalizedThrustVector
//              |val clampedAngle           = $clampedAngle
//              |""".stripMargin.trim)

          ctrlSub.unsafeOnNext(MouseControlState(angle = (-1 * (clampedAngle + 90)).toInt, thrust = normalized / 25))

          svg(
            x             := (start.x - controlSize.x / 2).toInt.toString,                                 // "0",
            y             := (start.y - controlSize.y).toInt.toString,                                     // "0",
            viewBox       := s"-${controlSize.x / 2} -${controlSize.y} ${controlSize.x} ${controlSize.y}", // min-x min-y width height
            width         := controlSize.x.toString,
            height        := controlSize.y.toString,
            pointerEvents := "none",

            // circle(cx := maxLength, cy := maxLength, r := maxLength, fill := "none", stroke := "black", strokeWidth := "5"),
            circle(
              cx            := "0",
              cy            := "0",
              r             := "40",
              fill          := "green",
              pointerEvents := "none",
            ),
            circle(
              cx            := "0",
              cy            := "0",
              r             := maxLength.toString,
              fill          := "none",
              stroke        := "green",
              strokeWidth   := "10",
              pointerEvents := "none",
            ),
            defs(
              marker(
                idAttr       := "arrowhead",
                markerWidth  := "10",
                markerHeight := "7",
                refX         := "0",
                refY         := "3.5",
                orient       := "auto",
                polygon(points := "0 0, 10 3.5, 0 7"),
              ),
            ),
            VModifier.ifTrue(length >= 10) {
              line(
                pointerEvents                  := "none",
                x1                             := "0",
                y1                             := "0",
                x2                             := normalizedThrustVector.x.toString,
                y2                             := normalizedThrustVector.y.toString,
                stroke                         := "green",
                VModifier.attr("stroke-width") := "8",
                // VModifier.attr("marker-end")   := "url(#arrowhead)",
              )
            },
          )
      }
    }

  }

  def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }

  def myRound(base: Int)(x: Int): Int =
    (base * math.round(x / base.toDouble)).toInt

  val canvasSize: Vec2  = Vec2(1400, 600)
  val viewBoxSize: Vec2 = Vec2(7000, 3000)

  def renderLevelGraphic(
    level: Level,
    lander: PreciseState,
    previous: Queue[PreciseState],
    landerControl: BehaviorSubject[MouseControlState],
    highScorePath: List[PreciseState],
    radar: List[ShipRay],
  ) = {
    import svg._
    val allCoords =
      Coord(0, 0) :: level.initialState.surfaceCoords ::: List(Coord(7000, 0))

    val displayCoords = allCoords.map(toDisplayCoord)
    val pts           = displayCoords.map { case Coord(x, y) => s"$x, $y" }.mkString(" ")

    def landerStuff(lander: PreciseState) = {

      val landerCoords        = Coord(PreciseState.myRound(lander.x), PreciseState.myRound(lander.y))
      val landerDisplayCoords = toDisplayCoord(landerCoords)

      List(
        g(
          Rocket.rocketWithFlame(lander.power, Vec2(landerDisplayCoords.x, landerDisplayCoords.y), lander.rotate, 200),
        ),
        g(
          radar.map { ray =>
            val isColliding = ray.maybeClosestCollisionPointAndDistance.isDefined
            val end         = ray.maybeClosestCollisionPointAndDistance.map(_._1.pos).getOrElse(ray.ray.end)

            val endCoord =
              toDisplayCoord(Coord(PreciseState.myRound(end.x), PreciseState.myRound(end.y)))
            line(
              stroke        := (if (isColliding) "red" else "green"),
              strokeWidth   := "2",
              x1            := landerDisplayCoords.x.toString,
              y1            := landerDisplayCoords.y.toString,
              x2            := endCoord.x.toString,
              y2            := endCoord.y.toString,
              pointerEvents := "none",
            )
          },
        ),
        renderVelocityIndicator(lander)(pointerEvents := "none"),
      )
    }

    val simulationSteps =
      landerControl.map(s => GameCommand(s.angle, s.thrust)).map { ctrl =>
        Simulator
          .runUntilCrashed(SimulationStepInput(level, lander, ctrl))
          .map(toRoundedState)
          .map(s => toDisplayCoord(Coord(s.x, s.y)))
      }

    val ctrlSub  = Subject.behavior(MouseDragThrustControl(None, None))
    val debugSub = Subject.behavior("debug")

    div(
      svg(
        width   := canvasSize.x.toInt.toString,                          // "1400",
        height  := canvasSize.y.toInt.toString,                          // "600",
        viewBox := s"0 0 ${viewBoxSize.x.toInt} ${viewBoxSize.y.toInt}", // "0 0 7000 3000",
        rect(
          x             := "0",
          y             := "0",
          width         := viewBoxSize.x.toInt.toString, // "7000",
          height        := viewBoxSize.y.toInt.toString, // "3000",
          fill          := "hsla(200,10%,80%,0.9)",
          pointerEvents := "none",
        ),
        polyline(
          points        := pts,
          fill          := "red",
          stroke        := "black",
          strokeWidth   := "3",
          pointerEvents := "none",
        ),
        simulationSteps.map { locations =>
          locations.map { case Coord(x, y) =>
            circle(cx := x.toString, cy := y.toString, r := "10", fill := "black", pointerEvents := "none")
          }
        },
        previous
          .map(toRoundedState)
          .map(s => toDisplayCoord(Coord(s.x, s.y)))
          .map { case Coord(x, y) =>
            circle(cx := x.toString, cy := y.toString, r := "10", fill := "lightgreen", pointerEvents := "none")
          },
        highScorePath
          .map(toRoundedState)
          .map(s => toDisplayCoord(Coord(s.x, s.y)))
          .map { case Coord(x, y) =>
            circle(cx := x.toString, cy := y.toString, r := "5", fill := "darkred", pointerEvents := "none")
          },

        //        rays.map { case ShipRay(surfacePoint, ship, ray, intersections) =>
//          val start = landerDisplayCoords
//          val end   = toDisplayCoord(Coord(surfacePoint.x.toInt, surfacePoint.y.toInt))
//          line(
//            stroke        := (if (intersections.isEmpty) "green" else "red"),
//            strokeWidth   := "5",
//            x1            := start.x.toString,
//            y1            := start.y.toString,
//            x2            := end.x.toString,
//            y2            := end.y.toString,
//            pointerEvents := "none",
//          )
//        },
        onMouseDown.map(getRelativeLocationOfMouseEventInContainer).map { relLocation =>
          MouseDragThrustControl(Some(relLocation), Some(relLocation))
        } --> ctrlSub,
        ctrlSub.map { ctrl =>
          onMouseMove.map { evt =>
            if (ctrl.startDragPercent.isEmpty) ctrl
            else {
              val relLocation = getRelativeLocationOfMouseEventInContainer(evt)
              ctrl.copy(dragLocationPercent = Some(relLocation))
            }
          } --> ctrlSub
        },
        onMouseUp.as(MouseDragThrustControl(None, None)) --> ctrlSub,
        thrustVectoringControl(ctrlSub, landerControl),
        landerStuff(lander),

//    line(
//      stroke := "green",
//      strokeWidth := "20",
//      x1 := start.x,
//      y1 := start.y,
//      x2 := end.x,
//      y2 := end.y
//    ),
      ),
      pre(debugSub),
    )

  }

  def counter = SyncIO {
    // https://outwatch.github.io/docs/readme.html#example-counter
    val number = Subject.behavior(0)
    div(
      button("+", onClick(number.map(_ + 1)) --> number, marginRight := "10px"),
      number,
    )
  }

  def inputField = SyncIO {
    // https://outwatch.github.io/docs/readme.html#example-input-field
    val text = Subject.behavior("")
    div(
      input(
        tpe := "text",
        value <-- text,
        onInput.value --> text,
      ),
      button("clear", onClick.as("") --> text),
      div("text: ", text),
      div("length: ", text.map(_.length)),
    )
  }

  def getRelativeLocationOfMouseEventInContainer(evt: TypedTargetMouseEvent[Element]): Vec2 = {
    val e    = evt.target
    evt.preventDefault()
    evt.stopPropagation()
    evt.stopImmediatePropagation()
    val dim  = e.getBoundingClientRect()
    val w    = dim.width
    val h    = dim.height
    val x    = evt.clientX - dim.left
    val y    = evt.clientY - dim.top
    val relX = x / w
    val relY = y / h

    if (
      relX > 1 || relY > 1 || w < canvasSize.x * 0.9 || w > canvasSize.x * 1.1 || h < canvasSize.y * 0.9 || h > canvasSize.y * 1.1
    ) {
      println(s"""ERROR
e   : $e
dim : $dim
w   : $w
h   : $h
x   : $x
y   : $y
relX: $relX
relY: $relY
""".stripMargin)
    }

    val relLocation = Vec2(relX, relY)

    relLocation
  }

}
