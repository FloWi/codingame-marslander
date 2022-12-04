package webapp

import cats.effect.{IO, SyncIO}
import colibri.{BehaviorSubject, Observable, Subject}
import com.raquo.domtypes.jsdom.defs.events.TypedTargetMouseEvent
import io.circe.syntax.EncoderOps
import org.scalajs.dom.{console, Element}
import outwatch._
import outwatch.dsl._
import simulator.Simulator
import simulator.Simulator.PreciseState.toRoundedState
import simulator.Simulator._
import webapp.Model.{MouseControlState, UISettings}
import webapp.graphics.Helper.toDisplayCoord
import webapp.graphics.LevelRenderer.{canvasSize, renderThrustVectoringControl, svgGameGraphic, viewBoxSize}
import webapp.graphics.Rocket
import webapp.marslander.Game.{GameSettings, GameState}
import webapp.marslander.{Coord, Level}
import webapp.vectory.{Algorithms, Vec2}
import typings.tensorflowTfjs.{mod => tf}
import typings.tensorflowTfjsCore.distTensorMod.{Tensor1D, Tensor2D}
import typings.tensorflowTfjsCore.distTypesMod.{TensorLike, TensorLike1D, TensorLike2D}

import scala.collection.immutable.Queue
import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichIterableOnce

// Outwatch documentation:
// https://outwatch.github.io/docs/readme.html

object Main {

  def main(args: Array[String]): Unit =
    Outwatch.renderInto[SyncIO]("#app", app).unsafeRunSync()
//    Outwatch.renderInto[SyncIO]("#app", svgPlayground).unsafeRunSync()

  def app =
    div(
      for {
        allLevels  <- Communication.getLevels
        _          <- IO(println("waiting for tensorflow to be ready..."))
        _          <- IO.fromPromise(IO(tf.ready()))
        _          <- IO(println(s"tf version: ${typings.tensorflowTfjs.versionMod.version}"))
        _          <- IO(println(s"tf backend: ${typings.tensorflowTfjs.mod.getBackend()}"))
        uiSettings <- loadUiSettings
      } yield renderUi(allLevels, uiSettings),
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
      Option(org.scalajs.dom.window.localStorage.getItem(localStorageNameLevel(level)))
        .flatMap(io.circe.parser.decode[FlightRecoder](_).toOption),
    )

  def localStorageNameLevel(level: Level): String =
    s"level-${level.name}"

  def storeBestRecording(level: Level, flightRecoder: FlightRecoder): IO[Unit] = {
    import io.circe.syntax._
    IO(
      org.scalajs.dom.window.localStorage.setItem(localStorageNameLevel(level), flightRecoder.asJson.noSpacesSortKeys),
    )
  }

  def loadUiSettings: IO[UISettings] = IO(
    Option(org.scalajs.dom.window.localStorage.getItem("uiSettings"))
      .flatMap(io.circe.parser.decode[UISettings](_).toOption)
      .getOrElse(UISettings.default),
  )

  def storeUiSettings(uiSettings: UISettings): IO[Unit] = {
    import io.circe.syntax._
    IO(
      org.scalajs.dom.window.localStorage.setItem("uiSettings", uiSettings.asJson.noSpacesSortKeys),
    )
  }

  def renderUi(allLevels: List[Level], uiSettings: UISettings): VNode = {
    val selectedLevel = Subject.behavior(allLevels.lastOption)

    val uiSettingsSub: BehaviorSubject[UISettings] = Subject.behavior(uiSettings)

    uiSettingsSub.mapEffect { newValue =>
      storeUiSettings(newValue) *> IO.pure(newValue)
    }.unsafeForeach { newValue =>
      println(s"stored new uiSettings: $newValue")
    }

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
        renderLevel(level, maybeRecorder, selectedLevel, refreshHighscoresSub, uiSettingsSub)

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

  def radarOverviewTable(rays: List[ShipRay]) =
    table(
      thead(
        tr(rays.sortBy(_.angleDeg).map(sr => th(sr.angleDeg))),
      ),
      tbody(
        tr(rays.sortBy(_.angleDeg).map(sr => td(sr.maybeClosestCollisionPointAndDistance.map(_._2.toInt)))),
      ),
    )

  def toTensor(preciseState: PreciseState): Tensor1D = {
    val state: Vector[Double] = Vector(
      preciseState.x,
      preciseState.y,
      preciseState.hSpeed,
      preciseState.vSpeed,
      preciseState.fuel,
      preciseState.rotate,
      preciseState.power,
    )
    tf.tensor1d(js.Array.from(state.toJSArray).asInstanceOf[TensorLike1D])
  }

  def renderLevel(
    level: Level,
    maybeRecorder: Option[FlightRecoder],
    selectedLevelSub: BehaviorSubject[Option[Level]],
    refreshHighScoreSub: BehaviorSubject[Unit],
    uiSettingsSub: BehaviorSubject[UISettings],
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
        .map { msg =>
          val tensor = toTensor(msg._1._2)

          console.log("1st tensor", tensor.toString(verbose = true))
          tensor.dispose() // cleanup in non gc environment like webgl
          /*
          alternative:
          const y = tf.tidy(() => {
            const result = a.square().log().neg();
            return result;
          });
           */

          msg

        }
        .map { case msg @ ((_, current, previous), result) =>
          result match {
            case EvaluationResult.AllClear(_) =>
            case other                        =>
              gameState.unsafeOnNext(GameState.Stopped(other))
          }
          msg
        }
        .mapEffect { case msg @ ((_, state, previous), result) =>
          result match {
            case EvaluationResult.Landed(_) =>
              val currentScore = score(level, state)
              if (currentScore > best.map(_._1.score).getOrElse(0)) {
                storeBestRecording(level, FlightRecoder(currentScore, previous.map(_._2).toList))
                  .map(_ => refreshHighScoreSub.unsafeOnNext(()))
                  .as(msg)
              }
              else IO(msg)
            case _                          => IO(msg)
          }
        }
        .debugLog("Simulation")

    div(
      h1(s"Level ${level.name}"),
      simulation.map { case ((_, s, previous), evaluationResult) =>
        val radar        = calcShipRadar(s, level)
        val landingRadar = calcLandingAreaAccess(s, level)

        uiSettingsSub.map { uiSettings =>
          val divScore                = div(
            idAttr := "score",
            h2("Score"),
            table(
              thead(tr(th(textAlign.right, "Score"), th(textAlign.right, "Highscore"))),
              tbody(tr(td(textAlign.right, score(level, s)), td(textAlign.right, best.map(_._1.score)))),
            ),
          )
          val divGameControl          = div(
            idAttr := "gameControl",
            h2("Game Control"),
            div(
              div(
                display.flex,
                flexDirection.row,
                gameState.map { gs =>
                  val gsSpecific = gs match {
                    case GameState.Paused          =>
                      button(s"Start", onClick.as(GameState.Running) --> gameState)
                    case GameState.Running         =>
                      button(s"Pause", onClick.as(GameState.Paused) --> gameState)
                    case GameState.Stopped(reason) =>
                      reason match {
                        case EvaluationResult.AllClear(_)  => p("this shouldn't happen")
                        case EvaluationResult.Crashed(_)   => p("Houston, we had a problem...")
                        case EvaluationResult.OffLimits(_) => p("Off script is ok, but off screen...?")
                        case EvaluationResult.Landed(_)    => p(fontSize := "2.5rem", "🎉")
                      }
                  }
                  VModifier(
                    button(s"Restart", onClick.as(Some(level)) --> selectedLevelSub),
                    gsSpecific,
                  )
                },
              ),
              div(
                VModifier(
                  label(
                    "radar",
                    input(
                      typ     := "checkbox",
                      checked := uiSettings.showRadar,
                      onChange.checked.map(checked => uiSettings.copy(showRadar = checked)) --> uiSettingsSub,
                    ),
                  ),
                  label(
                    "high score path",
                    input(
                      typ     := "checkbox",
                      checked := uiSettings.showHighScorePath,
                      onChange.checked.map { checked =>
                        uiSettings.copy(showHighScorePath = checked)
                      } --> uiSettingsSub,
                    ),
                  ),
                ),
              ),
            ),
          )
          val divLanderControlDisplay = div(
            idAttr := "landerControl",
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
          )
          val divLanderState          = div(
            idAttr := "landerState",
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
                    th(textAlign.right, "suicide burn height at curr vV"),
                    th(textAlign.right, "closest radar distance"),
                    th(textAlign.right, "landing radar GO?"),
                    th(textAlign.right, "horizontal distance to landing area"),
                    th(textAlign.right, "vertical distance to landing area"),
                    th(textAlign.right, "distance to landing area"),
                    th(textAlign.right, "collision distance on current trajectory"),
                  ),
                ),
                tbody(
                  tr(
                    td(textAlign.right, width := "10.0%", fontFamily := "monospace", formatNum(roundAt(2)(s.x))),
                    td(textAlign.right, width := "10.0%", fontFamily := "monospace", formatNum(roundAt(2)(s.y))),
                    td(
                      textAlign.right,
                      width                   := "10%",
                      fontFamily              := "monospace",
                      redIfOutOfLimits(s.rotate == 0),
                      formatNum(roundAt(2)(s.rotate)),
                    ),
                    td(textAlign.right, width := "10.0%", fontFamily := "monospace", s.power),
                    td(textAlign.right, width := "10.0%", fontFamily := "monospace", s.fuel),
                    td(
                      textAlign.right,
                      width                   := "10%",
                      fontFamily              := "monospace",
                      redIfOutOfLimits(math.abs(s.hSpeed) < Constants.maxLandingHorizontalSpeed),
                      formatNum(roundAt(2)(s.hSpeed)),
                    ),
                    td(
                      textAlign.right,
                      width                   := "10%",
                      fontFamily              := "monospace",
                      redIfOutOfLimits(math.abs(s.vSpeed) < Constants.maxLandingVerticalSpeed),
                      formatNum(roundAt(2)(s.vSpeed)),
                    ),
                    td(
                      textAlign.right,
                      width                   := "10.0%",
                      fontFamily              := "monospace",
                      formatNum(roundAt(2)(calcSuicideBurnHeight(s, level))),
                    ),
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
                    td(
                      textAlign.right,
                      width                   := "10.0%",
                      fontFamily              := "monospace",
                      landingRadar.map(_.maybeNearestCollision.isEmpty.toString).mkString("|"),
                    ),
                    td(
                      textAlign.right,
                      width                   := "10.0%",
                      fontFamily              := "monospace",
                      formatNum(roundAt(2)(evaluationResult.enrichedState.horizontalDistanceLandingArea)),
                    ),
                    td(
                      textAlign.right,
                      width                   := "10.0%",
                      fontFamily              := "monospace",
                      formatNum(roundAt(2)(evaluationResult.enrichedState.verticalDistanceLandingArea)),
                    ),
                    td(
                      textAlign.right,
                      width                   := "10.0%",
                      fontFamily              := "monospace",
                      formatNum(roundAt(2)(evaluationResult.enrichedState.distanceLandingArea)),
                    ),
                    td(
                      textAlign.right,
                      width                   := "10.0%",
                      fontFamily              := "monospace",
                      evaluationResult.enrichedState.collisionDistanceOnCurrentTrajectory
                        .map(n => formatNum(roundAt(2)(n))),
                    ),
                  ),
                ),
              ),
            ),
          )

          VModifier(
            div(
              display.flex,
              flex                   := "row",
              VModifier.style("gap") := "3rem",
              divGameControl,
              divLanderControlDisplay,
              divScore,
            ),
            divLanderState,
            renderLevelGraphic(
              level,
              s,
              previous.map(_._1),
              landerControlSub,
              best.map(_._2).getOrElse(List.empty),
              radar,
              landingRadar,
              uiSettings,
            ),
            h2("Detailed State"),
            pre(evaluationResult.enrichedState.asJson.spaces2SortKeys),
            h2("Detailed Flattened State"),
            pre(evaluationResult.enrichedState.flattened.asJson.spaces2SortKeys),
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
                        td(
                          textAlign.right,
                          width                   := "10%",
                          fontFamily              := "monospace",
                          redIfOutOfLimits(s.rotate == 0),
                          formatNum(roundAt(2)(s.rotate)),
                        ),
                        td(textAlign.right, width := "10%", fontFamily := "monospace", s.power),
                        td(textAlign.right, width := "10%", fontFamily := "monospace", s.fuel),
                        td(
                          textAlign.right,
                          width                   := "10%",
                          fontFamily              := "monospace",
                          redIfOutOfLimits(math.abs(s.hSpeed) < Constants.maxLandingHorizontalSpeed),
                          formatNum(roundAt(2)(s.hSpeed)),
                        ),
                        td(
                          textAlign.right,
                          width                   := "10%",
                          fontFamily              := "monospace",
                          redIfOutOfLimits(math.abs(s.vSpeed) < Constants.maxLandingVerticalSpeed),
                          formatNum(roundAt(2)(s.vSpeed)),
                        ),
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
        }
      },
    )
  }

  def formatNum(n: Double): String = f"$n%1.2f"

  def redIfOutOfLimits(isWithinLimit: Boolean): VModifier =
    VModifier.ifNot(isWithinLimit) {
      color := "red"
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

    def toViewBox(c: Vec2): Vec2 =
      Vec2(viewBoxSize.x * c.x, viewBoxSize.y * c.y)

    mouseDragControlSub.distinctOnEquals.map { ctrl =>
      ctrl.startDragPercent.map(toViewBox).zip(ctrl.dragLocationPercent.map(toViewBox)) match {
        case None                   => VModifier.empty
        case Some((start, current)) =>
          val maxLength = 400

          val ThrustVector(
            renderLocation,
            controlSize,
            length,
            normalizedThrustVector,
            mouseControlState,
          ) = calcThrustVector(start, current, maxLength)

          ctrlSub.unsafeOnNext(mouseControlState)

          renderThrustVectoringControl(
            renderLocation,
            controlSize,
            length,
            maxLength,
            normalizedThrustVector,
          )
      }
    }
  }

  case class ThrustVector(
    renderLocation: Vec2,
    controlSize: Vec2,
    length: Double,
    normalizedThrustVector: Vec2,
    mouseControlState: MouseControlState,
  )

  /** Translates the click-and-drag interaction into a clamped thrustVector
    */
  def calcThrustVector(startDragLocation: Vec2, currentDragLocation: Vec2, maxLength: Int): ThrustVector = {
    val start   = startDragLocation
    val current = currentDragLocation

    // control is a half circle (upper half)
    val controlSize = Vec2(2 * maxLength + 10, maxLength + 10)

    val renderLocation: Vec2 = Vec2(
      start.x - controlSize.x / 2,
      start.y - controlSize.y,
    )

    val canvasVector = current - start

    val thrustVector = {
      val isDraggingBelowHorizon = canvasVector.y > 0

      val y =
        if (isDraggingBelowHorizon) -1e-10 // mini number, to prevent weird edge case behavior
        else canvasVector.y

      canvasVector.copy(y = y)
    }
    val length       = thrustVector.length

    val lengthRatio      = length / maxLength
    val normalized       = clamp(myRound(25)((lengthRatio * 100).toInt), 0, 100) // [0, 25, 50, 75, 100]
    val normalizedLength = normalized / 100.0 * maxLength

    val thrustAngle    = thrustVector.copy(x = thrustVector.x).angle
    val thrustAngleDeg = math.round(thrustAngle.toDegrees) // -180 - +180
    val clampedAngle   = thrustAngleDeg

    val normalizedThrustVector = Vec2.unit(thrustAngleDeg.toRadians) * normalizedLength
    val mouseControl           = MouseControlState(angle = (-1 * (clampedAngle + 90)).toInt, thrust = normalized / 25)

    ThrustVector(
      renderLocation,
      controlSize,
      length,
      normalizedThrustVector,
      mouseControl,
    )
  }

  def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }

  def myRound(base: Int)(x: Int): Int =
    (base * math.round(x / base.toDouble)).toInt

  def renderLevelGraphic(
    level: Level,
    lander: PreciseState,
    previous: Queue[PreciseState],
    landerControl: BehaviorSubject[MouseControlState],
    highScorePath: List[PreciseState],
    radar: List[ShipRay],
    landingRadar: List[LandingRadarRay],
    uiSettings: UISettings,
  ): VNode = {

    val simulationSteps: Observable[List[Coord]] = landerControl
      .map(s => GameCommand(s.angle, s.thrust))
      .map(getSimulationStepsBasedOnCurrentGameCommand(level, lander, _))

    val ctrlSub  = Subject.behavior(MouseDragThrustControl(None, None))
    val debugSub = Subject.behavior("debug")

    val interactionEventHandlers: List[VModifier] = List(
      onMouseDown.map(getRelativeLocationOfMouseEventInContainer).map { relLocation =>
        println("onMouseDown")
        MouseDragThrustControl(Some(relLocation), Some(relLocation))
      } --> ctrlSub,
      ctrlSub.map { ctrl =>
        onMouseMove.map { evt =>
          // println("onMouseMove")
          if (ctrl.startDragPercent.isEmpty) ctrl
          else {
            val relLocation = getRelativeLocationOfMouseEventInContainer(evt)
            ctrl.copy(dragLocationPercent = Some(relLocation))
          }
        } --> ctrlSub
      },
      onMouseUp.as(MouseDragThrustControl(None, None)).map { ev =>
        // println("onMouseUp")
        ev
      } --> ctrlSub,
    )

    val highScorePathToVisualize = Option.when(uiSettings.showHighScorePath)(highScorePath)

    val gameGraphic        = svgGameGraphic(
      level,
      lander,
      simulationSteps,
      previous,
      highScorePathToVisualize,
      interactionEventHandlers,
      radar,
      landingRadar,
      showRadar = true,
    )
    val interactiveGraphic = gameGraphic(
      thrustVectoringControl(ctrlSub, landerControl),
    )
    div(
      interactiveGraphic,
      pre(debugSub),
    )

  }

  def getSimulationStepsBasedOnCurrentGameCommand(level: Level, lander: PreciseState, ctrl: GameCommand): List[Coord] =
    Simulator
      .runUntilCrashed(SimulationStepInput(level, lander, ctrl))
      .map(toRoundedState)
      .map(s => toDisplayCoord(Coord(s.x, s.y)))

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

/*

lunar lander openAI gym

### Observation Space
The state is an 8-dimensional vector: the coordinates of the lander in `x` & `y`, its linear
velocities in `x` & `y`, its angle, its angular velocity, and two booleans
that represent whether each leg is in contact with the ground or not.
### Rewards
After every step a reward is granted. The total reward of an episode is the
sum of the rewards for all the steps within that episode.
For each step, the reward:
- is increased/decreased the closer/further the lander is to the landing pad.
- is increased/decreased the slower/faster the lander is moving.
- is decreased the more the lander is tilted (angle not horizontal).
- is increased by 10 points for each leg that is in contact with the ground.
- is decreased by 0.03 points each frame a side engine is firing.
- is decreased by 0.3 points each frame the main engine is firing.
The episode receive an additional reward of -100 or +100 points for crashing or landing safely respectively.
An episode is considered a solution if it scores at least 200 points.



state = [
0:    (pos.x - VIEWPORT_W / SCALE / 2) / (VIEWPORT_W / SCALE / 2),
1:    (pos.y - (self.helipad_y + LEG_DOWN / SCALE)) / (VIEWPORT_H / SCALE / 2),
2:    vel.x * (VIEWPORT_W / SCALE / 2) / FPS,
3:    vel.y * (VIEWPORT_H / SCALE / 2) / FPS,
4:    self.lander.angle,
5:    20.0 * self.lander.angularVelocity / FPS,
6:    1.0 if self.legs[0].ground_contact else 0.0,
7:    1.0 if self.legs[1].ground_contact else 0.0,
]

assert len(state) == 8

reward = 0
shaping = (
    -100 * np.sqrt(state[0] * state[0] + state[1] * state[1])
    - 100 * np.sqrt(state[2] * state[2] + state[3] * state[3])
    - 100 * abs(state[4])
    + 10 * state[6]
    + 10 * state[7]
)  # And ten points for legs contact, the idea is if you
# lose contact again after landing, you get negative reward
if self.prev_shaping is not None:
    reward = shaping - self.prev_shaping
self.prev_shaping = shaping

reward -= (
    m_power * 0.30
)  # less fuel spent is better, about -30 for heuristic landing
reward -= s_power * 0.03

terminated = False
if self.game_over or abs(state[0]) >= 1.0:
    terminated = True
    reward = -100
if not self.lander.awake:
    terminated = True
    reward = +100

 */

/*
distanceLandingArea
fuel
hSpeed
horizontalDistanceLandingArea
isCrashed
isLanded
isOffLimits
isOutOfFuel
landingRadarDistances
power
radarDistances
rotation
vSpeed
verticalDistanceLandingArea
x
y
 */
