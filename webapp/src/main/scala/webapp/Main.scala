package webapp

import cats.effect.SyncIO
import colibri.{BehaviorSubject, Observable, Observer, Subject}
import com.raquo.domtypes.jsdom.defs.events.TypedTargetMouseEvent
import org.scalajs.dom.Element
import outwatch._
import outwatch.dsl._
import webapp.marslander.Game.{GameSettings, GameState}
import webapp.marslander.{Coord, Level}
import webapp.simulator.Simulator
import webapp.simulator.Simulator.PreciseState.{fromRoundedState, toRoundedState}
import webapp.simulator.Simulator.{simulate, GameCommand, PreciseState, SimulationStepInput}
import webapp.vectory.{Algorithms, Line, Vec2}

import scala.collection.immutable.Queue

// Outwatch documentation:
// https://outwatch.github.io/docs/readme.html

object Main {

  def main(args: Array[String]): Unit =
    Outwatch.renderInto[SyncIO]("#app", app).unsafeRunSync()

  def app =
    div(
      Communication.getLevels.map(renderUi),
    )

  def renderUi(allLevels: List[Level]) = {
    val selectedLevel  = Subject.behavior(allLevels.lastOption)
    val levelSelection = div(allLevels.map { level =>
      button(s"level ${level.name}", onClick.as(Some(level)) --> selectedLevel)
    })

    val levelDisplay = selectedLevel.map {
      case Some(level) => renderLevel(level)
      case None        => VModifier.empty
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

  def renderLevel(level: Level): VModifier = {

    val initialState = Simulator.PreciseState(
      x = level.landerInitialState.x,
      y = level.landerInitialState.y,
      rotate = level.landerInitialState.rotate,
      hSpeed = level.landerInitialState.hSpeed,
      vSpeed = level.landerInitialState.vSpeed,
      power = level.landerInitialState.power,
      fuel = level.landerInitialState.fuel,
    )

    val initialGameState: GameState = GameState.Initial

    val gameState: BehaviorSubject[GameState] =
      Subject.behavior(initialGameState)
    val gameSettings                          = Subject.behavior(GameSettings(fps = 5))

    val interval = gameState.withLatest(gameSettings).map { case (gs, settings) =>
      gs match {
        case GameState.Paused  => None
        case GameState.Running => Some(settings.fps)
        case GameState.Crashed => None
        case GameState.Initial => None
      }
    }

    val tick: Observable[Long] = interval.switchMap {
      case None      => Observable(0)
      case Some(fps) => Observable.intervalMillis((1000.0 / fps).toInt)
    }

    val landerControlSub =
      Subject.behavior(MouseControlState(level.landerInitialState.rotate, level.landerInitialState.power))

    val simulation: Observable[(PreciseState, Queue[PreciseState])] =
      tick.withLatest(landerControlSub).scan0((initialState, Queue.empty[PreciseState])) {
        case ((state, previous), (_, control)) =>
          val simulationStepInput = SimulationStepInput(level, state, GameCommand(control.angle, control.thrust))
          val newState            = simulate(simulationStepInput)
          (newState, previous.enqueue(state))
      }

    simulation.unsafeForeach(gs => println(gs))

//    tick.withLatest(landerControlSub).unsafeForeach { case (ms, ctrl) =>
//      println(s"withLatest Tick Tock at $ms: $ctrl")
//    }

//    tick
//      .withLatest(landerControlSub)
//      .scan(2000) { case (current, (tick, ctrl)) => current - ctrl.thrust }
//      .unsafeForeach(fuel => println(s"[${System.currentTimeMillis}] fuel left: $fuel"))

//    landerControlSub.debounceMillis(1000).unsafeForeach(ctrl => println(s"debounce time: $ctrl"))
//    landerControlSub
//      .sampleMillis(1000)
//      .unsafeForeach(ctrl => println(s"${System.currentTimeMillis} sampleMills time: $ctrl"))

//    landerControlSub.zip(tick).unsafeForeach { case (ctrl, ms) =>
//      println(s"zip Tick Tock $ms: $ctrl")
//    }
//
//    tick.zip(landerControlSub).unsafeForeach { case (ctrl, ms) =>
//      println(s"zip2 Tick Tock $ms: $ctrl")
//    }

//    tick.withLatest(landerControlSub).unsafeForeach { case (ms, ctrl) =>
//      println(s"withLatest Tick Tock at $ms: $ctrl")
//    }
//
//    tick.switchMap(_ => landerControlSub).unsafeForeach(ctrl => println(s"switchMap Tick Tock: $ctrl"))
//    tick.combineLatest(landerControlSub).unsafeForeach { case (ms, ctrl) =>
//      println(s"combineLatest Tick Tock $ms: $ctrl")
//    }

    // val landerState = gameState.map(gs => toRoundedState(gs.state))

//    simulation.map(gs => toRoundedState(gs.state)).map { s =>
//
    div(
      h1(s"Level ${level.name}"),
      // landerSettings(landerState),

      tick.map(ms => div(p(ms))),
      div(
        display.flex,
        flex                   := "row",
        VModifier.style("gap") := "3rem",
        div(
          h2("Game Control"),
          div(
            gameState.map {
              case GameState.Paused  =>
                button(s"Start", onClick.as(GameState.Running) --> gameState)
              case GameState.Running =>
                button(s"Pause", onClick.as(GameState.Paused) --> gameState)
              case GameState.Crashed => VModifier.empty
              case GameState.Initial =>
                VModifier.empty
                button(s"Start", onClick.as(GameState.Running) --> gameState)
            },
          ),
        ),
        div(
          h2("Lander Control"),
          landerControlSub.map { ctrl =>
            table(
              thead(tr(th("angle"), th("thrust"))),
              tbody(tr(td(roundAt(2)(ctrl.angle)), td(roundAt(2)(ctrl.thrust)))),
            )
          },
        ),
        div(
          h2("Lander State"),
          div(
            simulation.map(_._1).map { s =>
              table(
                thead(tr(th("x"), th("y"), th("rotation"), th("hSpeed"), th("vSpeed"))),
                tbody(
                  tr(
                    td(roundAt(2)(s.x)),
                    td(roundAt(2)(s.y)),
                    td(roundAt(2)(s.rotate)),
                    td(roundAt(2)(s.hSpeed)),
                    td(roundAt(2)(s.vSpeed)),
                  ),
                ),
              )
            },
          ),
        ),
      ),
      renderLevelGraphic(level, simulation.map(gs => toRoundedState(gs._1)), landerControlSub),
      h2("Game Input"),
      pre(level.toInputLines.mkString("\n")),
    )
  }

  case class ShipRay(surfacePoint: Vec2, ship: Vec2, ray: Line, intersections: List[Algorithms.LineIntersection])
  def calcShipRays(landerState: Simulator.State, level: Level): List[ShipRay] = {
    val surfaceLines = level.initialState.surfaceCoords
      .sliding(2, 1)
      .toList
      .map { case List(first, second) =>
        Line(first.x, first.y, second.x, second.y)
      }

    val ship = Vec2(landerState.x, landerState.y)
    val rays = level.initialState.surfaceCoords.map { c =>
      val surfacePoint      = Vec2(c.x, c.y)
      val ray               = Line(ship, surfacePoint)
      val otherSurfaceLines =
        surfaceLines.filterNot(l => (l.start - surfacePoint).length < 0.01 || (l.end - surfacePoint).length < 0.01)
      val collisions        = otherSurfaceLines.flatMap { l =>
        l.intersect(ray) match {
          case Some(i) if i.onLine1 && i.onLine2 => List(i)
          case _                                 => List.empty
        }
      }

      ShipRay(surfacePoint, ship, ray, collisions)
    }

    rays
  }

  def toDisplayCoord(coord: Coord): Coord =
    Coord(coord.x, 3000 - coord.y)

  case class MouseControlState(angle: Int, thrust: Int)

  def displaySpeedIndicator(landerSettings: Simulator.State) = {

    val v               = Vec2(landerSettings.hSpeed, landerSettings.vSpeed)
    val l               = v.length
    val max             = 400
    val maxLength       = max.toString
    val centerIndicator = Vec2(max, max)
    val end             = centerIndicator + v

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

          val thrustAngle    = thrustVector.angle
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

          ctrlSub.unsafeOnNext(MouseControlState(angle = (clampedAngle + 90).toInt, thrust = normalized / 25))

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
    lander: Observable[Simulator.State],
    landerControl: BehaviorSubject[MouseControlState],
  ) = {
    import svg._
    val allCoords =
      Coord(0, 0) :: level.initialState.surfaceCoords ::: List(Coord(7000, 0))

    val displayCoords = allCoords.map(toDisplayCoord)
    val pts           = displayCoords.map { case Coord(x, y) => s"$x, $y" }.mkString(" ")

    def landerStuff(lander: Simulator.State) = {
      val landerWidth  = 335.6
      val landerHeight = 308.7

      val landerScaleFactor = 0.55

      val landerDisplayW      = landerWidth * landerScaleFactor
      val landerDisplayH      = landerHeight * landerScaleFactor
      val landerCoords        = Coord(lander.x, lander.y)
      val landerDisplayCoords = toDisplayCoord(landerCoords)
      val landerX: Int        = landerDisplayCoords.x - (landerDisplayW / 2).toInt
      val landerY: Int        = landerDisplayCoords.y - landerDisplayH.toInt

      List(
        g(
          pointerEvents                             := "none",
          transform                                 := s"translate($landerX, $landerY)",
          g(
            pointerEvents := "none",
            image(
              //          <image x="10" y="20" width="80" height="80" href="recursion.svg" />
              // w: 335,6
              // h: 308,7
              href      := s"${Communication.assetLocation}/Lander.svg",
              width     := landerDisplayW.toString,
              height    := landerDisplayH.toString,
              //            VModifier.attr("transform-box")    := s"fill-box",
              //            VModifier.attr("transform-origin") := s"center",
              transform := s"rotate(${-lander.rotate}, ${landerDisplayW / 2}, ${landerDisplayH / 2})",
            ),
          ),
        ),
        displaySpeedIndicator(lander)(pointerEvents := "none"),
      )
    }

    val simulationSteps =
      lander.map(s => fromRoundedState(s)).combineLatest(landerControl.map(s => GameCommand(s.angle, s.thrust))).map {
        case (s, ctrl) =>
          Simulator
            .runUntilCrashed(SimulationStepInput(level, s, ctrl))
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
        lander.map(landerStuff),

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
