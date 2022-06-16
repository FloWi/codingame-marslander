package webapp

import cats.effect.SyncIO
import colibri.{BehaviorSubject, Observable, Subject}
import com.raquo.domtypes.jsdom.defs.events.TypedTargetMouseEvent
import org.scalajs.dom.{Element, KeyCode}
import outwatch._
import outwatch.dsl._
import webapp.marslander.{Coord, Level}
import webapp.vectory.Vec2

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

  case class LanderSettings(x: Int, y: Int, rotation: Int, vH: Int, vV: Int)
  def landerSettings(state: BehaviorSubject[LanderSettings]) =
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
            value   := s.rotation.toString,
            cls     := "range",
            onInput.value.map(v => s.copy(rotation = v.toInt)) --> state,
          ),
        ),
      )
    }

  def renderLevel(level: Level): VModifier = {

    val landerState = Subject.behavior(
      LanderSettings(
        level.landerInitialState.x,
        level.landerInitialState.y,
        level.landerInitialState.rotate,
        level.landerInitialState.hSpeed,
        level.landerInitialState.vSpeed,
      ),
    )

    val landerControl =
      Subject.behavior(MouseControlState(level.landerInitialState.rotate, level.landerInitialState.power))

    div(
      h1(s"Level ${level.name}"),
      landerSettings(landerState),
      landerState.map { s =>
        div(
          pre(s"""(${s.x}, ${s.y}) @ ${s.rotation}°
               |vH: ${s.vH}
               |vV: ${s.vV}
               |""".stripMargin),
          renderLevelGraphic(level, s),
        )
      },
      renderControlPanel(landerControl),
      h2("Game Input"),
      pre(level.toInputLines.mkString("\n")),
    )
  }

  def toDisplayCoord(coord: Coord): Coord =
    Coord(coord.x, 3000 - coord.y)

  case class MouseControlState(angle: Int, thrust: Int)

  def renderControlPanel(controlState: BehaviorSubject[MouseControlState]) = {
    import svg._

    sealed trait PowerCtrl
    case class Set(thrust: Int) extends PowerCtrl
    case object Increase        extends PowerCtrl
    case object Decrease        extends PowerCtrl

    val onNumber = onKeyDown.map { e =>
      if (e.key == "+") Some(Increase)
      else if (e.key == "-") Some(Decrease)
      else
        e.keyCode match {
          case KeyCode.Num0 => Some(Set(0))
          case KeyCode.Num1 => Some(Set(1))
          case KeyCode.Num2 => Some(Set(2))
          case KeyCode.Num3 => Some(Set(3))
          case KeyCode.Num4 => Some(Set(4))
          case _            => None
        }
    }

    controlState.map { s =>
      div(
        onNumber.map {
          case Some(Set(value))               => s.copy(thrust = value)
          case Some(Increase) if s.thrust < 4 => s.copy(thrust = s.thrust + 1)
          case Some(Decrease) if s.thrust > 0 => s.copy(thrust = s.thrust - 1)
          case _                              => s
        } --> controlState,
        label(
          "rotation",
          input(
            tpe     := "range",
            minAttr := "-90",
            maxAttr := "90",
            value   := s.angle.toString,
            cls     := "range",
            onInput.value.map(v => s.copy(angle = v.toInt)) --> controlState,
          ),
        ),
        pre(
          s"""x: ${s.angle}
             |thrust: ${s.thrust}""".stripMargin,
        ),
        svg(
          width  := "400",
          height := "100",
          fill   := "lightgrey",
        ),
      )
    }
  }

  def displaySpeedIndicator(landerSettings: LanderSettings) = {

    val v               = Vec2(landerSettings.vH, landerSettings.vV)
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
      VModifier.ifTrue(landerSettings.vH != 0 || landerSettings.vV != 0) {
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

  case class ThrustVectorControl(startDragPercent: Option[Vec2], dragLocationPercent: Option[Vec2])

  def thrustVectoringControl(ctrlSub: Subject[ThrustVectorControl]): Observable[VModifier] = {
    /*
    - mouse down (capture mouse point --> translate to percent-coord)
    - mouse move (capture 2nd mouse point --> translate to percent-coord)
    - calculate vector between points
    - draw vector
    - translate vector to control (angle (-90 - 90), thrust (0 - 5) )

     */

    import svg._

    val max       = 400
    val maxLength = max.toString

    def toViewBox(c: Vec2): Vec2 =
      Vec2(viewBoxSize.x * c.x, viewBoxSize.y * c.y)

    ctrlSub.distinctOnEquals.map { ctrl =>
      ctrl.startDragPercent.map(toViewBox).zip(ctrl.dragLocationPercent.map(toViewBox)) match {
        case None                   => VModifier.empty
        case Some((start, current)) =>
          val thrustVector = current - start
          val length       = thrustVector.length

          val controlSize = Vec2(800, 800)

          val startInside   = controlSize / 2
          val currentInside = startInside + thrustVector

//          println(s"rendering thrustVectoring control")
//          println(s"   startDragPercent: ${ctrl.startDragPercent}")
//          println(s"              start: $start")
//          println(s" dragLocationPercent: ${ctrl.dragLocationPercent}")
//          println(s"             current: $current")
//          println(s"        thrustVector: $thrustVector")
//          println(s"         startInside: $startInside")
//          println(s"       currentInside: $currentInside")

          svg(
            x             := (start.x - (controlSize.x / 2)).toInt.toString, // "0",
            y             := (start.y - (controlSize.y / 2)).toInt.toString, // "0",
            width         := controlSize.x.toString,
            height        := controlSize.y.toString,
            pointerEvents := "none",

            // circle(cx := maxLength, cy := maxLength, r := maxLength, fill := "none", stroke := "black", strokeWidth := "5"),
            circle(
              cx            := startInside.x.toInt.toString,
              cy            := startInside.y.toInt.toString,
              r             := "40",
              fill          := "green",
              pointerEvents := "none",
            ),
            circle(
              cx            := startInside.x.toInt.toString,
              cy            := startInside.y.toInt.toString,
              r             := (controlSize.x / 2).toString,
              fill          := "none",
              stroke        := "green",
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
                x1                             := startInside.x.toInt.toString,
                y1                             := startInside.y.toInt.toString,
                x2                             := currentInside.x.toString,
                y2                             := currentInside.y.toString,
                stroke                         := "green",
                VModifier.attr("stroke-width") := "8",
                VModifier.attr("marker-end")   := "url(#arrowhead)",
              )
            },
          )
      }
    }

  }

  def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }

  val canvasSize: Vec2  = Vec2(1400, 600)
  val viewBoxSize: Vec2 = Vec2(7000, 3000)

  def renderLevelGraphic(level: Level, lander: LanderSettings) = {
    import svg._
    val allCoords =
      Coord(0, 0) :: level.initialState.surfaceCoords ::: List(Coord(7000, 0))

    val displayCoords = allCoords.map(toDisplayCoord)
    val pts           = displayCoords.map { case Coord(x, y) => s"$x, $y" }.mkString(" ")

    val landerWidth  = 335.6
    val landerHeight = 308.7

    val landerScaleFactor = 0.55

    val landerDisplayW = landerWidth * landerScaleFactor
    val landerDisplayH = landerHeight * landerScaleFactor

    val landerCoords        = Coord(lander.x, lander.y)
    val landerDisplayCoords = toDisplayCoord(landerCoords)
    val landerX: Int        = landerDisplayCoords.x - (landerDisplayW / 2).toInt
    val landerY: Int        = landerDisplayCoords.y - landerDisplayH.toInt

    val ctrlSub = Subject.behavior(ThrustVectorControl(None, None))

    def getPercentFromMouseEvent(evt: TypedTargetMouseEvent[Element]): Vec2 = {
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

    svg(
      width   := canvasSize.x.toInt.toString,                          // "1400",
      height  := canvasSize.y.toInt.toString,                          // "600",
      viewBox := s"0 0 ${viewBoxSize.x.toInt} ${viewBoxSize.y.toInt}", // "0 0 7000 3000",
      rect(
        x                                         := "0",
        y                                         := "0",
        width                                     := viewBoxSize.x.toInt.toString, // "7000",
        height                                    := viewBoxSize.y.toInt.toString, // "3000",
        fill                                      := "hsla(200,10%,80%,0.9)",
        pointerEvents                             := "none",
      ),
      polyline(
        points                                    := pts,
        fill                                      := "red",
        stroke                                    := "black",
        strokeWidth                               := "3",
        pointerEvents                             := "none",
      ),
      onMouseDown.map(getPercentFromMouseEvent).map { relLocation =>
        ThrustVectorControl(Some(relLocation), Some(relLocation))
      } --> ctrlSub,
      ctrlSub.map { ctrl =>
        onMouseMove.map { evt =>
          if (ctrl.startDragPercent.isEmpty) ctrl
          else {
            val relLocation = getPercentFromMouseEvent(evt)
            ctrl.copy(dragLocationPercent = Some(relLocation))
          }

        } --> ctrlSub
      },
      onMouseUp.as(ThrustVectorControl(None, None)) --> ctrlSub,
      thrustVectoringControl(ctrlSub),
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
            transform := s"rotate(${lander.rotation}, ${landerDisplayW / 2}, ${landerDisplayH / 2})",
          ),
        ),
      ),
      displaySpeedIndicator(lander)(pointerEvents := "none"),

      //    line(
//      stroke := "green",
//      strokeWidth := "20",
//      x1 := start.x,
//      y1 := start.y,
//      x2 := end.x,
//      y2 := end.y
//    ),
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

}
