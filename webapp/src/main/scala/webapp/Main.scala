package webapp

import cats.effect.SyncIO
import colibri.{BehaviorSubject, Subject}
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
      h2("Game Input"),
      pre(level.toInputLines.mkString("\n")),
    )
  }

  def toDisplayCoord(coord: Coord): Coord =
    Coord(coord.x, 3000 - coord.y)

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

    svg(
      width   := "1400",
      height  := "600",
      viewBox := "0 0 7000 3000",
      rect(
        x           := "0",
        y           := "0",
        width       := "7000",
        height      := "3000",
        fill        := "hsla(200,10%,80%,0.9)",
      ),
      polyline(
        points      := pts,
        fill        := "red",
        stroke      := "black",
        strokeWidth := "3",
      ),
      g(
        transform   := s"translate($landerX, $landerY)",
        g(
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
      displaySpeedIndicator(lander),

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
