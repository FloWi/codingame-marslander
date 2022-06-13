package webapp

import cats.effect.SyncIO
import colibri.Subject
import outwatch._
import outwatch.dsl._
import webapp.marslander.{Coord, SurfaceModel}

// Outwatch documentation:
// https://outwatch.github.io/docs/readme.html

object Main {

  def main(args: Array[String]): Unit =
    Outwatch.renderInto[SyncIO]("#app", app).unsafeRunSync()

  def app =
    div(
      Communication.getLevels.map(renderUi),
    )

  def renderUi(allLevels: List[SurfaceModel])     = {
    val selectedLevel  = Subject.behavior(allLevels.headOption)
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
  def renderLevel(level: SurfaceModel): VModifier =
    div(
      h1(s"Level ${level.name}"),
      renderLevelGraphic(level),
      h2("Game Input"),
      pre(level.toInputLines.mkString("\n")),
    )

  def renderLevelGraphic(level: SurfaceModel) = {
    import svg._
    val allCoords =
      Coord(0, 0) :: level.initialState.surfaceCoords ::: List(Coord(7000, 0))
    val pts       = allCoords.map { case Coord(x, y) => s"$x, $y" }.mkString(" ")
    svg(
      width     := "1400",
      height    := "600",
      viewBox   := "0 0 7000 3000",
      transform := "scale(1,-1)",
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
