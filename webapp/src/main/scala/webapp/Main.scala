package webapp

import cats.effect.SyncIO
import colibri.Subject
import outwatch._
import outwatch.dsl._
import webapp.marslander.SurfaceModel

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
  def renderLevel(model: SurfaceModel): VModifier =
    div(
      h1(s"Level ${model.name}"),
      pre(model.toInputLines.mkString("\n")),
    )

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
