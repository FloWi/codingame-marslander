package webapp.graphics

import colibri.Observable
import outwatch._
import outwatch.dsl._
import simulator.Simulator.PreciseState.toRoundedState
import simulator.Simulator._
import webapp.graphics.Helper.{toCoord, toDisplayCoord}
import webapp.marslander.{Coord, Level}
import webapp.vectory.Vec2

object LevelRenderer {

  val canvasSize: Vec2  = Vec2(1400, 600)
  val viewBoxSize: Vec2 = Vec2(7000, 3000)

  def svgGameGraphic(
    level: Level,
    lander: PreciseState,
    simulationSteps: Observable[List[Coord]],
    previous: Seq[PreciseState],
    maybeHighScorePath: Option[List[PreciseState]],
    interactionControls: List[VModifier],
    radar: List[ShipRay],
    landingRadar: List[LandingRadarRay],
    showRadar: Boolean,
  ): VNode = {
    import svg._

    val allCoords =
      Coord(0, 0) :: level.initialState.surfaceCoords ::: List(Coord(7000, 0))

    val displayCoords = allCoords.map(toDisplayCoord)
    val pts           = displayCoords.map { case Coord(x, y) => s"$x, $y" }.mkString(" ")

    val gPredictedPath        = g(
      idAttr := "predictedPath",
      simulationSteps.map { steps =>
        steps.map { case Coord(x, y) =>
          circle(cx := x.toString, cy := y.toString, r := "10", fill := "black", pointerEvents := "none")
        }
      },
    )
    val gAlreadyTravelledPath = g(
      idAttr := "alreadyTravelledPath",
      previous
        .map(toRoundedState)
        .map(s => toDisplayCoord(Coord(s.x, s.y)))
        .map { case Coord(x, y) =>
          circle(cx := x.toString, cy := y.toString, r := "10", fill := "lightgreen", pointerEvents := "none")
        },
    )

    val gPastAndFuturePath = g(
      idAttr := "pastAndFuturePath",
      gPredictedPath,
      gAlreadyTravelledPath,
    )

    val gMaybeHighscorePath = maybeHighScorePath.map { highScorePath =>
      g(
        idAttr := "highScorePath",
        highScorePath
          .map(toRoundedState)
          .map(s => toDisplayCoord(Coord(s.x, s.y)))
          .map { case Coord(x, y) =>
            circle(cx := x.toString, cy := y.toString, r := "5", fill := "darkred", pointerEvents := "none")
          },
      )
    }

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
      gPastAndFuturePath,
      gMaybeHighscorePath,
      interactionControls,
      //      thrustVectoringControl(ctrlSub, landerControl),
      landerStuff(lander, radar, landingRadar, showRadar),
    )
  }

  def landerStuff(
    lander: PreciseState,
    radar: List[ShipRay],
    landingRadar: List[LandingRadarRay],
    showRadar: Boolean,
  ): List[VModifier] = {
    import svg._

    val landerCoords        = Coord(PreciseState.myRound(lander.x), PreciseState.myRound(lander.y))
    val landerDisplayCoords = toDisplayCoord(landerCoords)

    List(
      g(
        Rocket.rocketWithFlame(lander.power, Vec2(landerDisplayCoords.x, landerDisplayCoords.y), lander.rotate, 200),
      ),
      VModifier.ifTrue(showRadar)(
        g(
          idAttr := "radarRays",
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
              title         := s"${ray.angleDeg}°",
            )
          },
        ),
      ),
      g(
        idAttr                                      := "landingRadarRays",
        landingRadar.map { ray =>
          val isColliding = ray.maybeNearestCollision.isDefined
          val end         = ray.landingAreaLocation

          val endCoord =
            toDisplayCoord(Coord(PreciseState.myRound(end.x), PreciseState.myRound(end.y)))
          line(
            stroke        := (if (isColliding) "orangered" else "blueviolet"),
            strokeWidth   := "8",
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

  def renderVelocityIndicator(landerSettings: PreciseState): VNode = {

    val v               = Vec2(landerSettings.hSpeed, -landerSettings.vSpeed)
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

  def renderThrustVectoringControl(
    location: Vec2,
    controlSize: Vec2,
    length: Double,
    maxLength: Int,
    normalizedThrustVector: Vec2,
  ): VNode = {
    import svg._

    svg(
      x             := location.x.toInt.toString,                                                    // "0",
      y             := location.y.toInt.toString,                                                    // "0",
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
