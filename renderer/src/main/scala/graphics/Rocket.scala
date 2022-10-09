package webapp.graphics
import outwatch._
import outwatch.dsl._
import webapp.vectory.Vec2
object Rocket {

  import svg._

  /*
  origin: width/2, height
  origin: 634 / 2 = 317


  rocket height: 609px


rocket  height: 609
rocket1 height: 609
rocket2 height: 688
rocket3 height: 817
rocket4 height: 950

rocket  width: 634
rocket1 width: 630
rocket2 width: 634
rocket3 width: 630
rocket4 width: 630

   */

  val rocketWidth           = 634
  val rocketHeight          = 609
  val rocketHeightWithFlame = 950

  def rocketWithFlame(thrust: Int, location: Vec2, rotation: Int, expectedRocketHeight: Int): SvgVNode = {
    val flame = thrust match {
      case 1 => Assets.flame1
      case 2 => Assets.flame2
      case 3 => Assets.flame3
      case 4 => Assets.flame4
      case _ => g()
    }

    val scaleFactor: Double = expectedRocketHeight.toDouble / rocketHeight

    val scaledWidth           = rocketWidth * scaleFactor
    val scaledHeight          = rocketHeight * scaleFactor
    val scaledHeightWithFlame = rocketHeightWithFlame * scaleFactor

    val pivot          = Vec2(scaledWidth / 2, scaledHeight)
    val rocketLocation = location - pivot

    val svgDegree = rotation * -1

    g(
      transform := s"translate(${rocketLocation.x} ${rocketLocation.y}) rotate($svgDegree ${pivot.x} ${pivot.y})",
      Assets.rocket(
        width  := s"$scaledWidth",
        height := s"$scaledHeightWithFlame",
//        x      := rocketLocation.x.toString,
//        y      := rocketLocation.y.toString,
//      VModifier.attr("transform-origin") := s"${location.x} ${location.y}",
      )(flame),
    )

  }

  object Assets {

    val rocket = svg(
      viewBox   := "0 0 634 950",
      styleAttr := "fill-rule:evenodd;clip-rule:evenodd;stroke-linejoin:round;stroke-miterlimit:2;",
      g(
        idAttr := "rocket",
        path(
          idAttr                     := "rocket1",
          VModifier.attr("serif:id") := "rocket",
          d                          := "M475.267,117.801l-111.565,-113.308l-109.821,0c2.613,1.744 -113.767,117.52 -113.792,119.043c-0.851,52.702 -0.258,105.418 -0.387,158.125c3.05,-0.87 69.649,70.751 79.315,78.446c-6.828,5.664 -17.078,8.803 -20.484,16.995c-5.97,14.365 -7.067,52.615 -7.067,52.615c-0,0 -93.205,46.127 -101.446,45.441l0,95.875c-25.494,0.654 -51.034,0.299 -76.484,1.961c-12.682,0.828 -13.555,29.99 -8.061,30.289c65.854,3.57 131.944,1.51 197.852,-0.872c18.23,-0.66 2.197,-29.896 -6.973,-31.378c-25.563,-4.133 -51.789,-0.483 -77.685,-0.723c-1.05,-18.564 5.035,-79.211 5.035,-79.211c0,-0 86.596,-44.705 84.853,-47.319c55.782,-0.437 111.563,-1.215 167.348,-1.307c9.647,-0.018 19.983,-2.64 28.926,0.98c34.446,13.942 99.199,50.88 99.199,50.88c-0,0 0.58,51.133 0.872,76.7c-1.744,0 -55.981,-1.905 -81.877,1.961c-5.82,0.869 -21.361,27.005 -11.33,28.055c33.353,3.492 141.685,3.231 211.448,-7.102c0.056,-8.105 3.01,-18.298 -10.599,-19.973c-28.132,-3.459 -56.654,-1.961 -84.981,-2.941l-0,-95.875l-97.658,-47.336c-0,-0 -3.971,-35.836 -12.617,-53.875c-2.969,-6.195 -12.481,-5.741 -18.722,-8.611c4.12,-5.655 77.363,-79.604 77.363,-79.604l-0.662,-167.931Zm-73.214,306.803l-1.994,-28.141l-174.07,-2.147l0.501,29.112l175.563,1.176Zm-142.943,-388.733l-85.754,91.722l-0,149.506c-0,-0 85.754,87.365 85.754,91.723l99.363,-0l86.792,-89.277l1.726,-154.065l-85.758,-88.095l-102.123,-1.514Z",
          styleAttr                  := "fill:#b3b3b3;stroke:#000;stroke-width:4.17px;",
        ),
      ),
    )

    val flame1 = g(
      idAttr := "flame_1",
      path(
        d         := "M291.029,493.703c-0.017,-27.732 9.754,-50.218 21.811,-50.231c12.057,-0.025 21.844,22.424 21.85,50.143c0.016,27.72 -21.801,87.876 -21.801,87.876c0,0 -21.855,-60.069 -21.86,-87.788Z",
        styleAttr := "fill:#ff8400;stroke:#000;stroke-width:4.17px;",
      ),
      path(
        d         := "M301.94,493.691c-0.006,-13.866 4.885,-25.122 10.911,-25.135c6.031,0 10.916,11.219 10.922,25.072c0.005,13.866 -10.9,37.683 -10.9,37.683c-0,-0 -10.917,-23.767 -10.933,-37.62Z",
        styleAttr := "fill:#ffdb3d;stroke:#000;stroke-width:4.17px;",
      ),
    )

    val flame2 = g(
      idAttr := "flame_2",
      path(
        d         := "M291.029,530.657c-0.017,-48.134 9.754,-87.163 21.811,-87.185c12.057,-0.043 21.844,38.921 21.85,87.032c0.016,48.112 -21.801,152.524 -21.801,152.524c0,-0 -21.855,-104.26 -21.86,-152.371Z",
        styleAttr := "fill:#ff8400;stroke:#000;stroke-width:4.17px;",
      ),
      path(
        d         := "M301.94,530.635c-0.006,-24.067 4.885,-43.603 10.911,-43.625c6.031,-0 10.916,19.471 10.922,43.516c0.005,24.066 -10.9,65.404 -10.9,65.404c-0,0 -10.917,-41.25 -10.933,-65.295Z",
        styleAttr := "fill:#ffdb3d;stroke:#000;stroke-width:4.17px;",
      ),
    )

    val flame3 = g(
      idAttr := "flame_3",
      path(
        d         := "M279.127,578.17c-0.025,-74.365 15.07,-134.664 33.698,-134.698c18.629,-0.067 33.749,60.131 33.757,134.463c0.026,74.331 -33.681,235.646 -33.681,235.646c0,-0 -33.765,-161.079 -33.774,-235.411Z",
        styleAttr := "fill:#ff8400;stroke:#000;stroke-width:4.17px;",
      ),
      path(
        d         := "M295.985,578.137c-0.009,-37.183 7.547,-67.366 16.857,-67.4c9.319,0 16.866,30.083 16.875,67.231c0.008,37.183 -16.841,101.049 -16.841,101.049c-0,0 -16.866,-63.732 -16.891,-100.88Z",
        styleAttr := "fill:#ffdb3d;stroke:#000;stroke-width:4.17px;",
      ),
    )

    val flame4 = g(
      idAttr := "flame_4",
      path(
        d         := "M267.115,626.154c-0.034,-100.859 20.439,-182.641 45.704,-182.687c25.265,-0.091 45.772,81.554 45.784,182.367c0.034,100.813 -45.681,319.599 -45.681,319.599c-0,0 -45.795,-218.466 -45.807,-319.279Z",
        styleAttr := "fill:#ff8400;stroke:#000;stroke-width:4.17px;",
      ),
      path(
        d         := "M289.979,626.108c-0.012,-50.429 10.236,-91.366 22.863,-91.412c12.638,0 22.875,40.8 22.886,91.184c0.011,50.429 -22.84,137.049 -22.84,137.049c-0,0 -22.875,-86.437 -22.909,-136.821Z",
        styleAttr := "fill:#ffdb3d;stroke:#000;stroke-width:4.17px;",
      ),
    )

  }
}
