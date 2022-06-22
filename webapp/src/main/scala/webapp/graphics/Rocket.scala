package webapp.graphics
import outwatch._
import outwatch.dsl._
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

  val rocketWidth  = 634
  val rocketHeight = 609
  val flameWidth   = 50
  val flameHeight  = 146

  val flameOriginYRatio = 0.28

  def rocketWithFlame(thrust: Int): SvgVNode =
    svg(
      Assets.rocket(
        width  := rocketWidth.toString,
        height := rocketHeight.toString,
      ),
      g(
        Assets.flame(
          width  := flameWidth.toString,
          height := flameHeight.toString,
          x      := (rocketWidth * 0.5 - flameWidth * 0.5).toString,
          y      := (rocketHeight * (1 - flameOriginYRatio)).toString,
        ),
      ),
    )

  object Assets {

    val rocket = svg(
      viewBox   := "0 0 634 609",
      styleAttr := "fill-rule:evenodd;clip-rule:evenodd;stroke-linejoin:round;stroke-miterlimit:2;",
      g(
        idAttr := "rocket",
        path(
          idAttr                     := "rocket1",
          VModifier.attr("serif:id") := "rocket",
          d                          := "M478.982,116.084l-111.565,-113.308l-109.822,0c2.614,1.743 -113.767,117.52 -113.791,119.043c-0.851,52.702 -0.259,105.418 -0.388,158.125c3.051,-0.87 69.649,70.751 79.316,78.446c-6.828,5.664 -17.078,8.803 -20.484,16.995c-5.971,14.365 -7.068,52.615 -7.068,52.615c0,0 -93.204,46.127 -101.445,45.44l-0,95.876c-25.494,0.654 -51.034,0.299 -76.484,1.961c-12.682,0.828 -13.556,29.99 -8.061,30.289c65.854,3.57 131.943,1.51 197.852,-0.872c18.229,-0.66 2.196,-29.896 -6.973,-31.378c-25.563,-4.133 -51.789,-0.483 -77.685,-0.723c-1.051,-18.564 5.035,-79.212 5.035,-79.212c-0,0 86.596,-44.705 84.852,-47.318c55.783,-0.437 111.564,-1.215 167.348,-1.307c9.647,-0.018 19.984,-2.641 28.927,0.979c34.446,13.943 99.198,50.881 99.198,50.881c0,-0 0.58,51.133 0.872,76.7c-1.744,-0 -55.981,-1.905 -81.876,1.961c-5.82,0.868 -21.361,27.005 -11.331,28.055c33.353,3.492 141.686,3.231 211.448,-7.102c0.057,-8.105 3.01,-18.298 -10.598,-19.973c-28.133,-3.459 -56.655,-1.961 -84.982,-2.941l0,-95.876l-97.657,-47.335c-0,-0 -3.971,-35.836 -12.617,-53.875c-2.969,-6.195 -12.482,-5.741 -18.723,-8.611c4.121,-5.656 77.363,-79.604 77.363,-79.604l-0.661,-167.931Zm-73.214,306.803l-1.995,-28.141l-174.07,-2.147l0.502,29.112l175.563,1.176Zm-142.943,-388.733l-85.755,91.722l0,149.506c0,-0 85.755,87.365 85.755,91.723l99.362,-0l86.793,-89.278l1.726,-154.065l-85.759,-88.094l-102.122,-1.514Z",
          styleAttr                  := "fill:#b3b3b3;stroke:#000;stroke-width:4.17px;",
        ),
      ),
    )

    val flame = svg(
      viewBox   := "0 0 50 146",
      styleAttr := "fill-rule:evenodd;clip-rule:evenodd;stroke-linejoin:round;stroke-miterlimit:2;",
      g(
        idAttr := "flame_1",
        path(
          d         := "M3.529,55.475c-0.017,-27.732 9.754,-50.219 21.811,-50.231c12.057,-0.026 21.844,22.423 21.85,50.143c0.016,27.719 -21.801,87.876 -21.801,87.876c0,-0 -21.855,-60.069 -21.86,-87.788Z",
          styleAttr := "fill:#ff8400;stroke:#000;stroke-width:4.17px;",
        ),
        path(
          d         := "M14.44,55.462c-0.006,-13.866 4.885,-25.122 10.911,-25.134c6.031,-0 10.916,11.218 10.922,25.071c0.005,13.866 -10.9,37.683 -10.9,37.683c-0,-0 -10.917,-23.767 -10.933,-37.62Z",
          styleAttr := "fill:#ffdb3d;stroke:#000;stroke-width:4.17px;",
        ),
      ),
    )

  }
}
