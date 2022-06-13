package webapp

import cats.effect.IO
import org.scalajs.dom
import outwatch.dsl.s
import sttp.client3.impl.cats.FetchCatsBackend
import webapp.marslander.SurfaceModel

object Communication {
  val fetchBackend = FetchCatsBackend[IO]()

  private val assetLocation = IO {
    s"${dom.window.location.origin}/${if (
      dom.window.location.host == "localhost" || dom.window.location.host
        .startsWith("localhost")
    ) ""
    else "codingame/marslander/"}"
  }

  def getLevels: IO[List[SurfaceModel]] = {
    import sttp.client3._
    import sttp.client3.circe._
    import webapp.marslander.Codecs._

    assetLocation.flatMap { assetsLoc =>
      val request = basicRequest.response(asJson[List[SurfaceModel]]).get(uri"${assetsLoc}/levels.json")

      fetchBackend
        .send(request)
        .to[IO]
        .flatMap(resp => IO.fromEither(resp.body))
    }

  }

}
