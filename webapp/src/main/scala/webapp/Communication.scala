package webapp

import cats.effect.IO
import org.scalajs.dom
import sttp.client3.impl.cats.FetchCatsBackend
import webapp.marslander.Level

object Communication {
  val fetchBackend = FetchCatsBackend[IO]()

  val assetLocation =
    s"${dom.window.location.origin}/${if (
      dom.window.location.host == "localhost" || dom.window.location.host
        .startsWith("localhost")
    ) ""
    else "codingame/marslander/"}"

  def getLevels: IO[List[Level]] = {
    import sttp.client3._
    import sttp.client3.circe._
    import webapp.marslander.Codecs._

    val request = basicRequest.response(asJson[List[Level]]).get(uri"$assetLocation/levels.json")

    fetchBackend
      .send(request)
      .to[IO]
      .flatMap(resp => IO.fromEither(resp.body))

  }

}
