package webapp

import cats.effect.IO
import sttp.client3.impl.cats.FetchCatsBackend
import webapp.marslander.SurfaceModel

object Communication {
  val fetchBackend = FetchCatsBackend[IO]()

  def getLevels: IO[List[SurfaceModel]] = {
    import sttp.client3._
    import sttp.client3.circe._
    import webapp.marslander.Codecs._
    val request = basicRequest.response(asJson[List[SurfaceModel]]).get(uri"/levels.json")

    fetchBackend
      .send(request)
      .to[IO]
      .flatMap(resp => IO.fromEither(resp.body))

  }

}
