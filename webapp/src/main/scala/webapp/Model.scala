package webapp

import io.circe.{Decoder, Encoder}

object Model {

  case class MouseControlState(angle: Int, thrust: Int)

  object UISettings {
    val default: UISettings = UISettings(showRadar = false, showHighScorePath = true)

    implicit val landerInitialStateDecoder: Decoder[UISettings] = io.circe.generic.semiauto.deriveDecoder
    implicit val landerInitialStateEncoder: Encoder[UISettings] = io.circe.generic.semiauto.deriveEncoder

  }

  case class UISettings(showRadar: Boolean, showHighScorePath: Boolean)


}
