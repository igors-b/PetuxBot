package com.petuxbot
import io.circe.generic.JsonCodec
sealed trait Response

object Response {
  type ErrorDescription = String
  case object OK extends Response
  case class Error(errorDescription: String) extends Response
}
