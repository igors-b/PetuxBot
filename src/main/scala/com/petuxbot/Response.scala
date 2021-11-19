package com.petuxbot

sealed trait Response

object Response {
  type ErrorDescription = String
  case object OK extends Response
  case class Error(errorDescription: ErrorDescription) extends Response
}
