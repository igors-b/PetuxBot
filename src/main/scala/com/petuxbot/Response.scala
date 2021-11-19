package com.petuxbot

sealed trait Response


object Response {
  case object OK extends Response
}
