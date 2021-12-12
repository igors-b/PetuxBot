package com.petuxbot

import io.circe
import io.circe.parser._
import com.petuxbot.Codecs._

object Parser {
  def parse(request: String): Either[circe.Error, Request] =
    decode[Request](request.stripMargin)
}
