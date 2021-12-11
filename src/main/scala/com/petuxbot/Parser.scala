package com.petuxbot

import io.circe.parser._
import com.petuxbot.ImplicitCodecs._
import com.petuxbot.GameError._

object Parser {
  def parse(cmd: String): Either[GameError, Command] =
    decode[Command](cmd.stripMargin) match {
      case Left(_)        => Left(ParsingError)
      case Right(command) => Right(command)
    }
}
