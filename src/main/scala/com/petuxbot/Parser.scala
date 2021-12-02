package com.petuxbot

import com.petuxbot.Command._
import io.circe.parser._
import com.petuxbot.ImplicitCodecs._

object Parser {

  def parse(cmd: String): Command =

    decode[Command](cmd.stripMargin) match {
      case Left(_)        => WrongCommand
      case Right(command) => command
    }
}
