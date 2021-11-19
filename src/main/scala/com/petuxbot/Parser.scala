package com.petuxbot

import cats.effect.Sync
import com.petuxbot.Command.{StartGame, WrongCommand}

object Parser {

  def parse[F[_]: Sync](cmd: String): F[Command] = {
    Sync[F].delay(
    cmd match {
      case "start" => StartGame
      case _       => WrongCommand
    }
    )
  }
}
