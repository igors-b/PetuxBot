package com.petuxbot

import cats.effect.Sync
import com.petuxbot.Command.{StartGame, WrongCommand}

object Parser {

  def parse(cmd: String): Command = {
    cmd match {
      case "start" => StartGame
      case _       => WrongCommand
    }

  }
}
