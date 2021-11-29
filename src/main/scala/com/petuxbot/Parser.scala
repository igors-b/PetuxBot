package com.petuxbot

import com.petuxbot.Command._
import io.circe.parser._
import io.circe.syntax._
import com.petuxbot.ImplicitCodecs._

object Parser {

  def parse(cmd: String): Command =

    decode[Command](cmd) match {
      case Left(_)        => WrongCommand
      case Right(command) => command
    }

//  def main(args: Array[String]): Unit = {
//    val cmd: Command = StartGame
//    println(cmd.asJson.spaces2)
//  }
}
