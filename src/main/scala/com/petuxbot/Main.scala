package com.petuxbot

import canoe.api._
import cats.effect.{ExitCode, IO, IOApp}
import fs2.Stream
import com.petuxbot.BotToken.token
import com.petuxbot.BotActions.greetings


object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    Stream
      .resource(TelegramClient.global[IO](token))
      .flatMap { implicit client => Bot.polling[IO].follow(greetings) }
      .compile.drain.as(ExitCode.Success)
}
