package com.petuxbot

import canoe.api._
import cats.effect.{ExitCode, IO, IOApp}
import fs2.Stream
import com.petuxbot.BotToken.Token
import com.petuxbot.services.{CreateDeck, GameService, Shuffle}

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    for {
      gameService <- GameService.of[IO]
      shuffle = Shuffle[IO]
      createDeck = CreateDeck(shuffle)
      res <- Stream
      .resource(TelegramClient.global[IO](Token))
      .flatMap { implicit client =>
        val botActions = BotActions[IO](gameService, createDeck)
        Bot.polling[IO].follow(botActions.greetings) }
        .compile.drain.as(ExitCode.Success)
    } yield res

  }
}
