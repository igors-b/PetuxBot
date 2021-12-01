package com.petuxbot

import canoe.api._
import cats.effect.{ExitCode, IO, IOApp}
import fs2.Stream
import com.petuxbot.BotToken.Token
import com.petuxbot.BotActions.greetings
import com.petuxbot.services.{CreateDeck, GameService, Shuffle}

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    for {
      gameService <- GameService.of[IO]
      shuffle = Shuffle[IO]
      createDeck = CreateDeck(shuffle)
      res <- Stream
      .resource(TelegramClient.global[IO](Token))
      .flatMap { implicit client => Bot.polling[IO].follow(greetings(gameService, createDeck)) }
        .compile.drain.as(ExitCode.Success)
    } yield res

  }
}
