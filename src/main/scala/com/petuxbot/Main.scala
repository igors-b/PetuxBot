package com.petuxbot

import canoe.api._
import cats.effect.{ExitCode, IO, IOApp}
import fs2.Stream
import com.petuxbot.BotData.Token
import com.petuxbot.services._

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val shuffler =  ShuffleService[IO]
    val deckService  =  DeckService(shuffler)
    for {
//      gameService  <- GameService.of[IO]
//      shuffler     =  ShuffleService[IO]
//      deckService  =  DeckService(shuffler)
      result       <- Stream
        .resource(TelegramClient.global[IO](Token))
        .flatMap { implicit client =>
          val botService = BotService[IO](/*gameService, */deckService)
          Bot.polling[IO].follow(botService.greetings)
        }
        .compile
        .drain
        .as(ExitCode.Success)
    } yield result
  }
}
