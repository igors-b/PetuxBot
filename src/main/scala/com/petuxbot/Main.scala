package com.petuxbot

import canoe.api._
import canoe.syntax._
import cats.effect.{ConcurrentEffect, ExitCode, IO, IOApp, Sync}
import fs2.Stream
import com.petuxbot.BotToken.token
import canoe.models.Chat
import cats.Applicative
import com.petuxbot.domain.{GameService, GameState, Hand, Player, Score, Trick}
import cats.syntax.all._


object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    Stream
      .resource(TelegramClient.global[IO](token))
      .flatMap { implicit client => Bot.polling[IO].follow(greetings) }
      .compile.drain.as(ExitCode.Success)

  def greetings[F[_] : TelegramClient : Sync]: Scenario[F, Unit] =
    for {
      chat <- Scenario.expect(command("hello").chat)
      detailedChat <- Scenario.eval(chat.details)
      userFirstName = detailedChat.firstName.getOrElse("dear Friend")
      _ <- Scenario.eval(chat.send(s"Hello, $userFirstName! Would you like to start PETUX game?"))
      player = Player(userFirstName, Hand.empty, Score(15), Vector.empty[Trick])
      dealer = Player("Bot", Hand.empty, Score(15), Vector.empty[Trick])
      _ <- start(chat, Vector(player, dealer))
    } yield ()

  def start[F[_] : TelegramClient : Sync](chat: Chat, players: Vector[Player]): Scenario[F, Unit] =
    for {
      _          <- Scenario.eval(chat.send("Start game by typing START"))
      resp       <- Scenario.expect(text)
      cmd        <- Scenario.eval(Parser.parse(resp))
      gameState  <- Scenario.eval(GameState.make(players))
      result     <- Scenario.eval(CommandProcessor.process(gameState, cmd))
      (state, response) = result
      // resp1 from object to json string
      _          <- Scenario.eval(chat.send(resp1.toString))
//      _ <- resp.toLowerCase match {
//        case "start" => startGame(chat, players)
//        case _ => Scenario.eval(chat.send("You entered wrong respond")) >>
//          start(chat, players)
//      }
    } yield ()

  def startGame[F[_] : TelegramClient: Sync](chat: Chat, state: GameState[F]): Scenario[F, Unit] =
    for {
      _ <- Scenario.eval(chat.send("Game started"))
      gs <- Scenario.eval(GameService.empty)
      state <- Scenario.eval(gs.startGame(players))
      _ <- Scenario.eval(chat.send("Game started"))
    } yield ()
}
