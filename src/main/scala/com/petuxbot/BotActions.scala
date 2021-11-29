package com.petuxbot

import canoe.api._
import canoe.syntax._
import canoe.models.Chat
import canoe.syntax.{command, text}
import com.petuxbot.Command._
import com.petuxbot.Response.{Error, OK, ShowCardsToPlayer}
import com.petuxbot.ImplicitCodecs._
import com.petuxbot.domain.{GameService, GameState, Hand, Player, Score, Trick}
import io.circe.syntax._


object BotActions {
  def greetings[F[_] : TelegramClient](gameService: GameService[F]): Scenario[F, Unit] =
    for {
      chat          <-  Scenario.expect(command("hello").chat)
      detailedChat  <-  Scenario.eval(chat.details)
      userFirstName =   detailedChat.firstName.getOrElse("dear Friend")
      _             <-  Scenario.eval(chat.send(s"Hello, $userFirstName! Would you like to start PETUX game?"))
      player        =   Player(userFirstName, Hand.Empty, Score(15), List.empty[Trick])
      dealer        =   Player("Bot", Hand.Empty, Score(15), List.empty[Trick])
      _             <-  Scenario.eval(gameService.process(ResetState))
      _             <-  Scenario.eval(gameService.process(AddPlayer(player)))
      _             <-  Scenario.eval(gameService.process(AddPlayer(dealer)))
      _             <-  start(chat, gameService)
    } yield ()

  def start[F[_] : TelegramClient](chat: Chat, gameService: GameService[F]): Scenario[F, Unit] =
    for {
      _          <- Scenario.eval(chat.send("Start game by typing START"))
      resp       <- Scenario.expect(text)
      cmd        = Parser.parse(resp)
      response   <- Scenario.eval(gameService.process(cmd))
      _          <- response match {
        case ShowCardsToPlayer(cards, trumpCard) => {
          Scenario.eval(chat.send(s"Game started, your cards:")) >>
          Scenario.eval(chat.send(cards.asJson.spaces2)) >>
          Scenario.eval(chat.send("Trump card is:")) >>
          Scenario.eval(chat.send(trumpCard.asJson.spaces2)) >>
          startGame(chat, gameService)
        }

        case Error(_) => Scenario.eval(chat.send(response.asJson.spaces2)) >>
          start(chat, gameService)
      }
    } yield ()

  def startGame[F[_] : TelegramClient](chat: Chat, gameService: GameService[F]): Scenario[F, Unit] = {
    for {
      _ <- Scenario.eval(chat.send(s"Next stage"))

    } yield ()
  }
}
