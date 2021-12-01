package com.petuxbot

import canoe.api._
import canoe.syntax._
import canoe.models.Chat
import canoe.syntax.{command, text}
import com.petuxbot.Command._
import com.petuxbot.Response._
import com.petuxbot.ImplicitCodecs._
import com.petuxbot.domain.cardContainers._
import com.petuxbot.domain.{Player, Score}
import com.petuxbot.services.{CreateDeck, GameService}
import io.circe.syntax._

object BotActions {
  def greetings[F[_]: TelegramClient](gameService: GameService[F], createDeck: CreateDeck[F]): Scenario[F, Unit] =
    for {
      chat          <- Scenario.expect(command("hello").chat)
      detailedChat  <- Scenario.eval(chat.details)
      userFirstName = detailedChat.firstName.getOrElse("dear Friend")
      _             <- Scenario.eval(chat.send(s"Hello, $userFirstName! Would you like to start PETUX game?"))
      player        = Player(userFirstName, Hand.Empty, Score(15), List.empty[Trick])
      dealer        = Player("Bot", Hand.Empty, Score(15), List.empty[Trick])
      _             <- Scenario.eval(gameService.process(AddPlayers(List(player, dealer))))
      _             <- start(chat, gameService, createDeck)
    } yield ()

  def start[F[_]: TelegramClient](chat: Chat, gameService: GameService[F], createDeck: CreateDeck[F]): Scenario[F, Unit] =
    for {
      _        <- Scenario.eval(chat.send("Start game by typing StartCommand"))
      resp     <- Scenario.expect(text)
      cmd      =  Parser.parse(resp)
      deck     <- Scenario.eval(createDeck.apply())
      response <- cmd match {
                    case StartGame => Scenario.eval(gameService.process(StartGame(deck)))
                    case _         => Scenario.eval(gameService.process(WrongCommand))
                  }
      _ <- response match {

        case ShowCardsToPlayer(cards, trumpCard) => {
          Scenario.eval(chat.send(s"Game started, your cards:")) >>
            Scenario.eval(chat.send(cards.asJson.spaces2)) >>
            Scenario.eval(chat.send("Trump card is:")) >>
            Scenario.eval(chat.send(trumpCard.asJson.spaces2)) >>
            startGame(chat, gameService)
        }
        case Error(_) =>
          Scenario.eval(chat.send(response.asJson.spaces2)) >>
            start(chat, gameService, createDeck)
        case _        => Scenario.eval(chat.send("Error appeared".asJson.spaces2))
      }
    } yield ()

  def startGame[F[_]: TelegramClient](chat: Chat, gameService: GameService[F]): Scenario[F, Unit] = {
    for {
      _ <- Scenario.eval(chat.send(s"Next stage"))

    } yield ()
  }
}
