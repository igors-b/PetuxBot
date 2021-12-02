package com.petuxbot

import canoe.api._
import canoe.syntax._
import canoe.models.Chat
import canoe.syntax.{command, text}
import com.petuxbot.Command.{StartGame, _}
import com.petuxbot.Response._
import com.petuxbot.Request._
import com.petuxbot.ImplicitCodecs._
import com.petuxbot.Request.AddPlayers
import com.petuxbot.domain.cardContainers._
import com.petuxbot.domain.{Player, Score}
import com.petuxbot.services.{CreateDeck, GameService}
import io.circe.syntax._

object BotActions {
  def greetings[F[_]: TelegramClient](gameService: GameService[F], createDeck: CreateDeck[F]): Scenario[F, Unit] =
    for {
      chat          <- Scenario.expect(command("hello").chat)
      detailedChat  <- Scenario.eval(chat.details)
      id            = detailedChat.id
      userFirstName = detailedChat.firstName.getOrElse("dear Friend")
      _             <- Scenario.eval(chat.send(s"Hello, $userFirstName! Would you like to start PETUX game?"))
      player        = Player(id, userFirstName, Hand.Empty, Score(15), List.empty[Trick])
      dealer        = Player(0, "Bot", Hand.Empty, Score(15), List.empty[Trick])
      _             <- Scenario.eval(gameService.process(AddPlayers(List(player, dealer))))
      _             <- start(chat, gameService, createDeck)
    } yield ()

  def start[F[_]: TelegramClient](
    chat: Chat,
    gameService: GameService[F],
    createDeck: CreateDeck[F]
  ): Scenario[F, Unit] =
    for {
      _            <- Scenario.eval(chat.send("Start game by typing StartCommand"))
      detailedChat <- Scenario.eval(chat.details)
      playerId     =  detailedChat.id
      resp         <- Scenario.expect(text)
      cmd          = Parser.parse(resp)
      deck         <- Scenario.eval(createDeck.apply())
      response <- cmd match {
        case StartGame => Scenario.eval(gameService.process(StartRound(playerId, deck)))
        case _         => ??? //Scenario.eval(gameService.process(WrongCommand))
      }
      _ <- response match {

        case ShowCardsToPlayer(cards, trumpCard) =>
          Scenario.eval(chat.send(s"Game started, your cards:")) >>
            Scenario.eval(chat.send(cards.asJson.spaces2)) >>
            Scenario.eval(chat.send("Trump card is:")) >>
            Scenario.eval(chat.send(trumpCard.asJson.spaces2)) >>
            changeCards(chat, gameService)

        case Error(_) =>
          Scenario.eval(chat.send(response.asJson.spaces2)) >>
            start(chat, gameService, createDeck)
        case _ => Scenario.eval(chat.send("Error appeared"))
      }
    } yield ()

  def changeCards[F[_]: TelegramClient](chat: Chat, gameService: GameService[F]): Scenario[F, Unit] = {
    for {
      _ <- Scenario.eval(chat.send(s"Would you like to change some cards?"))
      detailedChat <- Scenario.eval(chat.details)
      playerId     =  detailedChat.id
      resp         <- Scenario.expect(text)
      cmd          = Parser.parse(resp)
      response <- cmd match {
        case ChangeCards(cards) => Scenario.eval(gameService.process(ChangeCardsForPlayer(playerId, cards)))
        case _                  => ??? //Scenario.eval(gameService.process(WrongCommand))
      }
      _ <- response match {

        case ShowCardsToPlayer(cards, trumpCard) =>
          Scenario.eval(chat.send(s"Your cards:")) >>
            Scenario.eval(chat.send(cards.asJson.spaces2)) >>
            Scenario.eval(chat.send("Trump card is:")) >>
            Scenario.eval(chat.send(trumpCard.asJson.spaces2)) >>
            defineWhoseTurn(chat, gameService)

        case Error(_) =>
          Scenario.eval(chat.send(response.asJson.spaces2)) >>
            changeCards(chat, gameService)
        case _ => Scenario.eval(chat.send("Error appeared"))
      }
    } yield ()
  }

  def defineWhoseTurn[F[_]: TelegramClient](chat: Chat, gameService: GameService[F]): Scenario[F, Unit] = {
    for {
      _ <- Scenario.eval(chat.send(s"Requesting playerID whose turn"))
      detailedChat <- Scenario.eval(chat.details)
      id     =  detailedChat.id
      response <- Scenario.eval(gameService.process(GetPlayerIdWhoseTurn))
      _ <- response match {
        case WhoseTurn(playerId) =>
          if (playerId == id) playerMakesTurn(chat, gameService)
          else botMakesTurn(chat, gameService)
        case Error(_) => ???
        case _ => Scenario.eval(chat.send("Error appeared"))
      }
    } yield ()
  }

  def playerMakesTurn[F[_]: TelegramClient](chat: Chat, gameService: GameService[F]): Scenario[F, Unit] = {
    for {
      _ <- Scenario.eval(chat.send("Now it is your turn. Put card on the Board"))
    } yield ()

  }

  def botMakesTurn[F[_]: TelegramClient](chat: Chat, gameService: GameService[F]): Scenario[F, Unit] = {
    ???
  }


}
