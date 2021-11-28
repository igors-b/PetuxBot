package com.petuxbot

import canoe.api._
import canoe.syntax._
import canoe.models.Chat
import canoe.syntax.{command, text}
import cats.effect.Sync
import com.petuxbot.Response.{Error, OK}
import com.petuxbot.ImplicitCodecs._
import com.petuxbot.domain.{GameState, Hand, Player, Score, Trick}
import io.circe.syntax._


object BotActions {
  def greetings[F[_] : TelegramClient : Sync]: Scenario[F, Unit] =
    for {
      chat <- Scenario.expect(command("hello").chat)
      detailedChat <- Scenario.eval(chat.details)
      userFirstName = detailedChat.firstName.getOrElse("dear Friend")
      _ <- Scenario.eval(chat.send(s"Hello, $userFirstName! Would you like to start PETUX game?"))
      player = Player(userFirstName, Hand.Empty, Score(15), Vector.empty[Trick])
      dealer = Player("Bot", Hand.Empty, Score(15), Vector.empty[Trick])
      _ <- start(chat, List(player, dealer))
    } yield ()

  def start[F[_] : TelegramClient: Sync](chat: Chat, players: List[Player]): Scenario[F, Unit] =
    for {
      _          <- Scenario.eval(chat.send("Start game by typing START"))
      resp       <- Scenario.expect(text)
      cmd        <- Scenario.eval(Parser.parse(resp))
      gameState  <- Scenario.eval(GameState.make(players))
      result     <- Scenario.eval(CommandProcessor.process(gameState, cmd))
      (state, response) = result
      _ <- response match {
        case OK => Scenario.eval(chat.send(response.asJson.spaces2)) >> startGame(chat, state)
        case Error(_) => Scenario.eval(chat.send(response.asJson.spaces2)) >>
          start(chat, players)
      }
    } yield ()

  def startGame[F[_] : TelegramClient](chat: Chat, state: GameState): Scenario[F, Unit] = {
    //val cards = state.players.head.hand.cards.map(_.toString)
    val cards = state.players.head.hand.cards.asJson.spaces2
    val trumpCard = state.deck.trumpCard.asJson.spaces2
    for {
      _ <- Scenario.eval(chat.send(s"Game started, your cards:"))
      _ <- Scenario.eval(chat.send(cards))
      _ <- Scenario.eval(chat.send("Trump card is:"))
      _ <- Scenario.eval(chat.send(trumpCard))
    } yield ()
  }
}
